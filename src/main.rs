use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::process::{Command, ExitStatus, Stdio};

mod expand;
mod parser;
mod state;
mod workers;

use crate::expand::{expand_tokens, is_var_char, is_var_start};
use crate::parser::{
    append_pipeline_tail, collect_brace_block, parse_args, parse_brace_block, parse_foreach_line,
    split_indent, split_on_and_or, split_on_pipes, split_on_semicolons, unindent_block_lines,
    BraceParse, LogicOp,
};
use crate::state::{write_locals_file, ShellState};
use crate::workers::{run_capture_worker, run_foreach_worker, write_block_file};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 && args[1] == "--foreach-worker" {
        if let Err(err) = run_foreach_worker(&args[2..]) {
            eprintln!("unshell: {err}");
            std::process::exit(1);
        }
        return;
    }
    if args.len() > 1 && args[1] == "--capture-worker" {
        if let Err(err) = run_capture_worker(&args[2..]) {
            eprintln!("unshell: {err}");
            std::process::exit(1);
        }
        return;
    }

    if args.len() > 1 {
        if let Err(err) = run_script(&args[1]) {
            eprintln!("unshell: failed to run script '{}': {err}", args[1]);
            std::process::exit(1);
        }
        return;
    }

    run_repl();
}

fn run_repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut state = ShellState::new();

    loop {
        if print_prompt(&mut stdout).is_err() {
            break;
        }

        let mut line = String::new();
        match stdin.read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {
                if !process_line(&line, &mut state) {
                    break;
                }
            }
            Err(err) => {
                eprintln!("unshell: failed to read line: {err}");
                break;
            }
        }
    }
}

fn run_script(path: &str) -> io::Result<()> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().collect::<Result<_, _>>()?;

    let mut state = ShellState::new();
    let mut ctx = ScriptContext {
        lines,
        state: &mut state,
    };
    ctx.execute()
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;

    Ok(())
}


fn process_line(line: &str, state: &mut ShellState) -> bool {
    let mut ctx = ScriptContext {
        lines: vec![line.to_string()],
        state,
    };
    match ctx.execute_with_exit() {
        Ok(exit) => !exit,
        Err(err) => {
            eprintln!("unshell: {err}");
            true
        }
    }
}

fn process_line_raw(line: &str, state: &mut ShellState) -> bool {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return true;
    }

    let sequences = split_on_semicolons(trimmed);
    for sequence in sequences {
        let sequence = sequence.trim();
        if sequence.is_empty() {
            continue;
        }

        let chain = split_on_and_or(sequence);
        let mut last_success = true;
        for (op, segment) in chain {
            let segment = segment.trim();
            if segment.is_empty() {
                continue;
            }
            match op {
                LogicOp::And if !last_success => continue,
                LogicOp::Or if last_success => continue,
                _ => {}
            }
            match run_pipeline_segment(segment, state) {
                Ok(RunResult::Exit) => return false,
                Ok(RunResult::Success(success)) => last_success = success,
                Err(err) => {
                    eprintln!("unshell: {err}");
                    last_success = false;
                }
            }
        }
    }
    true
}


fn split_assignments(tokens: &[String]) -> (Vec<(String, String)>, &[String]) {
    let mut assignments = Vec::new();
    let mut idx = 0;

    while idx < tokens.len() {
        if let Some((name, value)) = parse_assignment(&tokens[idx]) {
            assignments.push((name, value));
            idx += 1;
        } else {
            break;
        }
    }

    (assignments, &tokens[idx..])
}

fn is_valid_alias_name(name: &str) -> bool {
    let mut chars = name.chars();
    let first = match chars.next() {
        Some(ch) => ch,
        None => return false,
    };

    if !(first.is_ascii_alphabetic() || first == '_') {
        return false;
    }

    for ch in chars {
        if !(ch.is_ascii_alphanumeric() || ch == '_' || ch == '-') {
            return false;
        }
    }

    true
}

fn is_quoted_token(token: &str) -> bool {
    (token.starts_with('\'') && token.ends_with('\'') && token.len() >= 2)
        || (token.starts_with('"') && token.ends_with('"') && token.len() >= 2)
}

fn apply_alias(tokens: Vec<String>, state: &ShellState) -> Result<Vec<String>, String> {
    if tokens.is_empty() {
        return Ok(tokens);
    }

    let mut current = tokens;
    let recursive = state.options.aliases_recursive;
    let mut depth = 0;
    loop {
        let mut changed = false;
        let mut next = Vec::new();

        for (idx, token) in current.iter().enumerate() {
            if is_quoted_token(token) {
                next.push(token.clone());
                continue;
            }

            if idx == 0 {
                if let Some(alias) = state.aliases.get(token) {
                    let alias_tokens = parse_args(&alias.value)?;
                    next.extend(alias_tokens);
                    changed = true;
                    continue;
                }
            }

            if let Some(alias) = state.aliases.get(token) {
                if alias.global {
                    let alias_tokens = parse_args(&alias.value)?;
                    next.extend(alias_tokens);
                    changed = true;
                    continue;
                }
            }

            next.push(token.clone());
        }

        if !changed {
            return Ok(current);
        }

        if !recursive {
            return Ok(next);
        }

        depth += 1;
        if depth > 32 {
            return Err("alias expansion exceeded recursion limit".into());
        }

        current = next;
    }
}

fn parse_assignment(token: &str) -> Option<(String, String)> {
    let mut parts = token.splitn(2, '=');
    let name = parts.next()?;
    let value = parts.next()?;

    if !is_valid_var_name(name) {
        return None;
    }

    Some((name.to_string(), value.to_string()))
}

fn is_valid_var_name(name: &str) -> bool {
    let mut chars = name.chars();
    let first = match chars.next() {
        Some(ch) => ch,
        None => return false,
    };

    if !is_var_start(first) {
        return false;
    }

    for ch in chars {
        if !is_var_char(ch) {
            return false;
        }
    }

    true
}

enum RunResult {
    Success(bool),
    Exit,
}

fn run_pipeline_segment(segment: &str, state: &mut ShellState) -> Result<RunResult, String> {
    let commands = split_on_pipes(segment);
    if commands.is_empty() {
        return Ok(RunResult::Success(true));
    }

    if commands.len() == 1 {
        let args = parse_args(commands[0].trim())?;
        let args = apply_alias(args, state)?;
        if args.get(0).map(String::as_str) == Some("alias") {
            if let Some(result) = run_alias_builtin_unexpanded(&args, state)? {
                return Ok(result);
            }
        }
        let expanded = expand_tokens(args, state)?;
        let (assignments, remaining) = split_assignments(&expanded);
        for (name, value) in assignments {
            state.set_var(&name, value);
        }
        if remaining.is_empty() {
            return Ok(RunResult::Success(true));
        }
        if remaining.len() == 1 && remaining[0] == "exit" {
            return Ok(RunResult::Exit);
        }
        if let Some(result) = run_builtin(&remaining, state)? {
            return Ok(result);
        }
        let success = execute_with_status(remaining);
        return Ok(RunResult::Success(success));
    }

    let expanded_commands = build_pipeline_commands(&commands, state)?;

    run_pipeline(expanded_commands).map(RunResult::Success)
}

fn run_builtin(args: &[String], state: &mut ShellState) -> Result<Option<RunResult>, String> {
    let name = args.first().map(String::as_str).unwrap_or("");
    match name {
        "cd" => {
            if args.len() > 2 {
                return Err("cd: too many arguments".into());
            }
            let target = if args.len() == 1 {
                env::var("HOME").map_err(|_| "cd: HOME not set".to_string())?
            } else {
                args[1].clone()
            };
            env::set_current_dir(&target).map_err(|err| {
                format!("cd: failed to change directory to '{target}': {err}")
            })?;
            let cwd = env::current_dir()
                .map_err(|err| format!("cd: failed to read current directory: {err}"))?;
            let cwd_str = cwd.to_string_lossy().to_string();
            state.set_var("PWD", cwd_str.clone());
            unsafe {
                env::set_var("PWD", cwd_str);
            }
            Ok(Some(RunResult::Success(true)))
        }
        "export" => {
            if args.len() == 1 {
                return Err("export: missing arguments".into());
            }
            for item in &args[1..] {
                if let Some((name, value)) = parse_assignment(item) {
                    state.set_var(&name, value.clone());
                    unsafe {
                        env::set_var(&name, value);
                    }
                    continue;
                }
                if !is_valid_var_name(item) {
                    return Err(format!("export: invalid name '{item}'"));
                }
                let value = state
                    .vars
                    .get(item)
                    .cloned()
                    .or_else(|| env::var(item).ok())
                    .unwrap_or_default();
                state.set_var(item, value.clone());
                unsafe {
                    env::set_var(item, value);
                }
            }
            Ok(Some(RunResult::Success(true)))
        }
        "alias" => Ok(None),
        "unalias" => {
            if args.len() != 2 {
                return Err("unalias: expected exactly one name".into());
            }
            let alias_name = &args[1];
            if !state.remove_alias(alias_name) {
                return Err(format!("unalias: no such alias '{alias_name}'"));
            }
            Ok(Some(RunResult::Success(true)))
        }
        "set" => {
            if args.len() < 3 {
                return Err("set: expected KEY VALUE".into());
            }
            let key = &args[1];
            match key.as_str() {
                "aliases.recursive" => {
                    if args.len() != 3 {
                        return Err("set: aliases.recursive expects a value".into());
                    }
                    let value = &args[2];
                    let enabled = match value.as_str() {
                        "true" => true,
                        "false" => false,
                        _ => {
                            return Err(
                                "set: aliases.recursive expects 'true' or 'false'".into(),
                            )
                        }
                    };
                    state.options.aliases_recursive = enabled;
                    Ok(Some(RunResult::Success(true)))
                }
                "subshells.trim_newline" => {
                    if args.len() != 3 {
                        return Err("set: subshells.trim_newline expects a value".into());
                    }
                    let value = &args[2];
                    let enabled = match value.as_str() {
                        "true" => true,
                        "false" => false,
                        _ => {
                            return Err(
                                "set: subshells.trim_newline expects 'true' or 'false'".into(),
                            )
                        }
                    };
                    state.options.subshells_trim_newline = enabled;
                    Ok(Some(RunResult::Success(true)))
                }
                "expansions.characters" => {
                    if args.len() != 4 {
                        return Err("set: expansions.characters expects CHARS on|off".into());
                    }
                    let chars = &args[2];
                    let enabled = match args[3].as_str() {
                        "on" => true,
                        "off" => false,
                        _ => {
                            return Err(
                                "set: expansions.characters expects 'on' or 'off'".into(),
                            )
                        }
                    };
                    for ch in chars.chars() {
                        if enabled {
                            state.options.expansions_chars.insert(ch);
                        } else {
                            state.options.expansions_chars.remove(&ch);
                        }
                    }
                    Ok(Some(RunResult::Success(true)))
                }
                "expansions.handler" => {
                    if args.len() < 3 {
                        return Err("set: expansions.handler expects a command".into());
                    }
                    state.options.expansions_handler = args[2..].to_vec();
                    Ok(Some(RunResult::Success(true)))
                }
                _ => Err(format!("set: unknown option '{key}'")),
            }
        }
        "eval" => {
            if args.len() != 2 {
                return Err("eval: expected a single argument".into());
            }
            let mut ctx = ScriptContext {
                lines: vec![args[1].clone()],
                state,
            };
            if ctx.execute_with_exit()? {
                return Ok(Some(RunResult::Exit));
            }
            Ok(Some(RunResult::Success(true)))
        }
        _ => Ok(None),
    }
}

fn run_alias_builtin_unexpanded(
    args: &[String],
    state: &mut ShellState,
) -> Result<Option<RunResult>, String> {
    if args.len() == 1 {
        for (name, value) in state.aliases.iter() {
            if value.global {
                println!("alias -g {name} {}", value.value);
            } else {
                println!("alias {name} {}", value.value);
            }
        }
        return Ok(Some(RunResult::Success(true)));
    }
    let mut arg_idx = 1;
    let mut global = false;
    if args.get(arg_idx).map(String::as_str) == Some("-g") {
        global = true;
        arg_idx += 1;
    }
    if args.len() <= arg_idx + 1 {
        return Err("alias: missing value".into());
    }
    let alias_name = &args[arg_idx];
    if !is_valid_alias_name(alias_name) {
        return Err(format!("alias: invalid name '{alias_name}'"));
    }
    let raw_tokens = args[arg_idx + 1..].to_vec();
    let expanded_tokens = expand_tokens(raw_tokens, state)?;
    let value = expanded_tokens.join(" ");
    state.set_alias(alias_name, value, global);
    Ok(Some(RunResult::Success(true)))
}

fn build_pipeline_commands(
    commands: &[String],
    state: &mut ShellState,
) -> Result<Vec<Vec<String>>, String> {
    let commands_len = commands.len();
    let mut expanded_commands = Vec::new();

    for cmd in commands {
        let args = parse_args(cmd.trim())?;
        let args = apply_alias(args, state)?;
        let expanded = expand_tokens(args, state)?;
        let (assignments, remaining) = split_assignments(&expanded);
        for (name, value) in assignments {
            state.set_var(&name, value);
        }
        if remaining.is_empty() {
            if commands_len == 1 {
                return Ok(Vec::new());
            }
            return Err("empty command in pipeline".into());
        }
        expanded_commands.push(remaining.to_vec());
    }

    Ok(expanded_commands)
}

fn build_pipeline_stages_from_segments(
    segments: &[String],
    state: &mut ShellState,
) -> Result<Vec<PipelineStage>, String> {
    let mut stages = Vec::new();

    for segment in segments {
        let trimmed = segment.trim();
        if trimmed.is_empty() {
            return Err("empty command in pipeline".into());
        }

        let tokens = parse_args(trimmed)?;
        let tokens = apply_alias(tokens, state)?;
        let segment_line = tokens.join(" ");

        if segment_line.starts_with("foreach ") {
            let brace = parse_brace_block(&segment_line);
            let (head, brace_block, brace_open, brace_tail) = match brace {
                BraceParse::Inline { head, body, tail } => (head, Some(body), false, tail),
                BraceParse::Open { head, tail } => (head, None, true, Some(tail)),
                BraceParse::None { head } => (head, None, false, None),
            };
            let args = parse_args(&head)?;
            if args.len() == 2 && args[0] == "foreach" {
                if brace_open {
                    return Err(
                        "foreach blocks in pipeline tails must use inline braces".into(),
                    );
                }
                if let Some(tail) = brace_tail {
                    return Err(format!(
                        "unexpected trailing text after foreach block: {tail}"
                    ));
                }
                if let Some(block) = brace_block {
                    stages.push(PipelineStage::Foreach {
                        var: args[1].clone(),
                        block,
                        inline: true,
                    });
                    continue;
                }
                return Err("foreach block missing braces in pipeline tail".into());
            }
        }

        let expanded = expand_tokens(tokens, state)?;
        let (assignments, remaining) = split_assignments(&expanded);
        for (name, value) in assignments {
            state.set_var(&name, value);
        }
        if remaining.is_empty() {
            return Err("empty command in pipeline".into());
        }
        stages.push(PipelineStage::External(remaining.to_vec()));
    }

    Ok(stages)
}

fn run_pipeline(commands: Vec<Vec<String>>) -> Result<bool, String> {
    let mut children = Vec::new();
    let mut prev_stdout: Option<std::process::ChildStdout> = None;

    for (idx, args) in commands.iter().enumerate() {
        let mut cmd = Command::new(&args[0]);
        cmd.args(&args[1..]);

        if let Some(stdout) = prev_stdout.take() {
            cmd.stdin(Stdio::from(stdout));
        }

        if idx + 1 < commands.len() {
            cmd.stdout(Stdio::piped());
        }

        let mut child = cmd
            .spawn()
            .map_err(|err| format!("failed to execute '{}': {err}", args[0]))?;

        if idx + 1 < commands.len() {
            let stdout = child
                .stdout
                .take()
                .ok_or_else(|| "failed to capture stdout for pipeline".to_string())?;
            prev_stdout = Some(stdout);
        }

        children.push((args[0].clone(), child));
    }

    let mut last_success = true;
    for (idx, (name, mut child)) in children.into_iter().enumerate() {
        let status = child
            .wait()
            .map_err(|err| format!("failed to wait for '{}': {err}", name))?;
        if !status.success() {
            report_status(&name, status);
        }
        if idx + 1 == commands.len() {
            last_success = status.success();
        }
    }

    Ok(last_success)
}

enum PipelineStage {
    External(Vec<String>),
    Foreach {
        var: String,
        block: String,
        inline: bool,
    },
}

fn run_pipeline_stages(stages: Vec<PipelineStage>, state: &ShellState) -> Result<bool, String> {
    if stages.is_empty() {
        return Ok(true);
    }

    let foreach_needed = stages
        .iter()
        .any(|stage| matches!(stage, PipelineStage::Foreach { .. }));
    let (locals_path, locals_guard) = if foreach_needed {
        let (path, guard) =
            write_locals_file(state).map_err(|err| format!("failed to write locals: {err}"))?;
        (Some(path), Some(guard))
    } else {
        (None, None)
    };

    let mut block_guards = Vec::new();
    let mut children = Vec::new();
    let mut prev_stdout: Option<std::process::ChildStdout> = None;

    for (idx, stage) in stages.iter().enumerate() {
        let mut cmd = match stage {
            PipelineStage::External(args) => {
                let mut cmd = Command::new(&args[0]);
                cmd.args(&args[1..]);
                cmd
            }
            PipelineStage::Foreach { var, block, inline } => {
                let exe = env::current_exe()
                    .map_err(|err| format!("failed to resolve ush path: {err}"))?;
                let locals = locals_path
                    .as_ref()
                    .ok_or_else(|| "missing locals path for foreach".to_string())?;
                let (block_path, guard) = write_block_file(block)
                    .map_err(|err| format!("failed to write foreach block: {err}"))?;
                block_guards.push(guard);
                let mut cmd = Command::new(exe);
                cmd.arg("--foreach-worker")
                    .arg("--var")
                    .arg(var)
                    .arg("--block")
                    .arg(block_path)
                    .arg("--locals")
                    .arg(locals);
                if *inline {
                    cmd.arg("--inline");
                }
                cmd
            }
        };

        if let Some(stdout) = prev_stdout.take() {
            cmd.stdin(Stdio::from(stdout));
        }

        if idx + 1 < stages.len() {
            cmd.stdout(Stdio::piped());
        }

        let mut child = cmd
            .spawn()
            .map_err(|err| format!("failed to execute pipeline stage: {err}"))?;

        if idx + 1 < stages.len() {
            let stdout = child
                .stdout
                .take()
                .ok_or_else(|| "failed to capture stdout for pipeline".to_string())?;
            prev_stdout = Some(stdout);
        }

        let name = match stage {
            PipelineStage::External(args) => args[0].clone(),
            PipelineStage::Foreach { .. } => "foreach".to_string(),
        };
        children.push((name, child));
    }

    let mut last_success = true;
    for (idx, (name, mut child)) in children.into_iter().enumerate() {
        let status = child
            .wait()
            .map_err(|err| format!("failed to wait for '{}': {err}", name))?;
        if !status.success() {
            report_status(&name, status);
        }
        if idx + 1 == stages.len() {
            last_success = status.success();
        }
    }

    drop(block_guards);
    drop(locals_guard);

    Ok(last_success)
}

fn expand_aliases_in_line(line: &str, state: &ShellState) -> Result<String, String> {
    let segments = split_on_pipes(line);
    if segments.len() <= 1 {
        let tokens = parse_args(line)?;
        let tokens = apply_alias(tokens, state)?;
        return Ok(tokens.join(" "));
    }

    let mut expanded_segments = Vec::new();
    for segment in segments {
        let tokens = parse_args(segment.trim())?;
        let tokens = apply_alias(tokens, state)?;
        expanded_segments.push(tokens.join(" "));
    }

    Ok(expanded_segments.join(" | "))
}

pub fn execute_inline_block(block: &str, state: &mut ShellState) -> Result<(), String> {
    let segments = split_on_semicolons(block);
    for segment in segments {
        if segment.trim().is_empty() {
            continue;
        }
        if !process_line_raw(&segment, state) {
            return Err("exit not allowed in inline block".into());
        }
    }
    Ok(())
}

pub struct ScriptContext<'a> {
    pub lines: Vec<String>,
    pub state: &'a mut ShellState,
}

struct BlockResult {
    next: usize,
    exit: bool,
}

impl<'a> ScriptContext<'a> {
    pub fn execute(&mut self) -> Result<(), String> {
        let _ = self.execute_with_exit()?;
        Ok(())
    }

    pub fn execute_with_exit(&mut self) -> Result<bool, String> {
        let mut idx = 0;
        while idx < self.lines.len() {
            let result = self.execute_block(idx, 0, true)?;
            idx = result.next;
            if result.exit {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn execute_block(
        &mut self,
        mut idx: usize,
        indent_level: usize,
        should_execute: bool,
    ) -> Result<BlockResult, String> {
        while idx < self.lines.len() {
            let line = self.lines[idx].clone();
            let (indent, content) = split_indent(&line);
            let trimmed = content.trim().to_string();

            if trimmed.is_empty() || trimmed.starts_with('#') {
                idx += 1;
                continue;
            }

            if indent < indent_level {
                break;
            }

            if indent > indent_level {
                return Err(format!("unexpected indent on line {}", idx + 1));
            }

            if trimmed == "}" {
                return Err("unexpected '}'".into());
            }

            let expanded = expand_aliases_in_line(&trimmed, self.state)?;
            let use_expanded = expanded.starts_with("if ")
                || expanded.starts_with("for ")
                || parse_foreach_line(&expanded).is_some();
            let trimmed = if use_expanded { expanded } else { trimmed };

            if let Some(rest) = trimmed.strip_prefix("if ") {
                let brace = parse_brace_block(rest);
                let (condition, brace_block, brace_open, brace_tail, brace_inline_tail) =
                    match brace {
                        BraceParse::Inline { head, body, tail } => {
                            (head, Some(body), false, None, tail)
                        }
                        BraceParse::Open { head, tail } => (head, None, true, Some(tail), None),
                        BraceParse::None { head } => (head, None, false, None, None),
                    };
                let brace_inline_tail = brace_inline_tail;
                let run_child = if should_execute {
                    self.evaluate_condition(condition.trim())?
                } else {
                    false
                };

                if let Some(block) = brace_block {
                    if run_child {
                        execute_inline_block(&block, self.state)?;
                    }
                    let (exit, next_idx, handled) = self.handle_else_chain_tail(
                        brace_inline_tail,
                        idx + 1,
                        indent_level,
                        should_execute && !run_child,
                    )?;
                    idx = next_idx;
                    if exit {
                        return Ok(BlockResult {
                            next: idx,
                            exit: true,
                        });
                    }
                    if handled {
                        continue;
                    }
                    continue;
                }

                if brace_open {
                    let (block_lines, end_idx, tail_line) =
                        collect_brace_block(&self.lines, idx + 1, brace_tail)?;
                    let (exit, next_idx, handled) = self.handle_else_chain_tail(
                        tail_line,
                        end_idx + 1,
                        indent_level,
                        should_execute && !run_child,
                    )?;
                    if run_child {
                        let mut ctx = ScriptContext {
                            lines: block_lines,
                            state: self.state,
                        };
                        if ctx.execute_with_exit()? {
                            return Ok(BlockResult {
                                next: next_idx,
                                exit: true,
                            });
                        }
                    } else if handled {
                        // else/elif chain executed instead
                    }
                    idx = next_idx;
                    if exit {
                        return Ok(BlockResult {
                            next: idx,
                            exit: true,
                        });
                    }
                    continue;
                }

                idx += 1;
                let result =
                    self.execute_block(idx, indent_level + 1, should_execute && run_child)?;
                idx = result.next;

                if result.exit {
                    return Ok(result);
                }

                let (exit, next_idx, handled) =
                    self.handle_else_chain(idx, indent_level, should_execute && !run_child)?;
                idx = next_idx;
                if exit {
                    return Ok(BlockResult {
                        next: idx,
                        exit: true,
                    });
                }
                if handled {
                    continue;
                }
                continue;
            }

            if let Some((var_name, before, after, brace_block, brace_open, brace_tail)) =
                parse_foreach_line(&trimmed)
            {
                if !is_valid_var_name(&var_name) {
                    return Err(format!(
                        "invalid foreach variable '{}' on line {}",
                        var_name,
                        idx + 1
                    ));
                }

                let mut after = after;

                if let Some(block) = brace_block {
                    if should_execute {
                        let mut stages = Vec::new();
                        let before_cmds = build_pipeline_commands(&before, self.state)?;
                        for cmd in before_cmds {
                            stages.push(PipelineStage::External(cmd));
                        }
                        stages.push(PipelineStage::Foreach {
                            var: var_name.clone(),
                            block,
                            inline: true,
                        });
                        let after_stages =
                            build_pipeline_stages_from_segments(&after, self.state)?;
                        stages.extend(after_stages);
                        run_pipeline_stages(stages, self.state)?;
                    }
                    idx += 1;
                    continue;
                }

                if brace_open {
                    let (block_lines, end_idx, tail_line) =
                        collect_brace_block(&self.lines, idx + 1, brace_tail)?;
                    if let Some(tail) = tail_line.as_deref() {
                        append_pipeline_tail(&mut after, tail)?;
                    }
                    if should_execute {
                        let mut stages = Vec::new();
                        let before_cmds = build_pipeline_commands(&before, self.state)?;
                        for cmd in before_cmds {
                            stages.push(PipelineStage::External(cmd));
                        }
                        stages.push(PipelineStage::Foreach {
                            var: var_name.clone(),
                            block: block_lines.join("\n"),
                            inline: false,
                        });
                        let after_stages =
                            build_pipeline_stages_from_segments(&after, self.state)?;
                        stages.extend(after_stages);
                        run_pipeline_stages(stages, self.state)?;
                    }
                    idx = end_idx + 1;
                    continue;
                }

                idx += 1;
                let block_start = idx;
                let block_end = self.find_block_end(block_start, indent_level + 1);
                if !after.is_empty() {
                    return Err("foreach blocks without braces cannot be piped onward".into());
                }

                if should_execute {
                    let mut stages = Vec::new();
                    let before_cmds = build_pipeline_commands(&before, self.state)?;
                    for cmd in before_cmds {
                        stages.push(PipelineStage::External(cmd));
                    }
                    let raw_lines = self.lines[block_start..block_end].to_vec();
                    let block_lines = unindent_block_lines(&raw_lines);
                    stages.push(PipelineStage::Foreach {
                        var: var_name.clone(),
                        block: block_lines.join("\n"),
                        inline: false,
                    });
                    run_pipeline_stages(stages, self.state)?;
                }

                idx = block_end;
                continue;
            }

            if let Some(rest) = trimmed.strip_prefix("for ") {
                let brace = parse_brace_block(rest);
                let (body, brace_block, brace_open, brace_tail) = match brace {
                    BraceParse::Inline { head, body, tail: _ } => (head, Some(body), false, None),
                    BraceParse::Open { head, tail } => (head, None, true, Some(tail)),
                    BraceParse::None { head } => (head, None, false, None),
                };
                let args = parse_args(&body)?;
                if args.len() < 3 || args[1] != "in" {
                    return Err(format!("invalid for syntax on line {}", idx + 1));
                }

                let var_name = &args[0];
                if !is_valid_var_name(var_name) {
                    return Err(format!("invalid for variable '{}' on line {}", var_name, idx + 1));
                }

                let list = expand_tokens(args[2..].to_vec(), &self.state)?;

                if let Some(block) = brace_block {
                    if should_execute {
                        for value in list {
                            self.state.set_var(var_name, value);
                            execute_inline_block(&block, self.state)?;
                        }
                    }
                    idx += 1;
                    continue;
                }

                if brace_open {
                    let (block_lines, end_idx, tail_line) =
                        collect_brace_block(&self.lines, idx + 1, brace_tail)?;
                    if should_execute {
                        for value in list {
                            self.state.set_var(var_name, value);
                            let mut ctx = ScriptContext {
                                lines: block_lines.clone(),
                                state: self.state,
                            };
                            if ctx.execute_with_exit()? {
                                return Ok(BlockResult {
                                    next: end_idx + 1,
                                    exit: true,
                                });
                            }
                        }
                    }
                    if let Some(tail) = tail_line {
                        return Err(format!(
                            "unexpected trailing text after for block: {tail}"
                        ));
                    }
                    idx = end_idx + 1;
                    continue;
                }

                idx += 1;
                let block_start = idx;
                let block_end = self.find_block_end(block_start, indent_level + 1);

                if should_execute {
                    for value in list {
                        self.state.set_var(var_name, value);
                        let result = self.execute_block(block_start, indent_level + 1, true)?;
                        if result.exit {
                            return Ok(result);
                        }
                    }
                }

                idx = block_end;
                continue;
            }

            if should_execute && !process_line_raw(&trimmed, &mut self.state) {
                return Ok(BlockResult {
                    next: idx + 1,
                    exit: true,
                });
            }

            idx += 1;
        }

        Ok(BlockResult {
            next: idx,
            exit: false,
        })
    }

    fn handle_else_chain(
        &mut self,
        mut idx: usize,
        indent_level: usize,
        should_execute: bool,
    ) -> Result<(bool, usize, bool), String> {
        let mut handled = false;
        while idx < self.lines.len() {
            let line = self.lines[idx].clone();
            let (indent, content) = split_indent(&line);
            let trimmed = content.trim().to_string();

            if trimmed.is_empty() || trimmed.starts_with('#') {
                idx += 1;
                continue;
            }

            if indent < indent_level {
                break;
            }

            if indent > indent_level {
                return Err(format!("unexpected indent on line {}", idx + 1));
            }

            let trimmed = expand_aliases_in_line(&trimmed, self.state)?;

            if let Some(rest) = trimmed.strip_prefix("elif ") {
                handled = true;
                let (exit, next_idx, done) =
                    self.handle_else_line(rest, idx + 1, indent_level, should_execute, true)?;
                if done {
                    return Ok((exit, next_idx, true));
                }
                idx = next_idx;
                continue;
            }

            if trimmed == "else" || trimmed.starts_with("else ") {
                handled = true;
                let tail = if trimmed == "else" {
                    ""
                } else {
                    trimmed.strip_prefix("else ").unwrap_or("")
                };
                let (exit, next_idx, done) =
                    self.handle_else_line(tail, idx + 1, indent_level, should_execute, false)?;
                if done {
                    return Ok((exit, next_idx, true));
                }
                idx = next_idx;
                continue;
            }

            break;
        }

        Ok((false, idx, handled))
    }

    fn handle_else_chain_tail(
        &mut self,
        tail_line: Option<String>,
        idx: usize,
        indent_level: usize,
        should_execute: bool,
    ) -> Result<(bool, usize, bool), String> {
        if let Some(tail) = tail_line {
            let trimmed = expand_aliases_in_line(tail.trim(), self.state)?;
            if trimmed.starts_with("elif ") {
                let rest = trimmed.strip_prefix("elif ").unwrap_or("").trim();
                return self.handle_else_line(rest, idx, indent_level, should_execute, true);
            }
            if trimmed.starts_with("else") {
                let rest = trimmed.strip_prefix("else").unwrap_or("").trim();
                return self.handle_else_line(rest, idx, indent_level, should_execute, false);
            }
            return Err("unexpected trailing text after '}'".into());
        }

        self.handle_else_chain(idx, indent_level, should_execute)
    }

    fn handle_else_line(
        &mut self,
        rest: &str,
        block_start: usize,
        indent_level: usize,
        should_execute: bool,
        is_elif: bool,
    ) -> Result<(bool, usize, bool), String> {
        let brace = parse_brace_block(rest);
        let (condition, brace_block, brace_open, brace_tail, brace_inline_tail) = match brace {
            BraceParse::Inline { head, body, tail } => (head, Some(body), false, None, tail),
            BraceParse::Open { head, tail } => (head, None, true, Some(tail), None),
            BraceParse::None { head } => (head, None, false, None, None),
        };

        let run_child = if is_elif {
            if should_execute {
                self.evaluate_condition(condition.trim())?
            } else {
                false
            }
        } else {
            should_execute
        };

        if let Some(block) = brace_block {
            if run_child {
                execute_inline_block(&block, self.state)?;
                if let Some(tail) = brace_inline_tail {
                    let (exit, next_idx, _) = self.handle_else_chain_tail(
                        Some(tail),
                        block_start,
                        indent_level,
                        false,
                    )?;
                    return Ok((exit, next_idx, true));
                }
                let (exit, next_idx, _) =
                    self.handle_else_chain(block_start, indent_level, false)?;
                return Ok((exit, next_idx, true));
            }
            if let Some(tail) = brace_inline_tail {
                return self.handle_else_chain_tail(
                    Some(tail),
                    block_start,
                    indent_level,
                    should_execute && !run_child,
                );
            }
            return self.handle_else_chain(block_start, indent_level, should_execute && !run_child);
        }

        if brace_open {
            let (block_lines, end_idx, tail_line) =
                collect_brace_block(&self.lines, block_start, brace_tail)?;
            if run_child {
                let mut ctx = ScriptContext {
                    lines: block_lines,
                    state: self.state,
                };
                if ctx.execute_with_exit()? {
                    return Ok((true, end_idx + 1, true));
                }
                if let Some(tail) = tail_line {
                    let (exit, next_idx, _) = self.handle_else_chain_tail(
                        Some(tail),
                        end_idx + 1,
                        indent_level,
                        false,
                    )?;
                    return Ok((exit, next_idx, true));
                }
                let (exit, next_idx, _) =
                    self.handle_else_chain(end_idx + 1, indent_level, false)?;
                return Ok((exit, next_idx, true));
            }
            if let Some(tail) = tail_line {
                return self.handle_else_chain_tail(
                    Some(tail),
                    end_idx + 1,
                    indent_level,
                    should_execute && !run_child,
                );
            }
            return self.handle_else_chain(end_idx + 1, indent_level, should_execute && !run_child);
        }

        let result = self.execute_block(block_start, indent_level + 1, run_child)?;
        if result.exit {
            return Ok((true, result.next, true));
        }
        if run_child {
            return Ok((false, result.next, true));
        }
        self.handle_else_chain(result.next, indent_level, should_execute && !run_child)
    }

    fn evaluate_condition(&self, command: &str) -> Result<bool, String> {
        let args = parse_args(command)?;
        if args.is_empty() {
            return Ok(false);
        }
        let expanded = expand_tokens(args, &self.state)?;

        match Command::new(&expanded[0]).args(&expanded[1..]).status() {
            Ok(status) => Ok(status.success()),
            Err(err) => Err(format!(
                "failed to execute condition '{}': {err}",
                expanded[0]
            )),
        }
    }

    fn find_block_end(&self, mut idx: usize, indent_level: usize) -> usize {
        while idx < self.lines.len() {
            let raw = &self.lines[idx];
            let (indent, content) = split_indent(raw);
            let trimmed = content.trim();

            if trimmed.is_empty() || trimmed.starts_with('#') {
                idx += 1;
                continue;
            }

            if indent < indent_level {
                break;
            }

            idx += 1;
        }

        idx
    }
}

fn print_prompt(stdout: &mut io::Stdout) -> io::Result<()> {
    stdout.write_all(b"unshell> ")?;
    stdout.flush()
}

fn execute_with_status(args: &[String]) -> bool {
    if args.is_empty() {
        return true;
    }

    match Command::new(&args[0]).args(&args[1..]).status() {
        Ok(status) => {
            report_status(&args[0], status);
            status.success()
        }
        Err(err) => {
            eprintln!("unshell: failed to execute '{}': {err}", args[0]);
            false
        }
    }
}

fn report_status(cmd: &str, status: ExitStatus) {
    if let Some(code) = status.code() {
        if code != 0 {
            eprintln!("unshell: '{}' exited with status {code}", cmd);
        }
    } else if !status.success() {
        eprintln!("unshell: '{}' terminated by signal", cmd);
    }
}
