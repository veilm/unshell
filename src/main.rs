use std::collections::HashMap;
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::io::IsTerminal;
use std::os::unix::io::{AsRawFd, FromRawFd};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};

mod expand;
mod parser;
#[cfg(feature = "repl")]
mod repl;
mod state;
mod term;
mod workers;

use crate::expand::{
    expand_tokens, expand_tokens_with_meta, expand_unquoted_token, is_var_char, is_var_start,
    ExpandedToken,
};
use crate::parser::{
    append_pipeline_tail, collect_brace_block, parse_args, parse_brace_block, parse_foreach_line,
    split_indent, split_on_pipes, unindent_block_lines, BraceParse, LogicOp,
};
use crate::state::{lookup_var, write_locals_file, FunctionBody, FunctionDef, ShellState};
use crate::workers::{
    run_block_worker, run_capture_worker, run_foreach_worker, run_function_worker, write_block_file,
};
#[cfg(not(feature = "repl"))]
use crate::term::cursor_column;

const FUNCTION_MAX_DEPTH: usize = 64;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 && args[1] == "--foreach-worker" {
        if let Err(err) = run_foreach_worker(&args[2..]) {
            eprintln!("unshell: {err}");
            std::process::exit(1);
        }
        return;
    }
    if args.len() > 1 && args[1] == "--function-worker" {
        match run_function_worker(&args[2..]) {
            Ok(code) => std::process::exit(code),
            Err(err) => {
                eprintln!("unshell: {err}");
                std::process::exit(1);
            }
        }
    }
    if args.len() > 1 && args[1] == "--block-worker" {
        match run_block_worker(&args[2..]) {
            Ok(code) => std::process::exit(code),
            Err(err) => {
                eprintln!("unshell: {err}");
                std::process::exit(1);
            }
        }
    }
    if args.len() > 1 && args[1] == "--capture-worker" {
        if let Err(err) = run_capture_worker(&args[2..]) {
            eprintln!("unshell: {err}");
            std::process::exit(1);
        }
        return;
    }

    let (startup, script, script_args) = match parse_startup_args(&args[1..]) {
        Ok(value) => value,
        Err(err) => {
            eprintln!("unshell: {err}");
            std::process::exit(1);
        }
    };

    if let Some(script) = script {
        let mut state = ShellState::new();
        init_shell_state(&mut state, "script");
        state.positional = script_args;
        if let Err(err) = load_startup(&mut state, &startup) {
            eprintln!("unshell: failed to load startup: {err}");
        }
        if let Err(err) = run_script_with_state(&script, state) {
            eprintln!("unshell: failed to run script '{}': {err}", script);
            std::process::exit(1);
        }
        return;
    }

    run_repl(&startup);
}

#[cfg(feature = "repl")]
fn run_repl(startup: &StartupConfig) {
    let mut state = ShellState::new();
    init_shell_state(&mut state, "repl");
    if let Err(err) = load_startup(&mut state, startup) {
        eprintln!("unshell: failed to load startup: {err}");
    }
    repl::run_repl(&mut state);
}

#[cfg(not(feature = "repl"))]
fn run_repl(startup: &StartupConfig) {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut state = ShellState::new();

    init_shell_state(&mut state, "repl");
    if let Err(err) = load_startup(&mut state, startup) {
        eprintln!("unshell: failed to load startup: {err}");
    }
    println!("unshell: compiled without rustyline repl; using minimal input (no line editing or completion)");

    loop {
        maybe_print_incomplete_marker(&mut stdout, &mut state);
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

#[cfg(not(feature = "repl"))]
fn print_incomplete_marker(stdout: &mut io::Stdout) -> io::Result<()> {
    stdout.write_all(b"\x1b[7m$\x1b[0m\n")?;
    stdout.flush()
}

#[cfg(not(feature = "repl"))]
fn maybe_print_incomplete_marker(
    stdout: &mut io::Stdout,
    state: &mut ShellState,
) {
    if state.needs_cursor_check {
        if let Some(column) = cursor_column() {
            if column != 1 {
                let _ = print_incomplete_marker(stdout);
            }
        }
        state.needs_cursor_check = false;
        return;
    }
    if let Some(column) = cursor_column() {
        if column != 1 {
            let _ = print_incomplete_marker(stdout);
        }
        return;
    }
    if !state.last_output_newline {
        let _ = print_incomplete_marker(stdout);
        state.last_output_newline = true;
    }
}

#[derive(Default)]
struct StartupConfig {
    no_rc: bool,
    rc_path: Option<String>,
}

fn parse_startup_args(args: &[String]) -> Result<(StartupConfig, Option<String>, Vec<String>), String> {
    let mut config = StartupConfig::default();
    let mut idx = 0;
    let mut script = None;
    let mut script_args = Vec::new();

    while idx < args.len() {
        let arg = &args[idx];
        if arg == "--norc" {
            config.no_rc = true;
            idx += 1;
            continue;
        }
        if arg == "--rc" {
            let value = args.get(idx + 1).ok_or("--rc requires a path")?;
            config.rc_path = Some(value.clone());
            idx += 2;
            continue;
        }
        if arg.starts_with('-') {
            return Err(format!("unknown option '{arg}'"));
        }
        script = Some(arg.clone());
        script_args = args[idx + 1..].to_vec();
        break;
    }

    Ok((config, script, script_args))
}

fn init_shell_state(state: &mut ShellState, mode: &str) {
    let shell = "ush";
    unsafe {
        std::env::set_var("SHELL", shell);
        std::env::set_var("USH_MODE", mode);
    }
    state.set_var("SHELL", shell.to_string());
    state.set_var("USH_MODE", mode.to_string());
    state.interactive = mode == "repl";
    state.last_output_newline = true;
    state.needs_cursor_check = false;
}

fn run_script_with_state(path: &str, mut state: ShellState) -> io::Result<()> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().collect::<Result<_, _>>()?;

    let mut ctx = ScriptContext {
        lines,
        state: &mut state,
    };
    ctx.execute()
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;

    Ok(())
}

fn source_file(path: &Path, state: &mut ShellState) -> io::Result<()> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().collect::<Result<_, _>>()?;
    let mut ctx = ScriptContext {
        lines,
        state,
    };
    ctx.execute()
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    Ok(())
}

fn load_startup(state: &mut ShellState, startup: &StartupConfig) -> io::Result<()> {
    if startup.no_rc {
        return Ok(());
    }
    if let Some(path) = startup.rc_path.as_deref() {
        return source_file(Path::new(path), state);
    }
    let mut candidates = Vec::new();
    candidates.push(PathBuf::from("/etc/unshell/init"));
    if let Ok(xdg) = env::var("XDG_CONFIG_HOME") {
        if !xdg.trim().is_empty() {
            candidates.push(Path::new(&xdg).join("unshell").join("init"));
        }
    }
    if let Ok(home) = env::var("HOME") {
        candidates.push(Path::new(&home).join(".config").join("unshell").join("init"));
        candidates.push(Path::new(&home).join(".unshell").join("init"));
    }

    for path in candidates {
        if path.exists() {
            return source_file(&path, state);
        }
    }

    Ok(())
}


pub(crate) fn process_line(line: &str, state: &mut ShellState) -> bool {
    let mut ctx = ScriptContext {
        lines: vec![line.to_string()],
        state,
    };
    match ctx.execute_with_exit() {
        Ok(FlowControl::Exit) => false,
        Ok(FlowControl::Return(_)) => {
            eprintln!("unshell: return not allowed outside functions");
            true
        }
        Ok(FlowControl::None) => true,
        Err(err) => {
            eprintln!("unshell: {err}");
            true
        }
    }
}

fn process_line_raw(line: &str, state: &mut ShellState) -> FlowControl {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return FlowControl::None;
    }

    let tokens = match raw_line_tokens(trimmed, state) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("unshell: {err}");
            state.last_status = 1;
            return FlowControl::None;
        }
    };

    let sequences = split_tokens_on_semicolons(&tokens);
    for sequence in sequences {
        if sequence.is_empty() {
            continue;
        }
        let chain = match split_tokens_on_and_or(&sequence) {
            Ok(chain) => chain,
            Err(err) => {
                eprintln!("unshell: {err}");
                state.last_status = 1;
                continue;
            }
        };
        let mut last_success = true;
        for (op, segment) in chain {
            if segment.is_empty() {
                continue;
            }
            match op {
                LogicOp::And if !last_success => continue,
                LogicOp::Or if last_success => continue,
                _ => {}
            }
            let expanded = match expand_op_tokens(&segment, state) {
                Ok(tokens) => tokens,
                Err(err) => {
                    eprintln!("unshell: {err}");
                    last_success = false;
                    state.last_status = 1;
                    continue;
                }
            };
            match run_expanded_tokens(&expanded, state) {
                Ok(FlowControl::Exit) => return FlowControl::Exit,
                Ok(FlowControl::Return(code)) => return FlowControl::Return(code),
                Ok(FlowControl::None) => {
                    last_success = state.last_status == 0;
                }
                Err(err) => {
                    eprintln!("unshell: {err}");
                    last_success = false;
                    state.last_status = 1;
                }
            }
        }
    }
    FlowControl::None
}

fn run_expanded_tokens(tokens: &[OpToken], state: &mut ShellState) -> Result<FlowControl, String> {
    let sequences = split_tokens_on_semicolons(tokens);
    for sequence in sequences {
        if sequence.is_empty() {
            continue;
        }
        let chain = split_tokens_on_and_or(&sequence)?;
        let mut last_success = true;
        for (op, segment) in chain {
            if segment.is_empty() {
                continue;
            }
            match op {
                LogicOp::And if !last_success => continue,
                LogicOp::Or if last_success => continue,
                _ => {}
            }
            match run_pipeline_tokens(&segment, state)? {
                RunResult::Exit => return Ok(FlowControl::Exit),
                RunResult::Return(code) => return Ok(FlowControl::Return(code)),
                RunResult::Success(success) => last_success = success,
            }
        }
    }
    Ok(FlowControl::None)
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Operator {
    Pipe,
    And,
    Or,
    Semi,
}

#[derive(Clone, Debug)]
struct WordToken {
    value: String,
    protected: bool,
}

#[derive(Clone, Debug)]
enum OpToken {
    Word(WordToken),
    Op(Operator),
}

#[derive(Clone, Debug)]
enum OutputTarget {
    File(String),
    Stdout,
    Stderr,
    Null,
}

#[derive(Clone, Debug)]
struct OutputRedir {
    target: OutputTarget,
    append: bool,
}

#[derive(Clone, Debug, Default)]
struct Redirections {
    stdin: Option<String>,
    stdout: Option<OutputRedir>,
    stderr: Option<OutputRedir>,
}

#[derive(Clone, Debug)]
struct CommandSpec {
    args: Vec<String>,
    redirs: Redirections,
}

#[derive(Clone, Debug)]
struct FunctionCall {
    name: String,
    args: Vec<String>,
    redirs: Redirections,
}

#[derive(Clone, Debug)]
struct InlineBlock {
    block: String,
    redirs: Redirections,
}

fn raw_line_tokens(line: &str, state: &ShellState) -> Result<Vec<OpToken>, String> {
    let args = parse_args(line)?;
    let args = apply_alias(args, state)?;
    let mut tokens = Vec::new();

    for token in args {
        if let Some(op) = token_to_operator(&token) {
            tokens.push(OpToken::Op(op));
            continue;
        }
        if contains_top_level_operator(&token) {
            return Err("operators must be separated by whitespace".into());
        }
        tokens.push(OpToken::Word(WordToken {
            value: token,
            protected: false,
        }));
    }

    Ok(tokens)
}

fn is_inline_block_token(token: &str) -> bool {
    matches!(
        parse_brace_block(token),
        BraceParse::Inline { head, tail: None, .. } if head.trim().is_empty()
    )
}

fn expand_op_tokens(tokens: &[OpToken], state: &ShellState) -> Result<Vec<OpToken>, String> {
    let mut out = Vec::new();
    for token in tokens {
        match token {
            OpToken::Op(op) => out.push(OpToken::Op(op.clone())),
            OpToken::Word(word) => {
                if is_inline_block_token(&word.value) {
                    out.push(OpToken::Word(WordToken {
                        value: word.value.clone(),
                        protected: true,
                    }));
                    continue;
                }
                let expanded = expand_tokens_with_meta(vec![word.value.clone()], state)?;
                let mut parts = split_expanded_tokens(expanded)?;
                out.append(&mut parts);
            }
        }
    }
    Ok(out)
}

fn expand_line_tokens_spread_only(line: &str, state: &mut ShellState) -> Result<Vec<OpToken>, String> {
    let args = parse_args(line)?;
    let args = apply_alias(args, state)?;
    let mut tokens = Vec::new();

    for token in args {
        if token.starts_with("...") {
            let suffix = &token[3..];
            if suffix.is_empty() {
                return Err("spread requires content".into());
            }
            let source = expand_unquoted_token(suffix, state)?;
            let spread_tokens = parse_args(&source)?;
            tokens.extend(spread_tokens);
        } else {
            tokens.push(token);
        }
    }

    let expanded = tokens
        .into_iter()
        .map(|token| ExpandedToken {
            value: token.clone(),
            protected: token.contains('"') || token.contains('\''),
            allow_split: token_contains_operator(&token),
        })
        .collect();

    split_expanded_tokens(expanded)
}

fn split_expanded_tokens(tokens: Vec<ExpandedToken>) -> Result<Vec<OpToken>, String> {
    let mut out = Vec::new();
    for token in tokens {
        let mut parts = split_token_ops(&token)?;
        out.append(&mut parts);
    }
    Ok(out)
}

fn split_token_ops(token: &ExpandedToken) -> Result<Vec<OpToken>, String> {
    if token.protected || !token.allow_split {
        return Ok(vec![OpToken::Word(WordToken {
            value: token.value.clone(),
            protected: token.protected,
        })]);
    }
    if token.value.is_empty() {
        return Ok(vec![OpToken::Word(WordToken {
            value: String::new(),
            protected: token.protected,
        })]);
    }

    if let Some(op) = token_to_operator(&token.value) {
        return Ok(vec![OpToken::Op(op)]);
    }

    if contains_top_level_operator(&token.value) {
        return Err("operators must be separated by whitespace".into());
    }

    Ok(vec![OpToken::Word(WordToken {
        value: token.value.clone(),
        protected: token.protected,
    })])
}

fn token_contains_operator(token: &str) -> bool {
    token.contains(';') || token.contains("||") || token.contains('|') || token.contains("&&")
}

fn token_to_operator(token: &str) -> Option<Operator> {
    match token {
        "|" => Some(Operator::Pipe),
        "||" => Some(Operator::Or),
        "&&" => Some(Operator::And),
        ";" => Some(Operator::Semi),
        _ => None,
    }
}

fn contains_top_level_operator(token: &str) -> bool {
    let chars: Vec<char> = token.chars().collect();
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut brace_depth = 0;
    let mut in_double = false;
    let mut in_single = false;
    let mut idx = 0;

    while idx < chars.len() {
        let ch = chars[idx];

        if in_double && ch == '\\' && paren_depth == 0 {
            idx += 2;
            continue;
        }

        match ch {
            '\'' if !in_double && bracket_depth == 0 && paren_depth == 0 => {
                in_single = !in_single;
                idx += 1;
                continue;
            }
            '"' if !in_single && bracket_depth == 0 && paren_depth == 0 => {
                in_double = !in_double;
                idx += 1;
                continue;
            }
            _ => {}
        }

        if ch == '$' && bracket_depth == 0 && paren_depth == 0 {
            if idx + 1 < chars.len() && chars[idx + 1] == '(' {
                paren_depth = 1;
                idx += 2;
                continue;
            }
        }

        if ch == '[' && paren_depth == 0 {
            if bracket_depth == 0 {
                if idx + 1 < chars.len() && chars[idx + 1].is_whitespace() {
                    idx += 1;
                    continue;
                }
                bracket_depth = 1;
            } else {
                bracket_depth += 1;
            }
            idx += 1;
            continue;
        }

        if ch == ']' && bracket_depth > 0 {
            bracket_depth -= 1;
            idx += 1;
            continue;
        }

        if ch == '(' && paren_depth > 0 {
            paren_depth += 1;
            idx += 1;
            continue;
        }

        if ch == ')' && paren_depth > 0 {
            paren_depth -= 1;
            idx += 1;
            continue;
        }

        if bracket_depth == 0 && paren_depth == 0 && !in_double && !in_single {
            if ch == '{' {
                brace_depth += 1;
                idx += 1;
                continue;
            }
            if ch == '}' && brace_depth > 0 {
                brace_depth -= 1;
                idx += 1;
                continue;
            }
        }

        if bracket_depth == 0
            && paren_depth == 0
            && brace_depth == 0
            && !in_double
            && !in_single
        {
            if ch == ';' || ch == '|' {
                return true;
            }
            if ch == '&' && idx + 1 < chars.len() && chars[idx + 1] == '&' {
                return true;
            }
        }

        idx += 1;
    }

    false
}

fn split_tokens_on_semicolons(tokens: &[OpToken]) -> Vec<Vec<OpToken>> {
    let mut parts = Vec::new();
    let mut current = Vec::new();

    for token in tokens {
        if matches!(token, OpToken::Op(Operator::Semi)) {
            if !current.is_empty() {
                parts.push(current);
                current = Vec::new();
            }
            continue;
        }
        current.push(token.clone());
    }

    if !current.is_empty() {
        parts.push(current);
    }

    parts
}

fn split_tokens_on_and_or(tokens: &[OpToken]) -> Result<Vec<(LogicOp, Vec<OpToken>)>, String> {
    let mut parts = Vec::new();
    let mut current = Vec::new();
    let mut next_op = LogicOp::None;

    for token in tokens {
        match token {
            OpToken::Op(Operator::And) => {
                if current.is_empty() {
                    return Err("empty command in chain".into());
                }
                parts.push((next_op, current));
                current = Vec::new();
                next_op = LogicOp::And;
            }
            OpToken::Op(Operator::Or) => {
                if current.is_empty() {
                    return Err("empty command in chain".into());
                }
                parts.push((next_op, current));
                current = Vec::new();
                next_op = LogicOp::Or;
            }
            _ => current.push(token.clone()),
        }
    }

    if current.is_empty() {
        if parts.is_empty() {
            return Ok(Vec::new());
        }
        return Err("empty command in chain".into());
    }

    parts.push((next_op, current));
    Ok(parts)
}

fn split_tokens_on_pipes(tokens: &[OpToken]) -> Result<Vec<Vec<WordToken>>, String> {
    let mut commands = Vec::new();
    let mut current = Vec::new();

    for token in tokens {
        match token {
            OpToken::Word(word) => current.push(word.clone()),
            OpToken::Op(Operator::Pipe) => {
                if current.is_empty() {
                    return Err("empty command in pipeline".into());
                }
                commands.push(current);
                current = Vec::new();
            }
            OpToken::Op(Operator::And)
            | OpToken::Op(Operator::Or)
            | OpToken::Op(Operator::Semi) => {
                return Err("unexpected operator in pipeline".into());
            }
        }
    }

    if current.is_empty() {
        return Err("empty command in pipeline".into());
    }
    commands.push(current);

    Ok(commands)
}

fn parse_redirections(tokens: Vec<WordToken>) -> Result<(Vec<String>, Redirections), String> {
    let mut args = Vec::new();
    let mut redirs = Redirections::default();
    let mut idx = 0;

    while idx < tokens.len() {
        let token = &tokens[idx];
        if token.protected {
            args.push(token.value.clone());
            idx += 1;
            continue;
        }

        if let Some((out_redir, consumed)) = parse_output_redirection(&tokens, idx)? {
            apply_output_redirection(&mut redirs, out_redir);
            idx += consumed;
            continue;
        }

        if let Some((input_path, consumed)) = parse_input_redirection(&tokens, idx)? {
            redirs.stdin = Some(input_path);
            idx += consumed;
            continue;
        }

        if token.value.contains('>') || token.value.contains('<') {
            return Err("redirection operators must be separated by whitespace".into());
        }

        args.push(token.value.clone());
        idx += 1;
    }

    Ok((args, redirs))
}

fn parse_input_redirection(tokens: &[WordToken], idx: usize) -> Result<Option<(String, usize)>, String> {
    let token = &tokens[idx];
    if token.value != "<" {
        return Ok(None);
    }
    let target = tokens.get(idx + 1).ok_or_else(|| "redirection missing destination".to_string())?;
    if !target.protected && (target.value.contains('>') || target.value.contains('<')) {
        return Err("redirection operators must be separated by whitespace".into());
    }
    Ok(Some((target.value.clone(), 2)))
}

fn parse_output_redirection(
    tokens: &[WordToken],
    idx: usize,
) -> Result<Option<(OutputSpec, usize)>, String> {
    let token = &tokens[idx];
    let value = token.value.as_str();

    if let Some((spec, consumed)) = parse_output_with_inline_target(value)? {
        return Ok(Some((spec, consumed)));
    }

    let (stream, append) = match value {
        ">" => (OutputStream::Stdout, false),
        ">>" => (OutputStream::Stdout, true),
        "out>" | "o>" => (OutputStream::Stdout, false),
        "out>>" | "o>>" => (OutputStream::Stdout, true),
        "err>" | "e>" => (OutputStream::Stderr, false),
        "err>>" | "e>>" => (OutputStream::Stderr, true),
        "out+err>" | "o+e>" => (OutputStream::Both, false),
        "out+err>>" | "o+e>>" => (OutputStream::Both, true),
        _ => return Ok(None),
    };

    let target = tokens
        .get(idx + 1)
        .ok_or_else(|| "redirection missing destination".to_string())?;
    if !target.protected && (target.value.contains('>') || target.value.contains('<')) {
        return Err("redirection operators must be separated by whitespace".into());
    }
    let output = OutputRedir {
        target: OutputTarget::File(target.value.clone()),
        append,
    };
    let consumed = 2;
    Ok(Some((OutputSpec { stream, redir: output }, consumed)))
}

fn parse_output_with_inline_target(value: &str) -> Result<Option<(OutputSpec, usize)>, String> {
    let prefixes = [
        ("out+err>>", OutputStream::Both, true),
        ("out+err>", OutputStream::Both, false),
        ("o+e>>", OutputStream::Both, true),
        ("o+e>", OutputStream::Both, false),
        ("out>>", OutputStream::Stdout, true),
        ("out>", OutputStream::Stdout, false),
        ("o>>", OutputStream::Stdout, true),
        ("o>", OutputStream::Stdout, false),
        ("err>>", OutputStream::Stderr, true),
        ("err>", OutputStream::Stderr, false),
        ("e>>", OutputStream::Stderr, true),
        ("e>", OutputStream::Stderr, false),
    ];

    for (prefix, stream, append) in prefixes {
        if let Some(suffix) = value.strip_prefix(prefix) {
            if suffix.is_empty() {
                return Ok(None);
            }
            let target = match suffix {
                "out" | "o" => OutputTarget::Stdout,
                "err" | "e" => OutputTarget::Stderr,
                "null" | "n" => OutputTarget::Null,
                _ => {
                    return Err("redirection destination must be separated by whitespace".into());
                }
            };
            let output = OutputRedir { target, append };
            return Ok(Some((OutputSpec { stream, redir: output }, 1)));
        }
    }

    if (value.starts_with('>') || value.starts_with('<')) && !matches!(value, ">" | ">>" | "<") {
        return Err("redirection operators must be separated by whitespace".into());
    }

    Ok(None)
}

#[derive(Clone, Copy)]
enum OutputStream {
    Stdout,
    Stderr,
    Both,
}

struct OutputSpec {
    stream: OutputStream,
    redir: OutputRedir,
}

fn apply_output_redirection(redirs: &mut Redirections, spec: OutputSpec) {
    match spec.stream {
        OutputStream::Stdout => {
            redirs.stdout = Some(spec.redir);
        }
        OutputStream::Stderr => {
            redirs.stderr = Some(spec.redir);
        }
        OutputStream::Both => {
            redirs.stdout = Some(spec.redir.clone());
            redirs.stderr = Some(spec.redir);
        }
    }
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
    Return(i32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FlowControl {
    None,
    Exit,
    Return(i32),
}

struct ForeachTokens {
    var_name: String,
    before: Vec<Vec<String>>,
    after: Vec<Vec<String>>,
    brace_block: Option<String>,
    brace_open: bool,
    brace_tail: Option<String>,
}

fn run_pipeline_tokens(tokens: &[OpToken], state: &mut ShellState) -> Result<RunResult, String> {
    let commands = split_tokens_on_pipes(tokens)?;
    if commands.is_empty() {
        return Ok(RunResult::Success(true));
    }

    if commands.len() == 1 {
        let args = &commands[0];
        let raw_args: Vec<String> = args.iter().map(|t| t.value.clone()).collect();
        let (assignments, remaining) = split_assignments(&raw_args);
        let assignments_len = assignments.len();
        for (name, value) in assignments {
            state.set_var(&name, value);
        }
        if remaining.is_empty() {
            state.last_status = 0;
            return Ok(RunResult::Success(true));
        }
        let remaining_tokens: Vec<WordToken> = args
            .iter()
            .skip(assignments_len)
            .cloned()
            .collect();
        let (remaining_args, redirs) = parse_redirections(remaining_tokens)?;
        if remaining_args.is_empty() {
            return Err("empty command in pipeline".into());
        }

        if let Some(func) = state.functions.get(&remaining_args[0]).cloned() {
            return with_redirections(&redirs, || run_function(&remaining_args, func, state));
        }
        if is_builtin(&remaining_args[0]) {
            return with_redirections(&redirs, || {
                run_builtin(&remaining_args, state)?
                    .ok_or_else(|| "builtin dispatch failed".to_string())
            });
        }
        let success = run_pipeline(
            vec![CommandSpec {
                args: remaining_args,
                redirs,
            }],
            state,
        )?;
        return Ok(RunResult::Success(success));
    }

    let stages = build_pipeline_stages_from_word_segments(commands, state)?;
    run_pipeline_stages(stages, state).map(RunResult::Success)
}

fn build_pipeline_stages_from_word_segments(
    segments: Vec<Vec<WordToken>>,
    state: &mut ShellState,
) -> Result<Vec<PipelineStage>, String> {
    let mut stages = Vec::new();

    for segment in segments {
        if segment.is_empty() {
            return Err("empty command in pipeline".into());
        }

        let segment_line = segment
            .iter()
            .map(|token| token.value.as_str())
            .collect::<Vec<_>>()
            .join(" ");
        let brace = parse_brace_block(&segment_line);
        match brace {
            BraceParse::Inline { head, body, tail } if head.trim().is_empty() => {
                if let Some(tail) = tail {
                    return Err(format!("unexpected trailing text after block: {tail}"));
                }
                stages.push(PipelineStage::Block(InlineBlock {
                    block: body,
                    redirs: Redirections::default(),
                }));
                continue;
            }
            BraceParse::Open { head, .. } if head.trim().is_empty() => {
                return Err("inline block missing '}' in pipeline".into());
            }
            _ => {}
        }

        let raw: Vec<String> = segment.iter().map(|t| t.value.clone()).collect();
        let (assignments, remaining) = split_assignments(&raw);
        let assignments_len = assignments.len();
        for (name, value) in assignments {
            state.set_var(&name, value);
        }
        if remaining.is_empty() {
            return Err("empty command in pipeline".into());
        }
        let remaining_tokens: Vec<WordToken> = segment
            .into_iter()
            .skip(assignments_len)
            .collect();
        let (args, redirs) = parse_redirections(remaining_tokens)?;
        if args.is_empty() {
            return Err("empty command in pipeline".into());
        }
        if state.functions.contains_key(&args[0]) {
            stages.push(PipelineStage::Function(FunctionCall {
                name: args[0].clone(),
                args: args[1..].to_vec(),
                redirs,
            }));
            continue;
        }
        stages.push(PipelineStage::External(CommandSpec { args, redirs }));
    }

    Ok(stages)
}

fn run_function(
    args: &[String],
    func: FunctionDef,
    state: &mut ShellState,
) -> Result<RunResult, String> {
    if state.locals_stack.len() >= FUNCTION_MAX_DEPTH {
        return Err("function call depth exceeded".into());
    }
    state.positional_stack.push(state.positional.clone());
    state.positional = args[1..].to_vec();
    state.locals_stack.push(HashMap::new());
    state.last_status = 0;

    let result = match func.body {
        FunctionBody::Inline(body) => execute_inline_block(&body, state),
        FunctionBody::Block(lines) => {
            let mut ctx = ScriptContext { lines, state };
            ctx.execute_with_exit()
        }
    };

    let flow = result;
    state.positional = state.positional_stack.pop().unwrap_or_default();
    state.locals_stack.pop();

    match flow? {
        FlowControl::Exit => Ok(RunResult::Exit),
        FlowControl::Return(code) => Ok(RunResult::Success(code == 0)),
        FlowControl::None => Ok(RunResult::Success(state.last_status == 0)),
    }
}

fn parse_foreach_tokens(tokens: &[OpToken]) -> Option<ForeachTokens> {
    let segments = split_tokens_on_pipes(tokens).ok()?;
    if segments.len() < 2 {
        return None;
    }

    for (idx, segment) in segments.iter().enumerate() {
        let segment_line = segment
            .iter()
            .map(|token| token.value.as_str())
            .collect::<Vec<_>>()
            .join(" ");
        let brace = parse_brace_block(&segment_line);
        let (head, brace_block, brace_open, brace_tail) = match brace {
            BraceParse::Inline { head, body, tail } => (head, Some(body), false, tail),
            BraceParse::Open { head, tail } => (head, None, true, Some(tail)),
            BraceParse::None { head } => (head, None, false, None),
        };
        let args = parse_args(&head).ok()?;
        if args.len() == 2 && args[0] == "foreach" {
            let before = segments[..idx]
                .iter()
                .map(|seg| seg.iter().map(|t| t.value.clone()).collect())
                .collect();
            let after = segments[idx + 1..]
                .iter()
                .map(|seg| seg.iter().map(|t| t.value.clone()).collect())
                .collect();
            return Some(ForeachTokens {
                var_name: args[1].clone(),
                before,
                after,
                brace_block,
                brace_open,
                brace_tail,
            });
        }
    }

    None
}

fn unindent_block_lines_by(lines: &[String], tabs: usize) -> Vec<String> {
    lines
        .iter()
        .map(|line| {
            let mut rest = line.as_str();
            for _ in 0..tabs {
                if let Some(next) = rest.strip_prefix('\t') {
                    rest = next;
                } else {
                    break;
                }
            }
            rest.to_string()
        })
        .collect()
}

fn build_pipeline_stages_from_token_segments(
    segments: &[Vec<String>],
    state: &mut ShellState,
) -> Result<Vec<PipelineStage>, String> {
    let mut stages = Vec::new();

    for segment in segments {
        if segment.is_empty() {
            return Err("empty command in pipeline".into());
        }

        let segment_line = segment.join(" ");
        let brace = parse_brace_block(&segment_line);
        match brace {
            BraceParse::Inline { head, body, tail } if head.trim().is_empty() => {
                if let Some(tail) = tail {
                    return Err(format!("unexpected trailing text after block: {tail}"));
                }
                stages.push(PipelineStage::Block(InlineBlock {
                    block: body,
                    redirs: Redirections::default(),
                }));
                continue;
            }
            BraceParse::Open { head, .. } if head.trim().is_empty() => {
                return Err("inline block missing '}' in pipeline".into());
            }
            _ => {}
        }
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
                    return Err("foreach blocks in pipeline tails must use inline braces".into());
                }
                if let Some(tail) = brace_tail {
                    return Err(format!(
                        "unexpected trailing text after foreach block: {tail}"
                    ));
                }
                let block = brace_block.ok_or_else(|| {
                    "foreach blocks in pipeline tails must use inline braces".to_string()
                })?;
                stages.push(PipelineStage::Foreach {
                    var: args[1].clone(),
                    block,
                    inline: true,
                });
                continue;
            }
        }

        let expanded = expand_tokens_with_meta(segment.clone(), state)?;
        let words: Vec<WordToken> = expanded
            .into_iter()
            .map(|token| WordToken {
                value: token.value,
                protected: token.protected,
            })
            .collect();
        let raw_values: Vec<String> = words.iter().map(|t| t.value.clone()).collect();
        let (assignments, remaining) = split_assignments(&raw_values);
        let assignments_len = assignments.len();
        for (name, value) in assignments {
            state.set_var(&name, value);
        }
        if remaining.is_empty() {
            return Err("empty command in pipeline".into());
        }
        let remaining_tokens: Vec<WordToken> = words
            .into_iter()
            .skip(assignments_len)
            .collect();
        let (args, redirs) = parse_redirections(remaining_tokens)?;
        if args.is_empty() {
            return Err("empty command in pipeline".into());
        }
        if state.functions.contains_key(&args[0]) {
            stages.push(PipelineStage::Function(FunctionCall {
                name: args[0].clone(),
                args: args[1..].to_vec(),
                redirs,
            }));
            continue;
        }
        stages.push(PipelineStage::External(CommandSpec { args, redirs }));
    }

    Ok(stages)
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "cd"
            | "alias"
            | "unalias"
            | "set"
            | "export"
            | "local"
            | "return"
            | "exit"
            | "builtin"
            | "source"
            | "eval"
    )
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
            let _ = cwd;
            state.last_status = 0;
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
                let value = lookup_var(state, item);
                state.set_var(item, value.clone());
                unsafe {
                    env::set_var(item, value);
                }
            }
            state.last_status = 0;
            Ok(Some(RunResult::Success(true)))
        }
        "exit" => {
            if args.len() > 2 {
                return Err("exit: too many arguments".into());
            }
            let code = if args.len() == 2 {
                args[1]
                    .parse::<i32>()
                    .map_err(|_| "exit: status must be an integer".to_string())?
            } else {
                state.last_status
            };
            state.last_status = code;
            Ok(Some(RunResult::Exit))
        }
        "local" => {
            if args.len() != 2 {
                return Err("local: expected NAME or NAME=VALUE".into());
            }
            let item = &args[1];
            if let Some((name, value)) = parse_assignment(item) {
                state.set_local_var(&name, value)?;
            } else {
                if !is_valid_var_name(item) {
                    return Err(format!("local: invalid name '{item}'"));
                }
                state.set_local_var(item, String::new())?;
            }
            state.last_status = 0;
            Ok(Some(RunResult::Success(true)))
        }
        "alias" => run_alias_builtin_expanded(args, state),
        "unalias" => {
            if args.len() != 2 {
                return Err("unalias: expected exactly one name".into());
            }
            let alias_name = &args[1];
            if !state.remove_alias(alias_name) {
                return Err(format!("unalias: no such alias '{alias_name}'"));
            }
            state.last_status = 0;
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
                    state.last_status = 0;
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
                    state.last_status = 0;
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
                    state.last_status = 0;
                    Ok(Some(RunResult::Success(true)))
                }
                "expansions.handler" => {
                    if args.len() < 3 {
                        return Err("set: expansions.handler expects a command".into());
                    }
                    state.options.expansions_handler = args[2..].to_vec();
                    state.last_status = 0;
                    Ok(Some(RunResult::Success(true)))
                }
                "repl.mode" => {
                    if args.len() != 3 {
                        return Err("set: repl.mode expects vi|emacs".into());
                    }
                    let mode = args[2].as_str();
                    match mode {
                        "vi" => state.repl.vi_mode = true,
                        "emacs" => state.repl.vi_mode = false,
                        _ => return Err("set: repl.mode expects vi|emacs".into()),
                    }
                    state.repl.generation += 1;
                    state.last_status = 0;
                    Ok(Some(RunResult::Success(true)))
                }
                "repl.completion.command" => {
                    if args.len() < 3 {
                        return Err("set: repl.completion.command expects a value".into());
                    }
                    if args.len() == 3 && args[2] == "off" {
                        state.repl.completion_command.clear();
                    } else {
                        state.repl.completion_command = args[2..].to_vec();
                    }
                    state.repl.generation += 1;
                    state.last_status = 0;
                    Ok(Some(RunResult::Success(true)))
                }
                "repl.bind" => {
                    if args.len() < 4 {
                        return Err("set: repl.bind expects KEY ACTION".into());
                    }
                    let key = args[2].clone();
                    let action = args[3..].join(" ");
                    if action == "off" {
                        state.repl.bindings.retain(|binding| binding.key != key);
                    } else {
                        if let Some(existing) =
                            state.repl.bindings.iter_mut().find(|binding| binding.key == key)
                        {
                            existing.action = action;
                        } else {
                            state.repl.bindings.push(crate::state::ReplBinding { key, action });
                        }
                    }
                    state.repl.generation += 1;
                    state.last_status = 0;
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
            match ctx.execute_with_exit()? {
                FlowControl::Exit => return Ok(Some(RunResult::Exit)),
                FlowControl::Return(code) => return Ok(Some(RunResult::Return(code))),
                FlowControl::None => {}
            }
            Ok(Some(RunResult::Success(true)))
        }
        "source" => {
            if args.len() < 2 {
                return Err("source: expected a path".into());
            }
            let path = &args[1];
            let saved_positional = state.positional.clone();
            state.positional = args[2..].to_vec();
            let result = source_file(Path::new(path), state);
            state.positional = saved_positional;
            match result {
                Ok(()) => {
                    state.last_status = 0;
                    Ok(Some(RunResult::Success(true)))
                }
                Err(err) => Err(format!("source: failed to read '{path}': {err}")),
            }
        }
        "builtin" => {
            if args.len() < 2 {
                return Err("builtin: expected a command name".into());
            }
            let target = &args[1];
            if target == "builtin" {
                return Err("builtin: nested builtin is not allowed".into());
            }
            let mut inner = Vec::with_capacity(args.len() - 1);
            inner.push(target.clone());
            inner.extend_from_slice(&args[2..]);
            match run_builtin(&inner, state)? {
                Some(result) => Ok(Some(result)),
                None => Err(format!("builtin: unknown builtin '{target}'")),
            }
        }
        "return" => {
            if state.locals_stack.is_empty() {
                return Err("return: not in function".into());
            }
            if args.len() > 2 {
                return Err("return: too many arguments".into());
            }
            let code = if args.len() == 2 {
                args[1]
                    .parse::<i32>()
                    .map_err(|_| "return: status must be an integer".to_string())?
            } else {
                state.last_status
            };
            state.last_status = code;
            Ok(Some(RunResult::Return(code)))
        }
        _ => Ok(None),
    }
}

fn run_alias_builtin_expanded(
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
        state.last_status = 0;
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
    let value = args[arg_idx + 1..].join(" ");
    state.set_alias(alias_name, value, global);
    state.last_status = 0;
    Ok(Some(RunResult::Success(true)))
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

        let brace = parse_brace_block(trimmed);
        match brace {
            BraceParse::Inline { head, body, tail } if head.trim().is_empty() => {
                if let Some(tail) = tail {
                    return Err(format!("unexpected trailing text after block: {tail}"));
                }
                stages.push(PipelineStage::Block(InlineBlock {
                    block: body,
                    redirs: Redirections::default(),
                }));
                continue;
            }
            BraceParse::Open { head, .. } if head.trim().is_empty() => {
                return Err("inline block missing '}' in pipeline".into());
            }
            _ => {}
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

        let expanded = expand_tokens_with_meta(tokens, state)?;
        let words: Vec<WordToken> = expanded
            .into_iter()
            .map(|token| WordToken {
                value: token.value,
                protected: token.protected,
            })
            .collect();
        let raw_values: Vec<String> = words.iter().map(|t| t.value.clone()).collect();
        let (assignments, remaining) = split_assignments(&raw_values);
        let assignments_len = assignments.len();
        for (name, value) in assignments {
            state.set_var(&name, value);
        }
        if remaining.is_empty() {
            return Err("empty command in pipeline".into());
        }
        let remaining_tokens: Vec<WordToken> = words
            .into_iter()
            .skip(assignments_len)
            .collect();
        let (args, redirs) = parse_redirections(remaining_tokens)?;
        if args.is_empty() {
            return Err("empty command in pipeline".into());
        }
        if state.functions.contains_key(&args[0]) {
            stages.push(PipelineStage::Function(FunctionCall {
                name: args[0].clone(),
                args: args[1..].to_vec(),
                redirs,
            }));
            continue;
        }
        stages.push(PipelineStage::External(CommandSpec { args, redirs }));
    }

    Ok(stages)
}

fn run_pipeline(commands: Vec<CommandSpec>, state: &mut ShellState) -> Result<bool, String> {
    let mut children = Vec::new();
    let mut prev_read: Option<File> = None;
    let capture_output = capture_output_enabled(state);
    let mut capture_read: Option<File> = None;

    for (idx, spec) in commands.iter().enumerate() {
        let is_last = idx + 1 == commands.len();
        let mut cmd = Command::new(&spec.args[0]);
        if spec.args.len() > 1 {
            cmd.args(&spec.args[1..]);
        }

        if let Some(input_path) = &spec.redirs.stdin {
            let file = open_input_file(input_path)?;
            cmd.stdin(Stdio::from(file));
            prev_read = None;
        } else if let Some(prev) = prev_read.take() {
            cmd.stdin(Stdio::from(prev));
        }

        let (next_read, stdout_write) = if !is_last {
            let (read, write) = create_pipe()?;
            (Some(read), Some(write))
        } else if capture_output {
            let (read, write) = create_pipe()?;
            capture_read = Some(read);
            (None, Some(write))
        } else {
            (None, None)
        };

        let stdout_target = spec.redirs.stdout.as_ref();
        let stderr_target = spec.redirs.stderr.as_ref();
        let stdout_uses_original = matches!(stdout_target, None)
            || matches!(stdout_target, Some(redir) if matches!(redir.target, OutputTarget::Stdout));
        let stderr_uses_original =
            matches!(stderr_target, Some(redir) if matches!(redir.target, OutputTarget::Stdout));

        let original_stdout = if stdout_uses_original || stderr_uses_original {
            stdout_write.as_ref()
        } else {
            None
        };

        let mut stdout_set = false;
        let mut stderr_set = false;

        if let (Some(stdout_redir), Some(stderr_redir)) = (stdout_target, stderr_target) {
            if let (OutputTarget::File(stdout_path), OutputTarget::File(stderr_path)) =
                (&stdout_redir.target, &stderr_redir.target)
            {
                if stdout_path == stderr_path && stdout_redir.append == stderr_redir.append {
                    let file = open_output_file(stdout_path, stdout_redir.append)?;
                    let err_file = file.try_clone().map_err(|err| {
                        format!("failed to clone output file '{stdout_path}': {err}")
                    })?;
                    cmd.stdout(Stdio::from(file));
                    cmd.stderr(Stdio::from(err_file));
                    stdout_set = true;
                    stderr_set = true;
                }
            }
        }

        if !stdout_set {
            if let Some(redir) = stdout_target {
                let stdio = build_output_stdio(&redir.target, redir.append, original_stdout)?;
                cmd.stdout(stdio);
            } else if let Some(file) = original_stdout {
                cmd.stdout(Stdio::from(
                    file.try_clone()
                        .map_err(|err| format!("failed to clone stdout pipe: {err}"))?,
                ));
            }
        }

        if !stderr_set {
            if let Some(redir) = stderr_target {
                let stdio = build_output_stdio(&redir.target, redir.append, original_stdout)?;
                cmd.stderr(stdio);
            }
        }

        let child = cmd
            .spawn()
            .map_err(|err| format!("failed to execute '{}': {err}", spec.args[0]))?;

        drop(stdout_write);
        if let Some(read) = next_read {
            prev_read = Some(read);
        }

        children.push((spec.args[0].clone(), child));
    }

    if capture_output {
        let last_byte = if let Some(mut stdout) = capture_read {
            stream_reader_stdout(&mut stdout).map_err(|err| err.to_string())?
        } else {
            None
        };
        update_last_output(state, last_byte);
    } else if state.interactive {
        state.last_output_newline = true;
        state.needs_cursor_check = true;
    }

    let mut last_success = true;
    let mut last_status = 0;
    for (idx, (name, mut child)) in children.into_iter().enumerate() {
        let status = child
            .wait()
            .map_err(|err| format!("failed to wait for '{}': {err}", name))?;
        if idx + 1 == commands.len() {
            last_success = status.success();
            last_status = exit_status_code(&status);
        }
    }
    state.last_status = last_status;

    Ok(last_success)
}

enum PipelineStage {
    External(CommandSpec),
    Function(FunctionCall),
    Block(InlineBlock),
    Foreach {
        var: String,
        block: String,
        inline: bool,
    },
}

fn run_pipeline_stages(
    stages: Vec<PipelineStage>,
    state: &mut ShellState,
) -> Result<bool, String> {
    if stages.is_empty() {
        return Ok(true);
    }

    let worker_needed = stages.iter().any(|stage| {
        matches!(
            stage,
            PipelineStage::Foreach { .. }
                | PipelineStage::Function(_)
                | PipelineStage::Block(_)
        )
    });
    let (locals_path, locals_guard) = if worker_needed {
        let (path, guard) =
            write_locals_file(state).map_err(|err| format!("failed to write locals: {err}"))?;
        (Some(path), Some(guard))
    } else {
        (None, None)
    };

    let mut block_guards = Vec::new();
    let mut children = Vec::new();
    let mut prev_read: Option<File> = None;
    let capture_output = capture_output_enabled(state);
    let mut capture_read: Option<File> = None;

    for (idx, stage) in stages.iter().enumerate() {
        let is_last = idx + 1 == stages.len();
        let (mut cmd, redirs, name) = match stage {
            PipelineStage::External(spec) => {
                let mut cmd = Command::new(&spec.args[0]);
                if spec.args.len() > 1 {
                    cmd.args(&spec.args[1..]);
                }
                (cmd, spec.redirs.clone(), spec.args[0].clone())
            }
            PipelineStage::Function(spec) => {
                let locals = locals_path
                    .as_ref()
                    .ok_or_else(|| "missing locals path for function".to_string())?;
                let exe = env::current_exe()
                    .map_err(|err| format!("failed to resolve ush path: {err}"))?;
                let mut cmd = Command::new(exe);
                cmd.arg("--function-worker")
                    .arg("--locals")
                    .arg(locals)
                    .arg("--name")
                    .arg(&spec.name)
                    .arg("--args")
                    .args(&spec.args);
                (cmd, spec.redirs.clone(), spec.name.clone())
            }
            PipelineStage::Block(spec) => {
                let locals = locals_path
                    .as_ref()
                    .ok_or_else(|| "missing locals path for block".to_string())?;
                let (block_path, guard) = write_block_file(&spec.block)
                    .map_err(|err| format!("failed to write pipeline block: {err}"))?;
                block_guards.push(guard);
                let exe = env::current_exe()
                    .map_err(|err| format!("failed to resolve ush path: {err}"))?;
                let mut cmd = Command::new(exe);
                cmd.arg("--block-worker")
                    .arg("--block")
                    .arg(block_path)
                    .arg("--locals")
                    .arg(locals);
                (cmd, spec.redirs.clone(), "block".to_string())
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
                (cmd, Redirections::default(), "foreach".to_string())
            }
        };

        if let Some(input_path) = &redirs.stdin {
            let file = open_input_file(input_path)?;
            cmd.stdin(Stdio::from(file));
            prev_read = None;
        } else if let Some(prev) = prev_read.take() {
            cmd.stdin(Stdio::from(prev));
        }

        let (next_read, stdout_write) = if !is_last {
            let (read, write) = create_pipe()?;
            (Some(read), Some(write))
        } else if capture_output {
            let (read, write) = create_pipe()?;
            capture_read = Some(read);
            (None, Some(write))
        } else {
            (None, None)
        };

        let stdout_target = redirs.stdout.as_ref();
        let stderr_target = redirs.stderr.as_ref();
        let stdout_uses_original = matches!(stdout_target, None)
            || matches!(stdout_target, Some(redir) if matches!(redir.target, OutputTarget::Stdout));
        let stderr_uses_original =
            matches!(stderr_target, Some(redir) if matches!(redir.target, OutputTarget::Stdout));

        let original_stdout = if stdout_uses_original || stderr_uses_original {
            stdout_write.as_ref()
        } else {
            None
        };

        let mut stdout_set = false;
        let mut stderr_set = false;

        if let (Some(stdout_redir), Some(stderr_redir)) = (stdout_target, stderr_target) {
            if let (OutputTarget::File(stdout_path), OutputTarget::File(stderr_path)) =
                (&stdout_redir.target, &stderr_redir.target)
            {
                if stdout_path == stderr_path && stdout_redir.append == stderr_redir.append {
                    let file = open_output_file(stdout_path, stdout_redir.append)?;
                    let err_file = file.try_clone().map_err(|err| {
                        format!("failed to clone output file '{stdout_path}': {err}")
                    })?;
                    cmd.stdout(Stdio::from(file));
                    cmd.stderr(Stdio::from(err_file));
                    stdout_set = true;
                    stderr_set = true;
                }
            }
        }

        if !stdout_set {
            if let Some(redir) = stdout_target {
                let stdio = build_output_stdio(&redir.target, redir.append, original_stdout)?;
                cmd.stdout(stdio);
            } else if let Some(file) = original_stdout {
                cmd.stdout(Stdio::from(
                    file.try_clone()
                        .map_err(|err| format!("failed to clone stdout pipe: {err}"))?,
                ));
            }
        }

        if !stderr_set {
            if let Some(redir) = stderr_target {
                let stdio = build_output_stdio(&redir.target, redir.append, original_stdout)?;
                cmd.stderr(stdio);
            }
        }

        let child = cmd
            .spawn()
            .map_err(|err| format!("failed to execute pipeline stage: {err}"))?;

        drop(stdout_write);
        if let Some(read) = next_read {
            prev_read = Some(read);
        }

        children.push((name, child));
    }

    if capture_output {
        let last_byte = if let Some(mut stdout) = capture_read {
            stream_reader_stdout(&mut stdout).map_err(|err| err.to_string())?
        } else {
            None
        };
        update_last_output(state, last_byte);
    } else if state.interactive {
        state.last_output_newline = true;
        state.needs_cursor_check = true;
    }

    let mut last_success = true;
    let mut last_status = 0;
    for (idx, (name, mut child)) in children.into_iter().enumerate() {
        let status = child
            .wait()
            .map_err(|err| format!("failed to wait for '{}': {err}", name))?;
        if idx + 1 == stages.len() {
            last_success = status.success();
            last_status = exit_status_code(&status);
        }
    }

    drop(block_guards);
    drop(locals_guard);

    state.last_status = last_status;
    Ok(last_success)
}

fn with_redirections<T>(redirs: &Redirections, f: impl FnOnce() -> Result<T, String>) -> Result<T, String> {
    if redirs.stdin.is_none() && redirs.stdout.is_none() && redirs.stderr.is_none() {
        return f();
    }
    let _guard = RedirectionGuard::apply(redirs)?;
    f()
}

struct RedirectionGuard {
    stdin_fd: i32,
    stdout_fd: i32,
    stderr_fd: i32,
}

impl RedirectionGuard {
    fn apply(redirs: &Redirections) -> Result<Self, String> {
        let stdin_fd = dup_fd(libc::STDIN_FILENO)?;
        let stdout_fd = dup_fd(libc::STDOUT_FILENO)?;
        let stderr_fd = dup_fd(libc::STDERR_FILENO)?;

        if let Some(path) = &redirs.stdin {
            let file = open_input_file(path)?;
            dup2_fd(file.as_raw_fd(), libc::STDIN_FILENO)?;
        }

        if let Some(redir) = &redirs.stdout {
            apply_output_dup(redir, stdout_fd, stderr_fd, libc::STDOUT_FILENO)?;
        }

        if let Some(redir) = &redirs.stderr {
            apply_output_dup(redir, stdout_fd, stderr_fd, libc::STDERR_FILENO)?;
        }

        Ok(Self {
            stdin_fd,
            stdout_fd,
            stderr_fd,
        })
    }
}

impl Drop for RedirectionGuard {
    fn drop(&mut self) {
        let _ = unsafe { libc::dup2(self.stdin_fd, libc::STDIN_FILENO) };
        let _ = unsafe { libc::dup2(self.stdout_fd, libc::STDOUT_FILENO) };
        let _ = unsafe { libc::dup2(self.stderr_fd, libc::STDERR_FILENO) };
        let _ = unsafe { libc::close(self.stdin_fd) };
        let _ = unsafe { libc::close(self.stdout_fd) };
        let _ = unsafe { libc::close(self.stderr_fd) };
    }
}

fn apply_output_dup(
    redir: &OutputRedir,
    saved_stdout: i32,
    saved_stderr: i32,
    target_fd: i32,
) -> Result<(), String> {
    match &redir.target {
        OutputTarget::Stdout => dup2_fd(saved_stdout, target_fd),
        OutputTarget::Stderr => dup2_fd(saved_stderr, target_fd),
        OutputTarget::Null => {
            let file = open_output_file("/dev/null", redir.append)?;
            dup2_fd(file.as_raw_fd(), target_fd)
        }
        OutputTarget::File(path) => {
            let file = open_output_file(path, redir.append)?;
            dup2_fd(file.as_raw_fd(), target_fd)
        }
    }
}

fn build_output_stdio(
    target: &OutputTarget,
    append: bool,
    original_stdout: Option<&File>,
) -> Result<Stdio, String> {
    match target {
        OutputTarget::File(path) => Ok(Stdio::from(open_output_file(path, append)?)),
        OutputTarget::Null => Ok(Stdio::from(open_output_file("/dev/null", append)?)),
        OutputTarget::Stdout => {
            if let Some(file) = original_stdout {
                Ok(Stdio::from(file.try_clone().map_err(|err| {
                    format!("failed to clone stdout pipe: {err}")
                })?))
            } else {
                Ok(Stdio::from(file_from_fd(libc::STDOUT_FILENO)?))
            }
        }
        OutputTarget::Stderr => Ok(Stdio::from(file_from_fd(libc::STDERR_FILENO)?)),
    }
}

fn open_output_file(path: &str, append: bool) -> Result<File, String> {
    let mut opts = OpenOptions::new();
    opts.write(true).create(true);
    if append {
        opts.append(true);
    } else {
        opts.truncate(true);
    }
    opts.open(path)
        .map_err(|err| format!("failed to open '{path}': {err}"))
}

fn open_input_file(path: &str) -> Result<File, String> {
    File::open(path).map_err(|err| format!("failed to open '{path}': {err}"))
}

fn file_from_fd(fd: i32) -> Result<File, String> {
    let duped = dup_fd(fd)?;
    Ok(unsafe { File::from_raw_fd(duped) })
}

fn create_pipe() -> Result<(File, File), String> {
    let mut fds = [0; 2];
    let result = unsafe { libc::pipe(fds.as_mut_ptr()) };
    if result != 0 {
        return Err(format!("failed to create pipe: {}", io::Error::last_os_error()));
    }
    let read = unsafe { File::from_raw_fd(fds[0]) };
    let write = unsafe { File::from_raw_fd(fds[1]) };
    Ok((read, write))
}

fn dup_fd(fd: i32) -> Result<i32, String> {
    let result = unsafe { libc::dup(fd) };
    if result < 0 {
        return Err(format!("failed to dup fd {fd}: {}", io::Error::last_os_error()));
    }
    Ok(result)
}

fn dup2_fd(src: i32, dst: i32) -> Result<(), String> {
    let result = unsafe { libc::dup2(src, dst) };
    if result < 0 {
        return Err(format!("failed to dup2 fd {src} -> {dst}: {}", io::Error::last_os_error()));
    }
    Ok(())
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

pub(crate) fn execute_inline_block(
    block: &str,
    state: &mut ShellState,
) -> Result<FlowControl, String> {
    let trimmed = block.trim();
    if trimmed.is_empty() {
        return Ok(FlowControl::None);
    }
    match process_line_raw(trimmed, state) {
        FlowControl::Exit => Err("exit not allowed in inline block".into()),
        FlowControl::Return(code) => Ok(FlowControl::Return(code)),
        FlowControl::None => Ok(FlowControl::None),
    }
}

pub struct ScriptContext<'a> {
    pub lines: Vec<String>,
    pub state: &'a mut ShellState,
}

struct BlockResult {
    next: usize,
    flow: FlowControl,
}

impl<'a> ScriptContext<'a> {
    pub fn execute(&mut self) -> Result<(), String> {
        let _ = self.execute_with_exit()?;
        Ok(())
    }

    pub(crate) fn execute_with_exit(&mut self) -> Result<FlowControl, String> {
        let mut idx = 0;
        while idx < self.lines.len() {
            let result = self.execute_block(idx, 0, true)?;
            idx = result.next;
            if result.flow != FlowControl::None {
                return Ok(result.flow);
            }
        }
        Ok(FlowControl::None)
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
                || expanded.starts_with("def ")
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
                        if let FlowControl::Return(code) =
                            execute_inline_block(&block, self.state)?
                        {
                            return Ok(BlockResult {
                                next: idx + 1,
                                flow: FlowControl::Return(code),
                            });
                        }
                    }
                    let (exit, next_idx, handled) = self.handle_else_chain_tail(
                        brace_inline_tail,
                        idx + 1,
                        indent_level,
                        should_execute && !run_child,
                    )?;
                    idx = next_idx;
                    if exit != FlowControl::None {
                        return Ok(BlockResult {
                            next: idx,
                            flow: exit,
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
                        match ctx.execute_with_exit()? {
                            FlowControl::Exit => {
                                return Ok(BlockResult {
                                    next: next_idx,
                                    flow: FlowControl::Exit,
                                });
                            }
                            FlowControl::Return(code) => {
                                return Ok(BlockResult {
                                    next: next_idx,
                                    flow: FlowControl::Return(code),
                                });
                            }
                            FlowControl::None => {}
                        }
                    } else if handled {
                        // else/elif chain executed instead
                    }
                    idx = next_idx;
                    if exit != FlowControl::None {
                        return Ok(BlockResult {
                            next: idx,
                            flow: exit,
                        });
                    }
                    continue;
                }

                idx += 1;
                let result =
                    self.execute_block(idx, indent_level + 1, should_execute && run_child)?;
                idx = result.next;

                if result.flow != FlowControl::None {
                    return Ok(result);
                }

                let (exit, next_idx, handled) =
                    self.handle_else_chain(idx, indent_level, should_execute && !run_child)?;
                idx = next_idx;
                if exit != FlowControl::None {
                    return Ok(BlockResult {
                        next: idx,
                        flow: exit,
                    });
                }
                if handled {
                    continue;
                }
                continue;
            }

            if let Some(rest) = trimmed.strip_prefix("def ") {
                let brace = parse_brace_block(rest);
                let (head, brace_block, brace_open, brace_tail) = match brace {
                    BraceParse::Inline { head, body, tail } => (head, Some(body), false, tail),
                    BraceParse::Open { head, tail } => (head, None, true, Some(tail)),
                    BraceParse::None { head } => (head, None, false, None),
                };
                let args = parse_args(&head)?;
                if args.len() != 1 {
                    return Err(format!("def expects a single name on line {}", idx + 1));
                }
                let name = &args[0];
                if !is_valid_var_name(name) {
                    return Err(format!("invalid function name '{}' on line {}", name, idx + 1));
                }
                if let Some(tail) = brace_tail.as_deref() {
                    if !tail.trim().is_empty() {
                        return Err(format!(
                            "unexpected trailing text after def block: {tail}"
                        ));
                    }
                }
                if let Some(block) = brace_block {
                    if should_execute {
                        self.state.functions.insert(
                            name.to_string(),
                            FunctionDef {
                                body: FunctionBody::Inline(block),
                            },
                        );
                    }
                    idx += 1;
                    continue;
                }

                if brace_open {
                    let (block_lines, end_idx, tail_line) =
                        collect_brace_block(&self.lines, idx + 1, brace_tail)?;
                    if let Some(tail) = tail_line {
                        return Err(format!(
                            "unexpected trailing text after def block: {tail}"
                        ));
                    }
                    if should_execute {
                        self.state.functions.insert(
                            name.to_string(),
                            FunctionDef {
                                body: FunctionBody::Block(block_lines),
                            },
                        );
                    }
                    idx = end_idx + 1;
                    continue;
                }

                idx += 1;
                let block_start = idx;
                let block_end = self.find_block_end(block_start, indent_level + 1);
                if should_execute {
                    let raw_lines = self.lines[block_start..block_end].to_vec();
                    let block_lines = unindent_block_lines_by(&raw_lines, indent_level + 1);
                    self.state.functions.insert(
                        name.to_string(),
                        FunctionDef {
                            body: FunctionBody::Block(block_lines),
                        },
                    );
                }
                idx = block_end;
                continue;
            }

            let brace = parse_brace_block(&trimmed);
            match brace {
                BraceParse::Inline { head, body, tail } => {
                    if head.trim().is_empty() {
                        if should_execute {
                            if let Some(tail) = tail {
                                let mut stages = Vec::new();
                                stages.push(PipelineStage::Block(InlineBlock {
                                    block: body,
                                    redirs: Redirections::default(),
                                }));
                                let mut after = Vec::new();
                                append_pipeline_tail(&mut after, &tail)?;
                                let mut after_stages =
                                    build_pipeline_stages_from_segments(&after, self.state)?;
                                stages.append(&mut after_stages);
                                run_pipeline_stages(stages, self.state)?;
                            } else if let FlowControl::Return(code) =
                                execute_inline_block(&body, self.state)?
                            {
                                return Ok(BlockResult {
                                    next: idx + 1,
                                    flow: FlowControl::Return(code),
                                });
                            }
                        }
                        idx += 1;
                        continue;
                    }
                }
                BraceParse::Open { head, tail } => {
                    let head_trim = head.trim_end();
                if head_trim.is_empty() || head_trim.ends_with('|') {
                    let (block_lines, end_idx, tail_line) =
                        collect_brace_block(&self.lines, idx + 1, Some(tail))?;
                    let block = block_lines.join("\n");
                    let mut after = Vec::new();
                    if let Some(tail_line) = tail_line.as_deref() {
                        append_pipeline_tail(&mut after, tail_line)?;
                    }
                    if should_execute {
                        if head_trim.is_empty() && after.is_empty() {
                            let mut ctx = ScriptContext {
                                lines: block_lines,
                                state: self.state,
                            };
                            match ctx.execute_with_exit()? {
                                FlowControl::Exit => {
                                    return Ok(BlockResult {
                                        next: end_idx + 1,
                                        flow: FlowControl::Exit,
                                    });
                                }
                                FlowControl::Return(code) => {
                                    return Ok(BlockResult {
                                        next: end_idx + 1,
                                        flow: FlowControl::Return(code),
                                    });
                                }
                                FlowControl::None => {}
                            }
                        } else {
                            let mut stages = Vec::new();
                            let head_before = head_trim
                                .strip_suffix('|')
                                .unwrap_or(head_trim)
                                .trim_end();
                            if !head_before.is_empty() {
                                let before_segments = split_on_pipes(head_before)
                                    .into_iter()
                                    .map(|seg| seg.trim().to_string())
                                    .filter(|seg| !seg.is_empty())
                                    .collect::<Vec<_>>();
                                let mut before_stages =
                                    build_pipeline_stages_from_segments(&before_segments, self.state)?;
                                stages.append(&mut before_stages);
                            }
                            stages.push(PipelineStage::Block(InlineBlock {
                                block,
                                redirs: Redirections::default(),
                            }));
                            let mut after_stages =
                                build_pipeline_stages_from_segments(&after, self.state)?;
                            stages.append(&mut after_stages);
                            run_pipeline_stages(stages, self.state)?;
                        }
                    }
                    idx = end_idx + 1;
                    continue;
                }
                }
                BraceParse::None { .. } => {}
            }

            let foreach_tokens = if trimmed.contains("...") {
                let tokens = expand_line_tokens_spread_only(&trimmed, self.state)?;
                parse_foreach_tokens(&tokens)
            } else {
                None
            };

            if let Some(foreach) = foreach_tokens {
                if !is_valid_var_name(&foreach.var_name) {
                    return Err(format!(
                        "invalid foreach variable '{}' on line {}",
                        foreach.var_name,
                        idx + 1
                    ));
                }

                let mut after_segments = foreach.after;

                if let Some(block) = foreach.brace_block {
                    if should_execute {
                        let mut stages = Vec::new();
                        let mut before_stages =
                            build_pipeline_stages_from_token_segments(&foreach.before, self.state)?;
                        stages.append(&mut before_stages);
                        stages.push(PipelineStage::Foreach {
                            var: foreach.var_name.clone(),
                            block,
                            inline: true,
                        });
                        let after_stages =
                            build_pipeline_stages_from_token_segments(&after_segments, self.state)?;
                        stages.extend(after_stages);
                        run_pipeline_stages(stages, self.state)?;
                    }
                    idx += 1;
                    continue;
                }

                if foreach.brace_open {
                    let (block_lines, end_idx, tail_line) =
                        collect_brace_block(&self.lines, idx + 1, foreach.brace_tail)?;
                    if let Some(tail) = tail_line.as_deref() {
                        let tail_tokens = expand_line_tokens_spread_only(tail, self.state)?;
                        let mut tail_segments = split_tokens_on_pipes(&tail_tokens)?;
                        let mut tail_strings: Vec<Vec<String>> = tail_segments
                            .drain(..)
                            .map(|seg| seg.into_iter().map(|t| t.value).collect())
                            .collect();
                        after_segments.append(&mut tail_strings);
                    }
                    if should_execute {
                        let mut stages = Vec::new();
                        let mut before_stages =
                            build_pipeline_stages_from_token_segments(&foreach.before, self.state)?;
                        stages.append(&mut before_stages);
                        stages.push(PipelineStage::Foreach {
                            var: foreach.var_name.clone(),
                            block: block_lines.join("\n"),
                            inline: false,
                        });
                        let after_stages =
                            build_pipeline_stages_from_token_segments(&after_segments, self.state)?;
                        stages.extend(after_stages);
                        run_pipeline_stages(stages, self.state)?;
                    }
                    idx = end_idx + 1;
                    continue;
                }

                idx += 1;
                let block_start = idx;
                let block_end = self.find_block_end(block_start, indent_level + 1);
                if !after_segments.is_empty() {
                    return Err("foreach blocks without braces cannot be piped onward".into());
                }

                if should_execute {
                    let mut stages = Vec::new();
                        let mut before_stages =
                            build_pipeline_stages_from_token_segments(&foreach.before, self.state)?;
                        stages.append(&mut before_stages);
                    let raw_lines = self.lines[block_start..block_end].to_vec();
                    let block_lines = unindent_block_lines(&raw_lines);
                    stages.push(PipelineStage::Foreach {
                        var: foreach.var_name.clone(),
                        block: block_lines.join("\n"),
                        inline: false,
                    });
                    run_pipeline_stages(stages, self.state)?;
                }

                idx = block_end;
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
                        let mut before_stages = build_pipeline_stages_from_segments(&before, self.state)?;
                        stages.append(&mut before_stages);
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
                        let mut before_stages = build_pipeline_stages_from_segments(&before, self.state)?;
                        stages.append(&mut before_stages);
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
                    let mut before_stages = build_pipeline_stages_from_segments(&before, self.state)?;
                    stages.append(&mut before_stages);
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
                            if let FlowControl::Return(code) =
                                execute_inline_block(&block, self.state)?
                            {
                                return Ok(BlockResult {
                                    next: idx + 1,
                                    flow: FlowControl::Return(code),
                                });
                            }
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
                            match ctx.execute_with_exit()? {
                                FlowControl::Exit => {
                                    return Ok(BlockResult {
                                        next: end_idx + 1,
                                        flow: FlowControl::Exit,
                                    });
                                }
                                FlowControl::Return(code) => {
                                    return Ok(BlockResult {
                                        next: end_idx + 1,
                                        flow: FlowControl::Return(code),
                                    });
                                }
                                FlowControl::None => {}
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
                        if result.flow != FlowControl::None {
                            return Ok(result);
                        }
                    }
                }

                idx = block_end;
                continue;
            }

            if should_execute {
                match process_line_raw(&trimmed, &mut self.state) {
                    FlowControl::Exit => {
                        return Ok(BlockResult {
                            next: idx + 1,
                            flow: FlowControl::Exit,
                        });
                    }
                    FlowControl::Return(code) => {
                        return Ok(BlockResult {
                            next: idx + 1,
                            flow: FlowControl::Return(code),
                        });
                    }
                    FlowControl::None => {}
                }
            }

            idx += 1;
        }

        Ok(BlockResult {
            next: idx,
            flow: FlowControl::None,
        })
    }

    fn handle_else_chain(
        &mut self,
        mut idx: usize,
        indent_level: usize,
        should_execute: bool,
    ) -> Result<(FlowControl, usize, bool), String> {
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

        Ok((FlowControl::None, idx, handled))
    }

    fn handle_else_chain_tail(
        &mut self,
        tail_line: Option<String>,
        idx: usize,
        indent_level: usize,
        should_execute: bool,
    ) -> Result<(FlowControl, usize, bool), String> {
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
    ) -> Result<(FlowControl, usize, bool), String> {
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
                if let FlowControl::Return(code) =
                    execute_inline_block(&block, self.state)?
                {
                    return Ok((FlowControl::Return(code), block_start + 1, true));
                }
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
                match ctx.execute_with_exit()? {
                    FlowControl::Exit => return Ok((FlowControl::Exit, end_idx + 1, true)),
                    FlowControl::Return(code) => {
                        return Ok((FlowControl::Return(code), end_idx + 1, true))
                    }
                    FlowControl::None => {}
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
        if result.flow == FlowControl::Exit {
            return Ok((FlowControl::Exit, result.next, true));
        }
        if let FlowControl::Return(code) = result.flow {
            return Ok((FlowControl::Return(code), result.next, true));
        }
        if run_child {
            let (exit, next_idx, _) = self.handle_else_chain(result.next, indent_level, false)?;
            return Ok((exit, next_idx, true));
        }
        self.handle_else_chain(result.next, indent_level, should_execute && !run_child)
    }

    fn evaluate_condition(&mut self, command: &str) -> Result<bool, String> {
        let args = parse_args(command)?;
        if args.is_empty() {
            return Ok(false);
        }
        let args = apply_alias(args, &self.state)?;
        let expanded = expand_tokens_with_meta(args, &self.state)?;
        let words: Vec<WordToken> = expanded
            .into_iter()
            .map(|token| WordToken {
                value: token.value,
                protected: token.protected,
            })
            .collect();
        let (cmd_args, redirs) = parse_redirections(words)?;
        if cmd_args.is_empty() {
            return Ok(false);
        }
        let success = run_pipeline(
            vec![CommandSpec {
                args: cmd_args,
                redirs,
            }],
            &mut self.state,
        )?;
        Ok(success)
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

#[cfg(not(feature = "repl"))]
fn print_prompt(stdout: &mut io::Stdout) -> io::Result<()> {
    stdout.write_all(b"unshell> ")?;
    stdout.flush()
}

fn stream_reader_stdout<R: Read>(stdout: &mut R) -> io::Result<Option<u8>> {
    let mut buf = [0u8; 8192];
    let mut last = None;
    let mut out = io::stdout();
    loop {
        let count = stdout.read(&mut buf)?;
        if count == 0 {
            break;
        }
        out.write_all(&buf[..count])?;
        last = Some(buf[count - 1]);
    }
    out.flush()?;
    Ok(last)
}

fn update_last_output(state: &mut ShellState, last_byte: Option<u8>) {
    if let Some(byte) = last_byte {
        state.last_output_newline = byte == b'\n';
    } else {
        state.last_output_newline = true;
    }
}

fn capture_output_enabled(state: &ShellState) -> bool {
    state.interactive && !io::stdout().is_terminal()
}

fn exit_status_code(status: &ExitStatus) -> i32 {
    status.code().unwrap_or(1)
}
