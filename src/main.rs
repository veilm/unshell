use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Output, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

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

struct ForeachWorkerArgs {
    var: String,
    block_path: PathBuf,
    locals_path: PathBuf,
    inline: bool,
}

struct CaptureWorkerArgs {
    script_path: PathBuf,
    locals_path: PathBuf,
}

fn run_foreach_worker(args: &[String]) -> Result<(), String> {
    let opts = parse_foreach_worker_args(args)?;
    let mut state = read_locals_file(&opts.locals_path)
        .map_err(|err| format!("failed to load foreach locals: {err}"))?;
    let block = read_block_file(&opts.block_path)
        .map_err(|err| format!("failed to load foreach block: {err}"))?;

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let reader = BufReader::new(stdin.lock());
    let mut exit_requested = false;

    for line in reader.lines() {
        let mut line = line.map_err(|err| format!("failed to read foreach input: {err}"))?;
        trim_trailing_newline(&mut line);
        state.set_var(&opts.var, line);
        if opts.inline {
            execute_inline_block(&block, &mut state)?;
        } else {
            let mut ctx = ScriptContext {
                lines: block.lines().map(|s| s.to_string()).collect(),
                state: &mut state,
            };
            if ctx.execute_with_exit()? {
                exit_requested = true;
                break;
            }
        }
    }

    stdout.flush().map_err(|err| format!("failed to flush foreach output: {err}"))?;

    if exit_requested {
        std::process::exit(0);
    }

    Ok(())
}

fn run_capture_worker(args: &[String]) -> Result<(), String> {
    let opts = parse_capture_worker_args(args)?;
    let mut state = read_locals_file(&opts.locals_path)
        .map_err(|err| format!("failed to load capture locals: {err}"))?;
    let script = read_block_file(&opts.script_path)
        .map_err(|err| format!("failed to load capture script: {err}"))?;

    let mut ctx = ScriptContext {
        lines: vec![script],
        state: &mut state,
    };
    if ctx.execute_with_exit()? {
        return Err("exit not allowed in capture".into());
    }
    Ok(())
}

fn parse_foreach_worker_args(args: &[String]) -> Result<ForeachWorkerArgs, String> {
    let mut var = None;
    let mut block_path = None;
    let mut locals_path = None;
    let mut inline = false;
    let mut idx = 0;

    while idx < args.len() {
        match args[idx].as_str() {
            "--var" => {
                idx += 1;
                var = args.get(idx).cloned();
            }
            "--block" => {
                idx += 1;
                block_path = args.get(idx).map(PathBuf::from);
            }
            "--locals" => {
                idx += 1;
                locals_path = args.get(idx).map(PathBuf::from);
            }
            "--inline" => {
                inline = true;
            }
            other => {
                return Err(format!("unknown foreach worker flag '{other}'"));
            }
        }
        idx += 1;
    }

    Ok(ForeachWorkerArgs {
        var: var.ok_or_else(|| "foreach worker missing --var".to_string())?,
        block_path: block_path.ok_or_else(|| "foreach worker missing --block".to_string())?,
        locals_path: locals_path.ok_or_else(|| "foreach worker missing --locals".to_string())?,
        inline,
    })
}

fn parse_capture_worker_args(args: &[String]) -> Result<CaptureWorkerArgs, String> {
    let mut script_path = None;
    let mut locals_path = None;
    let mut idx = 0;

    while idx < args.len() {
        match args[idx].as_str() {
            "--script" => {
                idx += 1;
                script_path = args.get(idx).map(PathBuf::from);
            }
            "--locals" => {
                idx += 1;
                locals_path = args.get(idx).map(PathBuf::from);
            }
            other => {
                return Err(format!("unknown capture worker flag '{other}'"));
            }
        }
        idx += 1;
    }

    Ok(CaptureWorkerArgs {
        script_path: script_path
            .ok_or_else(|| "capture worker missing --script".to_string())?,
        locals_path: locals_path
            .ok_or_else(|| "capture worker missing --locals".to_string())?,
    })
}

fn read_block_file(path: &Path) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn trim_trailing_newline(text: &mut String) {
    if text.ends_with('\n') {
        text.pop();
        if text.ends_with('\r') {
            text.pop();
        }
    }
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

        let and_chain = split_on_and(sequence);
        let mut last_success = true;
        for segment in and_chain {
            let segment = segment.trim();
            if segment.is_empty() {
                continue;
            }
            if !last_success {
                continue;
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

struct ShellOptions {
    aliases_recursive: bool,
    subshells_trim_newline: bool,
    expansions_chars: HashSet<char>,
    expansions_handler: Vec<String>,
}

struct AliasEntry {
    value: String,
    global: bool,
}

struct ShellState {
    vars: HashMap<String, String>,
    aliases: HashMap<String, AliasEntry>,
    options: ShellOptions,
}

impl ShellState {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            aliases: HashMap::new(),
            options: ShellOptions {
                aliases_recursive: true,
                subshells_trim_newline: true,
                expansions_chars: HashSet::new(),
                expansions_handler: Vec::new(),
            },
        }
    }

    fn set_var(&mut self, name: &str, value: String) {
        self.vars.insert(name.to_string(), value);
    }

    fn set_alias(&mut self, name: &str, value: String, global: bool) {
        self.aliases.insert(
            name.to_string(),
            AliasEntry {
                value,
                global,
            },
        );
    }

    fn remove_alias(&mut self, name: &str) -> bool {
        self.aliases.remove(name).is_some()
    }
}

struct TempFileGuard {
    path: PathBuf,
}

impl TempFileGuard {
    fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

impl Drop for TempFileGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

fn create_temp_file(prefix: &str) -> io::Result<(PathBuf, File)> {
    let dir = env::temp_dir();
    let pid = std::process::id();
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();

    for attempt in 0..1000 {
        let name = format!("ush-{prefix}-{pid}-{now}-{attempt}");
        let path = dir.join(name);
        match OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
        {
            Ok(file) => return Ok((path, file)),
            Err(err) if err.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(err) => return Err(err),
        }
    }

    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "failed to create temp file",
    ))
}

fn write_u32<W: Write>(writer: &mut W, value: u32) -> io::Result<()> {
    writer.write_all(&value.to_le_bytes())
}

fn read_u32<R: Read>(reader: &mut R) -> io::Result<u32> {
    let mut buf = [0u8; 4];
    reader.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

fn write_bool<W: Write>(writer: &mut W, value: bool) -> io::Result<()> {
    writer.write_all(&[if value { 1 } else { 0 }])
}

fn read_bool<R: Read>(reader: &mut R) -> io::Result<bool> {
    let mut buf = [0u8; 1];
    reader.read_exact(&mut buf)?;
    Ok(buf[0] != 0)
}

fn write_locals_file(state: &ShellState) -> io::Result<(PathBuf, TempFileGuard)> {
    let (path, mut file) = create_temp_file("locals")?;
    let count = state.vars.len() as u32;
    write_u32(&mut file, count)?;
    for (name, value) in state.vars.iter() {
        write_u32(&mut file, name.len() as u32)?;
        file.write_all(name.as_bytes())?;
        write_u32(&mut file, value.len() as u32)?;
        file.write_all(value.as_bytes())?;
    }
    let alias_count = state.aliases.len() as u32;
    write_u32(&mut file, alias_count)?;
    for (name, value) in state.aliases.iter() {
        write_u32(&mut file, name.len() as u32)?;
        file.write_all(name.as_bytes())?;
        write_u32(&mut file, value.value.len() as u32)?;
        file.write_all(value.value.as_bytes())?;
        write_bool(&mut file, value.global)?;
    }
    write_bool(&mut file, state.options.aliases_recursive)?;
    write_bool(&mut file, state.options.subshells_trim_newline)?;
    let mut chars: Vec<char> = state.options.expansions_chars.iter().copied().collect();
    chars.sort_unstable();
    let chars_string: String = chars.into_iter().collect();
    write_u32(&mut file, chars_string.len() as u32)?;
    file.write_all(chars_string.as_bytes())?;
    write_u32(&mut file, state.options.expansions_handler.len() as u32)?;
    for value in state.options.expansions_handler.iter() {
        write_u32(&mut file, value.len() as u32)?;
        file.write_all(value.as_bytes())?;
    }
    file.flush()?;
    Ok((path.clone(), TempFileGuard::new(path)))
}

fn read_locals_file(path: &Path) -> io::Result<ShellState> {
    let mut file = File::open(path)?;
    let count = read_u32(&mut file)?;
    let mut state = ShellState::new();
    for _ in 0..count {
        let name_len = read_u32(&mut file)? as usize;
        let mut name_buf = vec![0u8; name_len];
        file.read_exact(&mut name_buf)?;
        let value_len = read_u32(&mut file)? as usize;
        let mut value_buf = vec![0u8; value_len];
        file.read_exact(&mut value_buf)?;
        let name = String::from_utf8(name_buf)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "locals name not utf-8"))?;
        let value = String::from_utf8(value_buf)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "locals value not utf-8"))?;
        state.set_var(&name, value);
    }
    let alias_count = read_u32(&mut file)?;
    for _ in 0..alias_count {
        let name_len = read_u32(&mut file)? as usize;
        let mut name_buf = vec![0u8; name_len];
        file.read_exact(&mut name_buf)?;
        let value_len = read_u32(&mut file)? as usize;
        let mut value_buf = vec![0u8; value_len];
        file.read_exact(&mut value_buf)?;
        let global = read_bool(&mut file)?;
        let name = String::from_utf8(name_buf)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "alias name not utf-8"))?;
        let value = String::from_utf8(value_buf)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "alias value not utf-8"))?;
        state.set_alias(&name, value, global);
    }
    state.options.aliases_recursive = read_bool(&mut file)?;
    state.options.subshells_trim_newline = read_bool(&mut file)?;
    let chars_len = read_u32(&mut file)? as usize;
    let mut chars_buf = vec![0u8; chars_len];
    file.read_exact(&mut chars_buf)?;
    let chars = String::from_utf8(chars_buf)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "expansion chars not utf-8"))?;
    state.options.expansions_chars = chars.chars().collect();
    let handler_len = read_u32(&mut file)? as usize;
    state.options.expansions_handler = Vec::with_capacity(handler_len);
    for _ in 0..handler_len {
        let value_len = read_u32(&mut file)? as usize;
        let mut value_buf = vec![0u8; value_len];
        file.read_exact(&mut value_buf)?;
        let value = String::from_utf8(value_buf).map_err(|_| {
            io::Error::new(io::ErrorKind::InvalidData, "expansion handler not utf-8")
        })?;
        state.options.expansions_handler.push(value);
    }
    Ok(state)
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

fn write_block_file(block: &str) -> io::Result<(PathBuf, TempFileGuard)> {
    let (path, mut file) = create_temp_file("block")?;
    file.write_all(block.as_bytes())?;
    file.flush()?;
    Ok((path.clone(), TempFileGuard::new(path)))
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

fn split_on_semicolons(line: &str) -> Vec<String> {
    split_by_char(line, ';')
}

fn split_on_pipes(line: &str) -> Vec<String> {
    split_by_char(line, '|')
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

fn append_pipeline_tail(after: &mut Vec<String>, tail: &str) -> Result<(), String> {
    let trimmed = tail.trim();
    if trimmed.is_empty() {
        return Ok(());
    }
    let without_pipe = trimmed
        .strip_prefix('|')
        .ok_or_else(|| format!("unexpected trailing text after foreach block: {trimmed}"))?;
    let segments = split_on_pipes(without_pipe);
    for segment in segments {
        let segment = segment.trim();
        if segment.is_empty() {
            continue;
        }
        after.push(segment.to_string());
    }
    Ok(())
}

fn split_on_and(line: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_double = false;
    let mut in_single = false;
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut chars = line.chars().peekable();

    while let Some(ch) = chars.next() {
        if in_double && ch == '\\' && paren_depth == 0 {
            current.push(ch);
            if let Some(next) = chars.next() {
                current.push(next);
            }
            continue;
        }

        match ch {
            '\'' if !in_double && bracket_depth == 0 && paren_depth == 0 => {
                in_single = !in_single;
                current.push(ch);
            }
            '"' if !in_single && bracket_depth == 0 && paren_depth == 0 => {
                in_double = !in_double;
                current.push(ch);
            }
            '$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if matches!(chars.peek(), Some('(')) {
                    chars.next();
                    paren_depth = 1;
                    current.push('$');
                    current.push('(');
                } else {
                    current.push(ch);
                }
            }
            '[' if !in_double && !in_single && bracket_depth == 0 => {
                if matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                    current.push(ch);
                } else {
                    bracket_depth = 1;
                    current.push(ch);
                }
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                current.push(ch);
            }
            '(' if paren_depth > 0 => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' if paren_depth > 0 => {
                paren_depth -= 1;
                current.push(ch);
            }
            '&' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if matches!(chars.peek(), Some('&')) {
                    chars.next();
                    parts.push(current);
                    current = String::new();
                } else {
                    current.push(ch);
                }
            }
            _ => current.push(ch),
        }
    }

    parts.push(current);
    parts
}

fn unindent_block_lines(lines: &[String]) -> Vec<String> {
    lines
        .iter()
        .map(|line| {
            if let Some(rest) = line.strip_prefix('\t') {
                rest.to_string()
            } else {
                line.to_string()
            }
        })
        .collect()
}

fn split_by_char(line: &str, delimiter: char) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_double = false;
    let mut in_single = false;
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut chars = line.chars().peekable();

    while let Some(ch) = chars.next() {
        if in_double && ch == '\\' && paren_depth == 0 {
            current.push(ch);
            if let Some(next) = chars.next() {
                current.push(next);
            }
            continue;
        }

        match ch {
            '\'' if !in_double && bracket_depth == 0 && paren_depth == 0 => {
                in_single = !in_single;
                current.push(ch);
            }
            '"' if !in_single && bracket_depth == 0 && paren_depth == 0 => {
                in_double = !in_double;
                current.push(ch);
            }
            '$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if matches!(chars.peek(), Some('(')) {
                    chars.next();
                    paren_depth = 1;
                    current.push('$');
                    current.push('(');
                } else {
                    current.push(ch);
                }
            }
            '[' if !in_double && !in_single && bracket_depth == 0 => {
                if matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                    current.push(ch);
                } else {
                    bracket_depth = 1;
                    current.push(ch);
                }
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                current.push(ch);
            }
            '(' if paren_depth > 0 => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' if paren_depth > 0 => {
                paren_depth -= 1;
                current.push(ch);
            }
            ch if ch == delimiter
                && !in_double
                && !in_single
                && bracket_depth == 0
                && paren_depth == 0 =>
            {
                parts.push(current);
                current = String::new();
            }
            _ => current.push(ch),
        }
    }

    parts.push(current);
    parts
}

enum BraceParse {
    Inline {
        head: String,
        body: String,
        tail: Option<String>,
    },
    Open { head: String, tail: String },
    None { head: String },
}

fn parse_brace_block(line: &str) -> BraceParse {
    let bytes = line.as_bytes();
    let mut idx = 0;
    let mut in_double = false;
    let mut in_single = false;
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut brace_start = None;

    while idx < bytes.len() {
        let ch = bytes[idx] as char;

        if in_double && ch == '\\' && paren_depth == 0 {
            idx += 1;
            if idx < bytes.len() {
                idx += 1;
            }
            continue;
        }

        match ch {
            '\'' if !in_double && bracket_depth == 0 && paren_depth == 0 => {
                in_single = !in_single;
            }
            '"' if !in_single && bracket_depth == 0 && paren_depth == 0 => {
                in_double = !in_double;
            }
            '$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if idx + 1 < bytes.len() && bytes[idx + 1] == b'(' {
                    paren_depth = 1;
                    idx += 1;
                }
            }
            '[' if !in_double && !in_single && bracket_depth == 0 => {
                if idx + 1 < bytes.len() && (bytes[idx + 1] as char).is_whitespace() {
                    // literal [
                } else {
                    bracket_depth = 1;
                }
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
            }
            '(' if paren_depth > 0 => {
                paren_depth += 1;
            }
            ')' if paren_depth > 0 => {
                paren_depth -= 1;
            }
            '{'
                if !in_double
                    && !in_single
                    && bracket_depth == 0
                    && paren_depth == 0 =>
            {
                brace_start = Some(idx);
                break;
            }
            _ => {}
        }

        idx += 1;
    }

    let brace_start = match brace_start {
        Some(pos) => pos,
        None => {
            return BraceParse::None {
                head: line.to_string(),
            }
        }
    };

    let mut brace_depth = 0;
    let mut end = None;
    let mut scan = brace_start;
    in_double = false;
    in_single = false;
    bracket_depth = 0;
    paren_depth = 0;

    while scan < bytes.len() {
        let ch = bytes[scan] as char;

        if in_double && ch == '\\' && paren_depth == 0 {
            scan += 1;
            if scan < bytes.len() {
                scan += 1;
            }
            continue;
        }

        match ch {
            '\'' if !in_double && bracket_depth == 0 && paren_depth == 0 => {
                in_single = !in_single;
            }
            '"' if !in_single && bracket_depth == 0 && paren_depth == 0 => {
                in_double = !in_double;
            }
            '$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if scan + 1 < bytes.len() && bytes[scan + 1] == b'(' {
                    paren_depth = 1;
                    scan += 1;
                }
            }
            '[' if !in_double && !in_single && bracket_depth == 0 => {
                if scan + 1 < bytes.len() && (bytes[scan + 1] as char).is_whitespace() {
                    // literal [
                } else {
                    bracket_depth = 1;
                }
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
            }
            '(' if paren_depth > 0 => {
                paren_depth += 1;
            }
            ')' if paren_depth > 0 => {
                paren_depth -= 1;
            }
            '{'
                if !in_double
                    && !in_single
                    && bracket_depth == 0
                    && paren_depth == 0 =>
            {
                brace_depth += 1;
            }
            '}'
                if !in_double
                    && !in_single
                    && bracket_depth == 0
                    && paren_depth == 0 =>
            {
                brace_depth -= 1;
                if brace_depth == 0 {
                    end = Some(scan);
                    break;
                }
            }
            _ => {}
        }

        scan += 1;
    }

    let end = match end {
        Some(pos) => pos,
        None => {
            let head = line[..brace_start].trim_end().to_string();
            let tail = line[brace_start + 1..].trim().to_string();
            return BraceParse::Open { head, tail };
        }
    };

    let head = line[..brace_start].trim_end().to_string();
    let body = line[brace_start + 1..end].trim().to_string();
    let tail_raw = line[end + 1..].trim();
    let tail = if tail_raw.is_empty() {
        None
    } else {
        Some(tail_raw.to_string())
    };
    BraceParse::Inline { head, body, tail }
}

fn execute_inline_block(block: &str, state: &mut ShellState) -> Result<(), String> {
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

fn collect_brace_block(
    lines: &[String],
    start_idx: usize,
    tail: Option<String>,
) -> Result<(Vec<String>, usize, Option<String>), String> {
    let mut block_lines = Vec::new();
    if let Some(tail) = tail {
        if !tail.is_empty() {
            block_lines.push(tail);
        }
    }

    let mut brace_depth = 1;
    let mut idx = start_idx;
    while idx < lines.len() {
        let raw = &lines[idx];
        let trimmed = raw.trim();
        let mut in_double = false;
        let mut in_single = false;
        let mut bracket_depth = 0;
        let mut paren_depth = 0;
        let bytes = raw.as_bytes();
        let mut pos = 0;

        if trimmed.starts_with('}') {
            let trailing = trimmed[1..].trim();
            brace_depth -= 1;
            if brace_depth == 0 {
                let tail_line = if trailing.is_empty() {
                    None
                } else {
                    Some(trailing.to_string())
                };
                return Ok((block_lines, idx, tail_line));
            }
            idx += 1;
            continue;
        }

        while pos < bytes.len() {
            let ch = bytes[pos] as char;

            if in_double && ch == '\\' && paren_depth == 0 {
                pos += 1;
                if pos < bytes.len() {
                    pos += 1;
                }
                continue;
            }

            match ch {
                '\'' if !in_double && bracket_depth == 0 && paren_depth == 0 => {
                    in_single = !in_single;
                }
                '"' if !in_single && bracket_depth == 0 && paren_depth == 0 => {
                    in_double = !in_double;
                }
                '$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                    if pos + 1 < bytes.len() && bytes[pos + 1] == b'(' {
                        paren_depth = 1;
                        pos += 1;
                    }
                }
                '[' if !in_double && !in_single && bracket_depth == 0 => {
                    if pos + 1 < bytes.len() && (bytes[pos + 1] as char).is_whitespace() {
                        // literal [
                    } else {
                        bracket_depth = 1;
                    }
                }
                '[' if bracket_depth > 0 => {
                    bracket_depth += 1;
                }
                ']' if bracket_depth > 0 => {
                    bracket_depth -= 1;
                }
                '(' if paren_depth > 0 => {
                    paren_depth += 1;
                }
                ')' if paren_depth > 0 => {
                    paren_depth -= 1;
                }
                '{'
                    if !in_double
                        && !in_single
                        && bracket_depth == 0
                        && paren_depth == 0 =>
                {
                    brace_depth += 1;
                }
                '}'
                    if !in_double
                        && !in_single
                        && bracket_depth == 0
                        && paren_depth == 0 =>
                {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        return Ok((block_lines, idx, None));
                    }
                }
                _ => {}
            }

            pos += 1;
        }

        let mut line = raw.to_string();
        if line.starts_with('\t') {
            line = line[1..].to_string();
        }
        block_lines.push(line);
        idx += 1;
    }

    Err("unterminated brace block".into())
}

fn parse_foreach_line(
    line: &str,
) -> Option<(
    String,
    Vec<String>,
    Vec<String>,
    Option<String>,
    bool,
    Option<String>,
)> {
    let segments = split_on_pipes(line);
    if segments.len() < 2 {
        return None;
    }
    let mut foreach_index = None;
    let mut foreach_var = String::new();
    let mut foreach_block = None;
    let mut foreach_open = false;
    let mut foreach_tail = None;

    for (idx, segment) in segments.iter().enumerate() {
        let brace = parse_brace_block(segment.trim());
        let (body, brace_block, brace_open, brace_tail) = match brace {
            BraceParse::Inline { head, body, tail: _ } => (head, Some(body), false, None),
            BraceParse::Open { head, tail } => (head, None, true, Some(tail)),
            BraceParse::None { head } => (head, None, false, None),
        };
        let args = parse_args(&body).ok()?;
        if args.len() == 2 && args[0] == "foreach" {
            foreach_index = Some(idx);
            foreach_var = args[1].clone();
            foreach_block = brace_block;
            foreach_open = brace_open;
            foreach_tail = brace_tail;
            break;
        }
    }

    let foreach_index = foreach_index?;
    let before = segments[..foreach_index].to_vec();
    let after = segments[foreach_index + 1..].to_vec();
    Some((
        foreach_var,
        before,
        after,
        foreach_block,
        foreach_open,
        foreach_tail,
    ))
}

fn parse_args(line: &str) -> Result<Vec<String>, String> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut in_double = false;
    let mut in_single = false;
    let mut current_quote: Option<char> = None;
    let mut chars = line.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '\'' if bracket_depth == 0 && paren_depth == 0 && !in_double && !in_single => {
                if !current.is_empty() {
                    return Err("unexpected quote inside token".into());
                }
                in_single = true;
                current_quote = Some('\'');
            }
            '\'' if in_single => {
                in_single = false;
            }
            '"' if bracket_depth == 0 && paren_depth == 0 && !in_double => {
                if !current.is_empty() {
                    return Err("unexpected quote inside token".into());
                }
                in_double = true;
                current_quote = Some('"');
            }
            '"' if in_double && paren_depth == 0 => {
                in_double = false;
            }
            '\\' if in_double && paren_depth == 0 => {
                if let Some(next) = chars.next() {
                    current.push(next);
                }
            }
            '$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if matches!(chars.peek(), Some('(')) {
                    chars.next();
                    paren_depth = 1;
                    current.push('$');
                    current.push('(');
                } else {
                    current.push(ch);
                }
            }
            '[' if !in_double && !in_single && bracket_depth == 0 => {
                if matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                    current.push(ch);
                } else {
                    bracket_depth = 1;
                    current.push(ch);
                }
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                current.push(ch);
            }
            '(' if paren_depth > 0 => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' if paren_depth > 0 => {
                paren_depth -= 1;
                current.push(ch);
            }
            c if c.is_whitespace()
                && bracket_depth == 0
                && paren_depth == 0
                && !in_double
                && !in_single =>
            {
                if !current.is_empty() {
                    if let Some(quote) = current_quote {
                        args.push(format!("{quote}{current}{quote}"));
                    } else {
                        args.push(std::mem::take(&mut current));
                    }
                    current.clear();
                    current_quote = None;
                }
            }
            c => current.push(c),
        }
    }

    if bracket_depth != 0 {
        return Err("unterminated capture group".into());
    }

    if paren_depth != 0 {
        return Err("unterminated command substitution".into());
    }

    if in_double {
        return Err("unterminated string".into());
    }

    if in_single {
        return Err("unterminated string".into());
    }

    if !current.is_empty() {
        if let Some(quote) = current_quote {
            args.push(format!("{quote}{current}{quote}"));
        } else {
            args.push(current);
        }
    }

    Ok(args)
}

fn expand_tokens(tokens: Vec<String>, state: &ShellState) -> Result<Vec<String>, String> {
    let mut expanded = Vec::new();

    for token in tokens {
        if token.starts_with('\'') && token.ends_with('\'') && token.len() >= 2 {
            expanded.push(token[1..token.len() - 1].to_string());
            continue;
        }
        if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
            match expand_string_literal(&token[1..token.len() - 1], state) {
                Ok(parts) => expanded.extend(parts),
                Err(err) => return Err(format!("string expansion failed: {err}")),
            }
            continue;
        }
        if token.starts_with("...") {
            let suffix = &token[3..];
            if suffix.is_empty() {
                return Err("spread requires content".into());
            }
            let source = expand_unquoted_token(suffix, state)?;
            let spread_tokens = parse_args(&source)?;
            let spread_expanded = expand_tokens(spread_tokens, state)?;
            expanded.extend(spread_expanded);
            continue;
        }

        let value = expand_unquoted_token(&token, state)?;
        if should_expand_with_handler(&value, state) {
            let replacements = run_expansion_handler(&value, state)?;
            expanded.extend(replacements);
        } else {
            expanded.push(value);
        }
    }

    Ok(expanded)
}

fn should_expand_with_handler(token: &str, state: &ShellState) -> bool {
    if state.options.expansions_chars.is_empty() {
        return false;
    }
    token.chars().any(|ch| state.options.expansions_chars.contains(&ch))
}

fn run_expansion_handler(token: &str, state: &ShellState) -> Result<Vec<String>, String> {
    if state.options.expansions_handler.is_empty() {
        return Err("expansion handler not configured".into());
    }
    let mut cmd = Command::new(&state.options.expansions_handler[0]);
    if state.options.expansions_handler.len() > 1 {
        cmd.args(&state.options.expansions_handler[1..]);
    }
    let output = cmd
        .arg(token)
        .output()
        .map_err(|err| format!("failed to execute expansion handler: {err}"))?;
    if !output.status.success() {
        return Err("expansion handler failed".into());
    }
    let text = String::from_utf8(output.stdout)
        .map_err(|_| "expansion handler output not utf-8".to_string())?;
    parse_json_string_array(&text)
}

fn parse_json_string_array(input: &str) -> Result<Vec<String>, String> {
    let mut chars = input.chars().peekable();
    skip_json_ws(&mut chars);
    if chars.next() != Some('[') {
        return Err("expansion handler output must be a JSON array".into());
    }
    let mut items = Vec::new();

    loop {
        skip_json_ws(&mut chars);
        match chars.peek() {
            Some(']') => {
                chars.next();
                break;
            }
            Some('"') => {
                chars.next();
                let value = parse_json_string(&mut chars)?;
                items.push(value);
                skip_json_ws(&mut chars);
                match chars.peek() {
                    Some(',') => {
                        chars.next();
                    }
                    Some(']') => {
                        chars.next();
                        break;
                    }
                    _ => return Err("expansion handler output malformed".into()),
                }
            }
            _ => return Err("expansion handler output malformed".into()),
        }
    }

    Ok(items)
}

fn skip_json_ws<I: Iterator<Item = char>>(chars: &mut std::iter::Peekable<I>) {
    while matches!(chars.peek(), Some(ch) if ch.is_whitespace()) {
        chars.next();
    }
}

fn parse_json_string<I: Iterator<Item = char>>(
    chars: &mut std::iter::Peekable<I>,
) -> Result<String, String> {
    let mut result = String::new();
    while let Some(ch) = chars.next() {
        match ch {
            '"' => return Ok(result),
            '\\' => {
                let escaped = chars.next().ok_or_else(|| "invalid json escape".to_string())?;
                match escaped {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    'b' => result.push('\u{0008}'),
                    'f' => result.push('\u{000C}'),
                    _ => return Err("unsupported json escape".into()),
                }
            }
            other => result.push(other),
        }
    }
    Err("unterminated json string".into())
}

fn expand_unquoted_token(token: &str, state: &ShellState) -> Result<String, String> {
    let chars: Vec<char> = token.chars().collect();
    let mut result = String::new();
    let mut idx = 0;

    while idx < chars.len() {
        let ch = chars[idx];

        if ch == '$' && idx + 1 < chars.len() {
            let next = chars[idx + 1];
            if next == '[' {
                if let Some((capture, next_idx)) = read_bracket_capture_chars(&chars, idx + 2) {
                    let value = run_capture(&capture, state)
                        .map_err(|err| format!("capture failed: {err}"))?;
                    result.push_str(&value);
                    idx = next_idx;
                    continue;
                }
            } else if next == '(' {
                if let Some((capture, next_idx)) = read_paren_capture_chars(&chars, idx + 2) {
                    let value = run_capture(&capture, state)
                        .map_err(|err| format!("capture failed: {err}"))?;
                    result.push_str(&value);
                    idx = next_idx;
                    continue;
                }
            } else if is_var_start(next) {
                let (name, next_idx) = read_var_name(&chars, idx + 1);
                let value = lookup_var(state, &name);
                result.push_str(&value);
                idx = next_idx;
                continue;
            }
        }

        if ch == '[' {
            if let Some((capture, next_idx)) = read_bracket_capture_chars(&chars, idx + 1) {
                let value = run_capture(&capture, state)
                    .map_err(|err| format!("capture failed: {err}"))?;
                result.push_str(&value);
                idx = next_idx;
                continue;
            }
        }

        result.push(ch);
        idx += 1;
    }

    Ok(result)
}

fn expand_string_literal(body: &str, state: &ShellState) -> Result<Vec<String>, String> {
    let mut result = String::new();
    let mut captures = Vec::new();
    let mut chars = body.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '$' {
            if chars.peek() == Some(&'[') {
                chars.next();
                let capture = read_capture(&mut chars)?;
                let value =
                    run_capture(&capture, state).map_err(|err| format!("capture failed: {err}"))?;
                result.push_str(&value);
            } else if chars.peek() == Some(&'(') {
                chars.next();
                let capture = read_until_closing(&mut chars, ')')?;
                let value =
                    run_capture(&capture, state).map_err(|err| format!("capture failed: {err}"))?;
                result.push_str(&value);
            } else if matches!(chars.peek(), Some(next) if is_var_start(*next)) {
                let name = read_var_name_iter(&mut chars);
                let value = lookup_var(state, &name);
                result.push_str(&value);
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }

    captures.push(result);
    Ok(captures)
}

fn read_bracket_capture_chars(chars: &[char], mut idx: usize) -> Option<(String, usize)> {
    let mut depth = 1;
    let mut content = String::new();

    while idx < chars.len() {
        let ch = chars[idx];
        if ch == '[' {
            depth += 1;
            content.push(ch);
        } else if ch == ']' {
            depth -= 1;
            if depth == 0 {
                return Some((content, idx + 1));
            }
            content.push(ch);
        } else {
            content.push(ch);
        }
        idx += 1;
    }

    None
}

fn read_paren_capture_chars(chars: &[char], mut idx: usize) -> Option<(String, usize)> {
    let mut depth = 1;
    let mut content = String::new();

    while idx < chars.len() {
        let ch = chars[idx];
        if ch == '(' {
            depth += 1;
            content.push(ch);
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                return Some((content, idx + 1));
            }
            content.push(ch);
        } else {
            content.push(ch);
        }
        idx += 1;
    }

    None
}

fn read_var_name_iter<I>(chars: &mut std::iter::Peekable<I>) -> String
where
    I: Iterator<Item = char>,
{
    let mut name = String::new();
    if let Some(ch) = chars.next() {
        name.push(ch);
    }

    while let Some(ch) = chars.peek() {
        if is_var_char(*ch) {
            name.push(*ch);
            chars.next();
        } else {
            break;
        }
    }

    name
}

fn read_var_name(chars: &[char], start: usize) -> (String, usize) {
    let mut idx = start + 1;
    while idx < chars.len() && is_var_char(chars[idx]) {
        idx += 1;
    }
    let name: String = chars[start..idx].iter().collect();
    (name, idx)
}

fn is_var_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

fn is_var_char(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

fn lookup_var(state: &ShellState, name: &str) -> String {
    if let Some(value) = state.vars.get(name) {
        return value.clone();
    }

    env::var(name).unwrap_or_default()
}

fn read_capture<I>(chars: &mut I) -> Result<String, String>
where
    I: Iterator<Item = char>,
{
    let mut depth = 1;
    let mut content = String::new();

    while let Some(ch) = chars.next() {
        match ch {
            '[' => {
                depth += 1;
                content.push(ch);
            }
            ']' => {
                depth -= 1;
                if depth == 0 {
                    return Ok(content);
                }
                content.push(ch);
            }
            _ => content.push(ch),
        }
    }

    Err("unterminated capture in string".into())
}

fn read_until_closing<I>(chars: &mut I, closing: char) -> Result<String, String>
where
    I: Iterator<Item = char>,
{
    let mut depth = 1;
    let mut content = String::new();

    while let Some(ch) = chars.next() {
        if ch == closing {
            depth -= 1;
            if depth == 0 {
                return Ok(content);
            }
        } else if ch == '(' && closing == ')' {
            depth += 1;
        }
        content.push(ch);
    }

    Err("unterminated command substitution".into())
}

fn run_capture(body: &str, state: &ShellState) -> io::Result<String> {
    let inner = body.trim();
    if inner.is_empty() {
        return Ok(String::new());
    }

    let (locals_path, _guard) =
        write_locals_file(state).map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let (script_path, _script_guard) =
        write_block_file(inner).map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let exe = env::current_exe()?;
    let output = Command::new(exe)
        .arg("--capture-worker")
        .arg("--script")
        .arg(script_path)
        .arg("--locals")
        .arg(locals_path)
        .output()?;

    io::stderr().write_all(&output.stderr)?;
    trim_capture_output(output, state.options.subshells_trim_newline)
}

fn trim_capture_output(output: Output, trim_newline: bool) -> io::Result<String> {
    let mut text = String::from_utf8(output.stdout)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "capture output not utf-8"))?;

    if trim_newline && text.ends_with('\n') {
        text.pop();
        if text.ends_with('\r') {
            text.pop();
        }
    }

    Ok(text)
}

struct ScriptContext<'a> {
    lines: Vec<String>,
    state: &'a mut ShellState,
}

struct BlockResult {
    next: usize,
    exit: bool,
}

impl<'a> ScriptContext<'a> {
    fn execute(&mut self) -> Result<(), String> {
        let _ = self.execute_with_exit()?;
        Ok(())
    }

    fn execute_with_exit(&mut self) -> Result<bool, String> {
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
                return Ok((false, block_start, true));
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
                return Ok((false, end_idx + 1, true));
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

fn split_indent(line: &str) -> (usize, &str) {
    let bytes = line.as_bytes();
    let mut idx = 0;
    while idx < bytes.len() && bytes[idx] == b'\t' {
        idx += 1;
    }
    (idx, &line[idx..])
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
