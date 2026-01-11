use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::state::{
    create_temp_file, debug_log_line_to, format_command, format_function_body, read_locals_file,
    write_locals_file, FunctionBody, FunctionDef, ShellState, TempFileGuard,
};
use crate::{execute_inline_block, FlowControl, ScriptContext};

const FUNCTION_MAX_DEPTH: usize = 64;

pub struct ForeachWorkerArgs {
    pub var: String,
    pub block_path: PathBuf,
    pub locals_path: PathBuf,
    pub inline: bool,
}

pub struct CaptureWorkerArgs {
    pub script_path: PathBuf,
    pub locals_path: PathBuf,
}

pub struct FunctionWorkerArgs {
    pub name: String,
    pub args: Vec<String>,
    pub locals_path: PathBuf,
}

pub struct BlockWorkerArgs {
    pub block_path: PathBuf,
    pub locals_path: PathBuf,
}

pub fn run_foreach_worker(args: &[String]) -> Result<(), String> {
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
            match execute_inline_block(&block, &mut state)? {
                FlowControl::Return(_) => return Err("return not allowed in foreach".into()),
                FlowControl::Break => return Err("break not allowed in foreach".into()),
                FlowControl::Continue => return Err("continue not allowed in foreach".into()),
                FlowControl::Exit | FlowControl::None => {}
            }
        } else {
            let mut ctx = ScriptContext {
                lines: block.lines().map(|s| s.to_string()).collect(),
                state: &mut state,
            };
            match ctx.execute_with_exit()? {
                FlowControl::Exit => {
                    exit_requested = true;
                    break;
                }
                FlowControl::Return(_) => {
                    return Err("return not allowed in foreach".into());
                }
                FlowControl::Break => {
                    return Err("break not allowed in foreach".into());
                }
                FlowControl::Continue => {
                    return Err("continue not allowed in foreach".into());
                }
                FlowControl::None => {}
            }
        }
    }

    stdout.flush().map_err(|err| format!("failed to flush foreach output: {err}"))?;

    if exit_requested {
        std::process::exit(0);
    }

    Ok(())
}

pub fn run_capture_worker(args: &[String]) -> Result<(), String> {
    let opts = parse_capture_worker_args(args)?;
    let mut state = read_locals_file(&opts.locals_path)
        .map_err(|err| format!("failed to load capture locals: {err}"))?;
    let script = read_block_file(&opts.script_path)
        .map_err(|err| format!("failed to load capture script: {err}"))?;

    let mut ctx = ScriptContext {
        lines: vec![script],
        state: &mut state,
    };
    match ctx.execute_with_exit()? {
        FlowControl::Exit => return Err("exit not allowed in capture".into()),
        FlowControl::Return(_) => return Err("return not allowed in capture".into()),
        FlowControl::Break => return Err("break not allowed in capture".into()),
        FlowControl::Continue => return Err("continue not allowed in capture".into()),
        FlowControl::None => {}
    }
    Ok(())
}

pub fn run_function_worker(args: &[String]) -> Result<i32, String> {
    let opts = parse_function_worker_args(args)?;
    let mut state = read_locals_file(&opts.locals_path)
        .map_err(|err| format!("failed to load function locals: {err}"))?;
    let func = state
        .functions
        .get(&opts.name)
        .cloned()
        .ok_or_else(|| format!("unknown function '{}'", opts.name))?;
    let mut call_args = Vec::with_capacity(opts.args.len() + 1);
    call_args.push(opts.name.clone());
    call_args.extend(opts.args);
    run_function_in_worker(&call_args, func, &mut state)
}

pub fn run_block_worker(args: &[String]) -> Result<i32, String> {
    let opts = parse_block_worker_args(args)?;
    let mut state = read_locals_file(&opts.locals_path)
        .map_err(|err| format!("failed to load block locals: {err}"))?;
    let block = read_block_file(&opts.block_path)
        .map_err(|err| format!("failed to read block: {err}"))?;
    if block.contains('\n') {
        let mut ctx = ScriptContext {
            lines: block.lines().map(|s| s.to_string()).collect(),
            state: &mut state,
        };
        match ctx.execute_with_exit()? {
            FlowControl::Return(_) => Err("return not allowed in inline block".into()),
            FlowControl::Break => Err("break not allowed in inline block".into()),
            FlowControl::Continue => Err("continue not allowed in inline block".into()),
            FlowControl::Exit => Ok(state.last_status),
            FlowControl::None => Ok(state.last_status),
        }
    } else {
        match execute_inline_block(&block, &mut state) {
            Ok(FlowControl::Return(_)) => Err("return not allowed in inline block".into()),
            Ok(FlowControl::Break) => Err("break not allowed in inline block".into()),
            Ok(FlowControl::Continue) => Err("continue not allowed in inline block".into()),
            Ok(FlowControl::None) => Ok(state.last_status),
            Ok(FlowControl::Exit) => Ok(state.last_status),
            Err(err) => Err(err),
        }
    }
}

pub fn parse_foreach_worker_args(args: &[String]) -> Result<ForeachWorkerArgs, String> {
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

pub fn parse_capture_worker_args(args: &[String]) -> Result<CaptureWorkerArgs, String> {
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

pub fn parse_function_worker_args(args: &[String]) -> Result<FunctionWorkerArgs, String> {
    let mut name = None;
    let mut locals_path = None;
    let mut func_args = Vec::new();
    let mut idx = 0;

    while idx < args.len() {
        match args[idx].as_str() {
            "--name" => {
                idx += 1;
                name = args.get(idx).cloned();
            }
            "--locals" => {
                idx += 1;
                locals_path = args.get(idx).map(PathBuf::from);
            }
            "--args" => {
                idx += 1;
                if idx < args.len() {
                    func_args = args[idx..].to_vec();
                }
                break;
            }
            other => {
                return Err(format!("unknown function worker flag '{other}'"));
            }
        }
        idx += 1;
    }

    Ok(FunctionWorkerArgs {
        name: name.ok_or_else(|| "function worker missing --name".to_string())?,
        args: func_args,
        locals_path: locals_path
            .ok_or_else(|| "function worker missing --locals".to_string())?,
    })
}

pub fn parse_block_worker_args(args: &[String]) -> Result<BlockWorkerArgs, String> {
    let mut block_path = None;
    let mut locals_path = None;
    let mut idx = 0;

    while idx < args.len() {
        match args[idx].as_str() {
            "--block" => {
                idx += 1;
                block_path = args.get(idx).map(PathBuf::from);
            }
            "--locals" => {
                idx += 1;
                locals_path = args.get(idx).map(PathBuf::from);
            }
            other => {
                return Err(format!("unknown block worker flag '{other}'"));
            }
        }
        idx += 1;
    }

    Ok(BlockWorkerArgs {
        block_path: block_path.ok_or_else(|| "block worker missing --block".to_string())?,
        locals_path: locals_path
            .ok_or_else(|| "block worker missing --locals".to_string())?,
    })
}

pub fn read_block_file(path: &Path) -> io::Result<String> {
    let mut file = std::fs::File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

pub fn trim_trailing_newline(text: &mut String) {
    if text.ends_with('\n') {
        text.pop();
        if text.ends_with('\r') {
            text.pop();
        }
    }
}

pub fn write_block_file(block: &str) -> io::Result<(PathBuf, TempFileGuard)> {
    let (path, mut file) = create_temp_file("block")?;
    file.write_all(block.as_bytes())?;
    file.flush()?;
    Ok((path.clone(), TempFileGuard::new(path)))
}

pub fn run_capture(body: &str, state: &ShellState) -> io::Result<String> {
    let inner = body.trim();
    if inner.is_empty() {
        return Ok(String::new());
    }

    let (locals_path, _guard) =
        write_locals_file(state).map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let (script_path, _script_guard) =
        write_block_file(inner).map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let exe = std::env::current_exe()?;
    let mut child = Command::new(exe)
        .arg("--capture-worker")
        .arg("--script")
        .arg(script_path)
        .arg("--locals")
        .arg(locals_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()?;

    let mut stdout = Vec::new();
    if let Some(mut out) = child.stdout.take() {
        out.read_to_end(&mut stdout)?;
    }
    let _status = child.wait()?;
    trim_capture_output(stdout, state.options.subshells_trim_newline)
}

fn run_function_in_worker(
    args: &[String],
    func: FunctionDef,
    state: &mut ShellState,
) -> Result<i32, String> {
    if state.locals_stack.len() >= FUNCTION_MAX_DEPTH {
        return Err("function call depth exceeded".into());
    }
    let log_path = state.options.debug_log_path.clone();
    let cmd_text = format_command(args);
    debug_log_line_to(log_path.as_deref(), &format!("run: {cmd_text}"));
    debug_log_line_to(
        log_path.as_deref(),
        &format!(
            "function: {} -> {}",
            args[0],
            format_function_body(&func.body)
        ),
    );
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

    let flow = result?;
    state.positional = state.positional_stack.pop().unwrap_or_default();
    state.locals_stack.pop();

    let code = match flow {
        FlowControl::Exit => state.last_status,
        FlowControl::Return(code) => code,
        FlowControl::None => state.last_status,
        FlowControl::Break => return Err("break not allowed in pipeline".into()),
        FlowControl::Continue => return Err("continue not allowed in pipeline".into()),
    };

    debug_log_line_to(
        log_path.as_deref(),
        &format!("exit: {cmd_text} -> {code}"),
    );
    Ok(code)
}

pub fn trim_capture_output(output: Vec<u8>, trim_newline: bool) -> io::Result<String> {
    let mut text = String::from_utf8(output)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "capture output not utf-8"))?;

    if trim_newline && text.ends_with('\n') {
        text.pop();
        if text.ends_with('\r') {
            text.pop();
        }
    }

    Ok(text)
}
