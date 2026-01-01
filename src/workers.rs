use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::state::{create_temp_file, read_locals_file, write_locals_file, ShellState, TempFileGuard};
use crate::{execute_inline_block, FlowControl, ScriptContext};

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
            if let FlowControl::Return(_) = execute_inline_block(&block, &mut state)? {
                return Err("return not allowed in foreach".into());
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
        FlowControl::None => {}
    }
    Ok(())
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

pub fn trim_capture_output(output: std::process::Output, trim_newline: bool) -> io::Result<String> {
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
