use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::process::{Command, ExitStatus, Output};

fn main() {
    let args: Vec<String> = env::args().collect();

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

    loop {
        if print_prompt(&mut stdout).is_err() {
            break;
        }

        let mut line = String::new();
        match stdin.read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {
                if !process_line(&line) {
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

    for raw_line in reader.lines() {
        let line = raw_line?;
        if !process_line(&line) {
            break;
        }
    }

    Ok(())
}

fn process_line(line: &str) -> bool {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return true;
    }

    if trimmed == "exit" {
        return false;
    }

    let args = match parse_args(trimmed) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("unshell: {err}");
            return true;
        }
    };

    let expanded = match expand_tokens(args) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("unshell: {err}");
            return true;
        }
    };

    execute(&expanded);
    true
}

fn parse_args(line: &str) -> Result<Vec<String>, String> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;

    let mut chars = line.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '[' if bracket_depth == 0 && current.is_empty() => {
                bracket_depth = 1;
                current.push(ch);
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                current.push(ch);

                if bracket_depth == 0 {
                    // require next separator or end
                    if let Some(next) = chars.peek() {
                        if !next.is_whitespace() {
                            return Err("capture must be followed by whitespace".into());
                        }
                    }
                }
            }
            c if c.is_whitespace() && bracket_depth == 0 => {
                if !current.is_empty() {
                    args.push(std::mem::take(&mut current));
                }
            }
            c => current.push(c),
        }
    }

    if bracket_depth != 0 {
        return Err("unterminated capture group".into());
    }

    if !current.is_empty() {
        args.push(current);
    }

    Ok(args)
}

fn expand_tokens(tokens: Vec<String>) -> Result<Vec<String>, String> {
    let mut expanded = Vec::new();

    for token in tokens {
        if is_capture(&token) {
            match run_capture(&token) {
                Ok(value) => expanded.push(value),
                Err(err) => return Err(format!("capture failed: {err}")),
            }
        } else {
            expanded.push(token);
        }
    }

    Ok(expanded)
}

fn is_capture(token: &str) -> bool {
    token.starts_with('[') && token.ends_with(']') && token.len() > 2
}

fn run_capture(token: &str) -> io::Result<String> {
    let inner = token.get(1..token.len() - 1).unwrap_or_default().trim();
    if inner.is_empty() {
        return Ok(String::new());
    }

    let args = parse_args(inner).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
    if args.is_empty() {
        return Ok(String::new());
    }

    let output = Command::new(&args[0]).args(&args[1..]).output()?;
    trim_capture_output(output)
}

fn trim_capture_output(output: Output) -> io::Result<String> {
    let mut text = String::from_utf8(output.stdout)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "capture output not utf-8"))?;

    if text.ends_with('\n') {
        text.pop();
        if text.ends_with('\r') {
            text.pop();
        }
    }

    Ok(text)
}

fn print_prompt(stdout: &mut io::Stdout) -> io::Result<()> {
    stdout.write_all(b"unshell> ")?;
    stdout.flush()
}

fn execute(args: &[String]) {
    if args.is_empty() {
        return;
    }

    match Command::new(&args[0]).args(&args[1..]).status() {
        Ok(status) => report_status(&args[0], status),
        Err(err) => eprintln!("unshell: failed to execute '{}': {err}", args[0]),
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
