use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::process::{Command, ExitStatus};

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

    let args = parse_args(trimmed);
    execute(&args);
    true
}

fn parse_args(line: &str) -> Vec<String> {
    line.split_whitespace()
        .map(|part| part.to_string())
        .collect()
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
