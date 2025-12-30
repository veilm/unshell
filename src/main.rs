use std::io::{self, Write};
use std::process::{Command, ExitStatus};

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        if print_prompt(&mut stdout).is_err() {
            break;
        }

        let mut line = String::new();
        match stdin.read_line(&mut line) {
            Ok(0) => {
                // EOF
                break;
            }
            Ok(_) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }

                if trimmed == "exit" {
                    break;
                }

                let args: Vec<&str> = trimmed.split_whitespace().collect();
                execute(&args);
            }
            Err(err) => {
                eprintln!("unshell: failed to read line: {err}");
                break;
            }
        }
    }
}

fn print_prompt(stdout: &mut io::Stdout) -> io::Result<()> {
    stdout.write_all(b"unshell> ")?;
    stdout.flush()
}

fn execute(args: &[&str]) {
    if args.is_empty() {
        return;
    }

    match Command::new(args[0]).args(&args[1..]).status() {
        Ok(status) => report_status(args[0], status),
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
