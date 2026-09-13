use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

struct Fixture {
    name: String,
    script: PathBuf,
    stdout: String,
    stderr: String,
}

#[test]
fn run_all_fixtures() {
    for fixture in load_fixtures().expect("fixtures") {
        let output = Command::new(env!("CARGO_BIN_EXE_ush"))
            .arg("--norc")
            .arg(&fixture.script)
            .output()
            .expect("failed to run ush");

        let actual_stdout = String::from_utf8(output.stdout).expect("stdout not UTF-8");
        let actual_stderr = String::from_utf8(output.stderr).expect("stderr not UTF-8");

        assert_eq!(
            actual_stdout, fixture.stdout,
            "stdout mismatch for fixture '{}'",
            fixture.name
        );
        assert_eq!(
            actual_stderr, fixture.stderr,
            "stderr mismatch for fixture '{}'",
            fixture.name
        );
    }
}

fn load_fixtures() -> std::io::Result<Vec<Fixture>> {
    let mut fixtures = Vec::new();
    let dir = Path::new("tests/fixtures");

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().and_then(|ext| ext.to_str()) != Some("ush") {
            continue;
        }

        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();

        let stdout = read_optional(&path.with_extension("stdout"));
        let stderr = read_optional(&path.with_extension("stderr"));

        fixtures.push(Fixture {
            name,
            script: path,
            stdout,
            stderr,
        });
    }

    fixtures.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(fixtures)
}

fn read_optional(path: &Path) -> String {
    match fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => String::new(),
    }
}

#[test]
fn sigint_resets_when_parent_ignores() {
    let script = Path::new("tests/fixtures/sigint_reset.ush");
    let stdout = read_optional(&script.with_extension("stdout"));
    let stderr = read_optional(&script.with_extension("stderr"));
    let command = format!(
        "trap '' INT; exec {} --norc {}",
        env!("CARGO_BIN_EXE_ush"),
        script.display()
    );

    let output = Command::new("sh")
        .arg("-c")
        .arg(command)
        .output()
        .expect("failed to run ush with ignored SIGINT");

    let actual_stdout = String::from_utf8(output.stdout).expect("stdout not UTF-8");
    let actual_stderr = String::from_utf8(output.stderr).expect("stderr not UTF-8");

    assert_eq!(
        actual_stdout, stdout,
        "stdout mismatch for sigint_reset fixture"
    );
    assert_eq!(
        actual_stderr, stderr,
        "stderr mismatch for sigint_reset fixture"
    );
}

#[test]
fn foreach_break_closes_large_upstream() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_ush"))
        .arg("--norc")
        .arg("-c")
        .arg("seq 100000 | foreach line { break }\necho done")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to run foreach break test");

    let deadline = Instant::now() + Duration::from_secs(5);
    let status = loop {
        if let Some(status) = child.try_wait().expect("failed to poll ush") {
            break status;
        }
        if Instant::now() >= deadline {
            let _ = child.kill();
            let _ = child.wait();
            panic!("foreach break left the upstream process blocked");
        }
        thread::sleep(Duration::from_millis(10));
    };

    let mut stdout = String::new();
    child
        .stdout
        .take()
        .unwrap()
        .read_to_string(&mut stdout)
        .expect("stdout not readable");
    let mut stderr = String::new();
    child
        .stderr
        .take()
        .unwrap()
        .read_to_string(&mut stderr)
        .expect("stderr not readable");

    assert!(status.success(), "ush failed: {stderr}");
    assert_eq!(stdout, "done\n");
    assert_eq!(stderr, "");
}
