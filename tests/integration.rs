use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

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
