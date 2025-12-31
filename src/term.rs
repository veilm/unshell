use std::io::{self, Write};
use std::io::IsTerminal;

#[cfg(unix)]
pub fn cursor_column() -> Option<u16> {
    if !io::stdout().is_terminal() || !io::stdin().is_terminal() {
        return None;
    }

    let mut stdout = io::stdout().lock();
    if stdout.write_all(b"\x1b[6n").is_err() {
        return None;
    }
    if stdout.flush().is_err() {
        return None;
    }

    unsafe {
        let fd = libc::STDIN_FILENO;
        let mut termios = std::mem::zeroed();
        if libc::tcgetattr(fd, &mut termios) != 0 {
            return None;
        }
        let original = termios;
        termios.c_lflag &= !(libc::ICANON | libc::ECHO);
        termios.c_cc[libc::VMIN] = 1;
        termios.c_cc[libc::VTIME] = 2;
        if libc::tcsetattr(fd, libc::TCSANOW, &termios) != 0 {
            return None;
        }

        let mut collected: Vec<u8> = Vec::new();
        let mut buf = [0u8; 64];
        let first = libc::read(fd, buf.as_mut_ptr() as *mut _, buf.len());
        if first > 0 {
            collected.extend_from_slice(&buf[..first as usize]);
        }

        termios.c_cc[libc::VMIN] = 0;
        termios.c_cc[libc::VTIME] = 1;
        let _ = libc::tcsetattr(fd, libc::TCSANOW, &termios);

        let mut attempts = 0;
        while attempts < 5 && !collected.contains(&b'R') {
            let n = libc::read(fd, buf.as_mut_ptr() as *mut _, buf.len());
            if n > 0 {
                collected.extend_from_slice(&buf[..n as usize]);
            }
            attempts += 1;
        }

        let _ = libc::tcsetattr(fd, libc::TCSANOW, &original);

        parse_cursor_response(&collected)
    }
}

#[cfg(not(unix))]
pub fn cursor_column() -> Option<u16> {
    let _ = io::stdout().flush();
    None
}

#[cfg(unix)]
fn parse_cursor_response(bytes: &[u8]) -> Option<u16> {
    let text = std::str::from_utf8(bytes).ok()?;
    let start = text.rfind("\x1b[")?;
    let response = &text[start + 2..];
    let mut parts = response.split('R').next()?.split(';');
    let _row = parts.next()?;
    let col = parts.next()?;
    col.parse::<u16>().ok()
}
