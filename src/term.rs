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

#[cfg(unix)]
fn terminal_columns() -> Option<u16> {
    if !io::stdout().is_terminal() {
        return None;
    }
    unsafe {
        let mut winsize: libc::winsize = std::mem::zeroed();
        if libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut winsize) != 0 {
            return None;
        }
        if winsize.ws_col == 0 {
            return None;
        }
        Some(winsize.ws_col)
    }
}

#[cfg(not(unix))]
fn terminal_columns() -> Option<u16> {
    None
}

fn prompt_spacer_bytes(columns: u16) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(b"\x1b[7m$\x1b[0m");
    let pad = columns.saturating_sub(1) as usize;
    if pad > 0 {
        out.extend(std::iter::repeat(b' ').take(pad));
    }
    out.extend_from_slice(b"\r\x1b[J");
    out
}

pub fn print_prompt_spacer(stdout: &mut dyn Write) -> io::Result<bool> {
    let columns = match terminal_columns() {
        Some(columns) => columns,
        None => return Ok(false),
    };
    let bytes = prompt_spacer_bytes(columns);
    stdout.write_all(&bytes)?;
    stdout.flush()?;
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::prompt_spacer_bytes;

    #[test]
    fn prompt_spacer_has_marker_and_clear() {
        let bytes = prompt_spacer_bytes(4);
        assert!(bytes.starts_with(b"\x1b[7m$\x1b[0m"));
        assert!(bytes.ends_with(b"\r\x1b[J"));
        assert!(bytes.contains(&b' '));
    }
}
