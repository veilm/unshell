

pub fn parse_args(line: &str) -> Result<Vec<String>, String> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut in_double = false;
    let mut in_single = false;
    let mut prev_was_space = true;
    let mut chars = line.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '\'' if bracket_depth == 0 && paren_depth == 0 && !in_double && !in_single => {
                in_single = true;
                current.push(ch);
                prev_was_space = false;
            }
            '\'' if in_single => {
                in_single = false;
                current.push(ch);
                prev_was_space = false;
            }
            '"' if bracket_depth == 0 && paren_depth == 0 && !in_double => {
                in_double = true;
                current.push(ch);
                prev_was_space = false;
            }
            '"' if in_double && paren_depth == 0 => {
                in_double = false;
                current.push(ch);
                prev_was_space = false;
            }
            '\\' if in_double && paren_depth == 0 => {
                if let Some(next) = chars.next() {
                    current.push(ch);
                    current.push(next);
                    prev_was_space = false;
                }
            }
            '#' if !in_double
                && !in_single
                && bracket_depth == 0
                && paren_depth == 0
                && prev_was_space =>
            {
                break;
            }
            '$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if matches!(chars.peek(), Some('(')) {
                    chars.next();
                    paren_depth = 1;
                    current.push('$');
                    current.push('(');
                    prev_was_space = false;
                } else {
                    current.push(ch);
                    prev_was_space = false;
                }
            }
            '[' if !in_double && !in_single && bracket_depth == 0 => {
                if matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                    current.push(ch);
                    prev_was_space = false;
                } else {
                    bracket_depth = 1;
                    current.push(ch);
                    prev_was_space = false;
                }
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
                current.push(ch);
                prev_was_space = false;
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                current.push(ch);
                prev_was_space = false;
            }
            '(' if paren_depth > 0 => {
                paren_depth += 1;
                current.push(ch);
                prev_was_space = false;
            }
            ')' if paren_depth > 0 => {
                paren_depth -= 1;
                current.push(ch);
                prev_was_space = false;
            }
            ch if ch.is_whitespace() && !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 =>
            {
                if !current.is_empty() {
                    args.push(current);
                    current = String::new();
                }
                prev_was_space = true;
            }
            ch if ch.is_whitespace() => {
                current.push(ch);
                prev_was_space = true;
            }
            _ => {
                current.push(ch);
                prev_was_space = false;
            }
        }
    }

    if in_double {
        return Err("unterminated string".into());
    }

    if in_single {
        return Err("unterminated string".into());
    }

    if !current.is_empty() {
        args.push(current);
    }

    Ok(args)
}

pub fn split_on_semicolons(line: &str) -> Vec<String> {
    split_by_char(line, ';')
}

pub fn split_on_pipes(line: &str) -> Vec<String> {
    split_by_char(line, '|')
}

pub fn append_pipeline_tail(after: &mut Vec<String>, tail: &str) -> Result<(), String> {
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

pub fn unindent_block_lines(lines: &[String]) -> Vec<String> {
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

pub enum LogicOp {
    None,
    And,
    Or,
}

fn split_by_char(line: &str, delimiter: char) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_double = false;
    let mut in_single = false;
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut chars = line.chars().peekable();
    let mut prev_was_space = true;

    while let Some(ch) = chars.next() {
        if in_double && ch == '\\' && paren_depth == 0 {
            current.push(ch);
            if let Some(next) = chars.next() {
                current.push(next);
            }
            prev_was_space = false;
            continue;
        }

        match ch {
            '\'' if !in_double && bracket_depth == 0 && paren_depth == 0 => {
                in_single = !in_single;
                current.push(ch);
                prev_was_space = false;
            }
            '"' if !in_single && bracket_depth == 0 && paren_depth == 0 => {
                in_double = !in_double;
                current.push(ch);
                prev_was_space = false;
            }
            '#' if !in_double
                && !in_single
                && bracket_depth == 0
                && paren_depth == 0
                && prev_was_space =>
            {
                break;
            }
            '$' if !in_double && !in_single && bracket_depth == 0 && paren_depth == 0 => {
                if matches!(chars.peek(), Some('(')) {
                    chars.next();
                    paren_depth = 1;
                    current.push('$');
                    current.push('(');
                    prev_was_space = false;
                } else {
                    current.push(ch);
                    prev_was_space = false;
                }
            }
            '[' if !in_double && !in_single && bracket_depth == 0 => {
                if matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                    current.push(ch);
                    prev_was_space = false;
                } else {
                    bracket_depth = 1;
                    current.push(ch);
                    prev_was_space = false;
                }
            }
            '[' if bracket_depth > 0 => {
                bracket_depth += 1;
                current.push(ch);
                prev_was_space = false;
            }
            ']' if bracket_depth > 0 => {
                bracket_depth -= 1;
                current.push(ch);
                prev_was_space = false;
            }
            '(' if paren_depth > 0 => {
                paren_depth += 1;
                current.push(ch);
                prev_was_space = false;
            }
            ')' if paren_depth > 0 => {
                paren_depth -= 1;
                current.push(ch);
                prev_was_space = false;
            }
            ch if ch == delimiter
                && !in_double
                && !in_single
                && bracket_depth == 0
                && paren_depth == 0 =>
            {
                parts.push(current);
                current = String::new();
                prev_was_space = true;
            }
            ch if ch.is_whitespace() => {
                current.push(ch);
                prev_was_space = true;
            }
            _ => {
                current.push(ch);
                prev_was_space = false;
            }
        }
    }

    parts.push(current);
    parts
}

pub enum BraceParse {
    Inline {
        head: String,
        body: String,
        tail: Option<String>,
    },
    Open { head: String, tail: String },
    None { head: String },
}

pub fn parse_brace_block(line: &str) -> BraceParse {
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

pub fn collect_brace_block(
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

pub fn parse_foreach_line(
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

pub fn split_indent(line: &str) -> (usize, &str) {
    let bytes = line.as_bytes();
    let mut idx = 0;
    while idx < bytes.len() && bytes[idx] == b'\t' {
        idx += 1;
    }
    (idx, &line[idx..])
}
