use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone)]
pub struct ShellOptions {
    pub aliases_recursive: bool,
    pub subshells_trim_newline: bool,
    pub expansions_chars: HashSet<char>,
    pub expansions_handler: Vec<String>,
}

#[derive(Clone)]
pub enum FunctionBody {
    Inline(String),
    Block(Vec<String>),
}

#[derive(Clone)]
pub struct FunctionDef {
    pub body: FunctionBody,
}

#[derive(Clone)]
pub struct AliasEntry {
    pub value: String,
    pub global: bool,
}

#[derive(Clone)]
pub struct ReplBinding {
    pub key: String,
    pub action: String,
}

#[derive(Clone, Copy)]
pub enum ReplCompletionMode {
    Fzf,
    List,
    Off,
}

#[derive(Clone)]
pub struct ReplOptions {
    pub vi_mode: bool,
    pub completion_mode: ReplCompletionMode,
    pub prompt_command: Option<String>,
    pub bindings: Vec<ReplBinding>,
    pub bracketed_paste: bool,
    pub history_file: Option<String>,
    pub generation: u64,
}

pub struct ShellState {
    pub vars: HashMap<String, String>,
    pub locals_stack: Vec<HashMap<String, String>>,
    pub functions: HashMap<String, FunctionDef>,
    pub aliases: HashMap<String, AliasEntry>,
    pub options: ShellOptions,
    pub repl: ReplOptions,
    pub interactive: bool,
    pub last_output_newline: bool,
    pub needs_cursor_check: bool,
    pub positional: Vec<String>,
    pub positional_stack: Vec<Vec<String>>,
    pub last_status: i32,
}

impl ShellState {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            locals_stack: Vec::new(),
            functions: HashMap::new(),
            aliases: HashMap::new(),
            options: ShellOptions {
                aliases_recursive: true,
                subshells_trim_newline: true,
                expansions_chars: HashSet::new(),
                expansions_handler: Vec::new(),
            },
            repl: ReplOptions {
                vi_mode: true,
                completion_mode: ReplCompletionMode::Fzf,
                prompt_command: None,
                bindings: Vec::new(),
                bracketed_paste: false,
                history_file: None,
                generation: 0,
            },
            interactive: false,
            last_output_newline: true,
            needs_cursor_check: false,
            positional: Vec::new(),
            positional_stack: Vec::new(),
            last_status: 0,
        }
    }

    pub fn set_var(&mut self, name: &str, value: String) {
        self.vars.insert(name.to_string(), value);
    }

    pub fn set_local_var(&mut self, name: &str, value: String) -> Result<(), String> {
        if let Some(frame) = self.locals_stack.last_mut() {
            frame.insert(name.to_string(), value);
            return Ok(());
        }
        Err("local: not in function".into())
    }

    pub fn set_alias(&mut self, name: &str, value: String, global: bool) {
        self.aliases.insert(
            name.to_string(),
            AliasEntry {
                value,
                global,
            },
        );
    }

    pub fn remove_alias(&mut self, name: &str) -> bool {
        self.aliases.remove(name).is_some()
    }
}

pub struct TempFileGuard {
    path: PathBuf,
}

impl TempFileGuard {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

impl Drop for TempFileGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

pub fn write_locals_file(state: &ShellState) -> io::Result<(PathBuf, TempFileGuard)> {
    let (path, mut file) = create_temp_file("locals")?;
    let count = state.vars.len() as u32;
    write_u32(&mut file, count)?;
    for (name, value) in state.vars.iter() {
        write_string(&mut file, name)?;
        write_string(&mut file, value)?;
    }
    let alias_count = state.aliases.len() as u32;
    write_u32(&mut file, alias_count)?;
    for (name, value) in state.aliases.iter() {
        write_string(&mut file, name)?;
        write_string(&mut file, &value.value)?;
        write_bool(&mut file, value.global)?;
    }
    write_bool(&mut file, state.options.aliases_recursive)?;
    write_bool(&mut file, state.options.subshells_trim_newline)?;
    let mut chars: Vec<char> = state.options.expansions_chars.iter().copied().collect();
    chars.sort_unstable();
    let chars_string: String = chars.into_iter().collect();
    write_string(&mut file, &chars_string)?;
    write_u32(&mut file, state.options.expansions_handler.len() as u32)?;
    for value in state.options.expansions_handler.iter() {
        write_string(&mut file, value)?;
    }
    write_u32(&mut file, state.locals_stack.len() as u32)?;
    for frame in state.locals_stack.iter() {
        write_u32(&mut file, frame.len() as u32)?;
        for (name, value) in frame.iter() {
            write_string(&mut file, name)?;
            write_string(&mut file, value)?;
        }
    }
    write_u32(&mut file, state.functions.len() as u32)?;
    for (name, func) in state.functions.iter() {
        write_string(&mut file, name)?;
        match &func.body {
            FunctionBody::Inline(body) => {
                write_bool(&mut file, true)?;
                write_string(&mut file, body)?;
            }
            FunctionBody::Block(lines) => {
                write_bool(&mut file, false)?;
                write_u32(&mut file, lines.len() as u32)?;
                for line in lines {
                    write_string(&mut file, line)?;
                }
            }
        }
    }
    write_u32(&mut file, state.positional.len() as u32)?;
    for value in state.positional.iter() {
        write_string(&mut file, value)?;
    }
    write_i32(&mut file, state.last_status)?;
    write_bool(&mut file, state.interactive)?;
    file.flush()?;
    Ok((path.clone(), TempFileGuard::new(path)))
}

pub fn read_locals_file(path: &Path) -> io::Result<ShellState> {
    let mut file = File::open(path)?;
    let count = read_u32(&mut file)?;
    let mut state = ShellState::new();
    for _ in 0..count {
        let name = read_string(&mut file, "locals name")?;
        let value = read_string(&mut file, "locals value")?;
        state.set_var(&name, value);
    }
    let alias_count = read_u32(&mut file)?;
    for _ in 0..alias_count {
        let name = read_string(&mut file, "alias name")?;
        let value = read_string(&mut file, "alias value")?;
        let global = read_bool(&mut file)?;
        state.set_alias(&name, value, global);
    }
    state.options.aliases_recursive = read_bool(&mut file)?;
    state.options.subshells_trim_newline = read_bool(&mut file)?;
    let chars_string = read_string(&mut file, "expansion chars")?;
    state.options.expansions_chars = chars_string.chars().collect();
    let handler_len = read_u32(&mut file)? as usize;
    state.options.expansions_handler = Vec::with_capacity(handler_len);
    for _ in 0..handler_len {
        let value = read_string(&mut file, "expansion handler")?;
        state.options.expansions_handler.push(value);
    }
    let locals_count = read_u32(&mut file)? as usize;
    state.locals_stack = Vec::with_capacity(locals_count);
    for _ in 0..locals_count {
        let frame_len = read_u32(&mut file)? as usize;
        let mut frame = HashMap::with_capacity(frame_len);
        for _ in 0..frame_len {
            let name = read_string(&mut file, "locals name")?;
            let value = read_string(&mut file, "locals value")?;
            frame.insert(name, value);
        }
        state.locals_stack.push(frame);
    }
    let funcs_len = read_u32(&mut file)? as usize;
    for _ in 0..funcs_len {
        let name = read_string(&mut file, "function name")?;
        let inline = read_bool(&mut file)?;
        let body = if inline {
            let body = read_string(&mut file, "function body")?;
            FunctionBody::Inline(body)
        } else {
            let line_count = read_u32(&mut file)? as usize;
            let mut lines = Vec::with_capacity(line_count);
            for _ in 0..line_count {
                let line = read_string(&mut file, "function line")?;
                lines.push(line);
            }
            FunctionBody::Block(lines)
        };
        state
            .functions
            .insert(name, FunctionDef { body });
    }
    let positional_len = read_u32(&mut file)? as usize;
    state.positional = Vec::with_capacity(positional_len);
    for _ in 0..positional_len {
        let value = read_string(&mut file, "positional arg")?;
        state.positional.push(value);
    }
    state.last_status = read_i32(&mut file)?;
    state.interactive = read_bool(&mut file)?;
    Ok(state)
}

const STATE_MAGIC: &[u8] = b"USHSTATE";
const STATE_VERSION: u32 = 1;

pub fn write_shell_state_file(state: &ShellState) -> io::Result<(PathBuf, TempFileGuard)> {
    let (path, mut file) = create_temp_file("state")?;
    file.write_all(STATE_MAGIC)?;
    write_u32(&mut file, STATE_VERSION)?;

    write_u32(&mut file, state.vars.len() as u32)?;
    for (name, value) in state.vars.iter() {
        write_string(&mut file, name)?;
        write_string(&mut file, value)?;
    }
    write_u32(&mut file, state.aliases.len() as u32)?;
    for (name, value) in state.aliases.iter() {
        write_string(&mut file, name)?;
        write_string(&mut file, &value.value)?;
        write_bool(&mut file, value.global)?;
    }
    write_bool(&mut file, state.options.aliases_recursive)?;
    write_bool(&mut file, state.options.subshells_trim_newline)?;
    let mut chars: Vec<char> = state.options.expansions_chars.iter().copied().collect();
    chars.sort_unstable();
    let chars_string: String = chars.into_iter().collect();
    write_string(&mut file, &chars_string)?;
    write_u32(&mut file, state.options.expansions_handler.len() as u32)?;
    for value in state.options.expansions_handler.iter() {
        write_string(&mut file, value)?;
    }
    write_u32(&mut file, state.locals_stack.len() as u32)?;
    for frame in state.locals_stack.iter() {
        write_u32(&mut file, frame.len() as u32)?;
        for (name, value) in frame.iter() {
            write_string(&mut file, name)?;
            write_string(&mut file, value)?;
        }
    }
    write_u32(&mut file, state.functions.len() as u32)?;
    for (name, func) in state.functions.iter() {
        write_string(&mut file, name)?;
        match &func.body {
            FunctionBody::Inline(body) => {
                write_bool(&mut file, true)?;
                write_string(&mut file, body)?;
            }
            FunctionBody::Block(lines) => {
                write_bool(&mut file, false)?;
                write_u32(&mut file, lines.len() as u32)?;
                for line in lines {
                    write_string(&mut file, line)?;
                }
            }
        }
    }
    write_u32(&mut file, state.positional.len() as u32)?;
    for value in state.positional.iter() {
        write_string(&mut file, value)?;
    }
    write_i32(&mut file, state.last_status)?;
    write_bool(&mut file, state.interactive)?;

    write_bool(&mut file, state.repl.vi_mode)?;
    write_u32(
        &mut file,
        match state.repl.completion_mode {
            ReplCompletionMode::Fzf => 0,
            ReplCompletionMode::List => 1,
            ReplCompletionMode::Off => 2,
        },
    )?;
    write_bool(&mut file, state.repl.bracketed_paste)?;
    write_u32(&mut file, state.repl.bindings.len() as u32)?;
    for binding in state.repl.bindings.iter() {
        write_string(&mut file, &binding.key)?;
        write_string(&mut file, &binding.action)?;
    }
    match state.repl.prompt_command.as_ref() {
        Some(value) => {
            write_bool(&mut file, true)?;
            write_string(&mut file, value)?;
        }
        None => {
            write_bool(&mut file, false)?;
        }
    }
    match state.repl.history_file.as_ref() {
        Some(value) => {
            write_bool(&mut file, true)?;
            write_string(&mut file, value)?;
        }
        None => {
            write_bool(&mut file, false)?;
        }
    }
    write_u64(&mut file, state.repl.generation)?;
    write_bool(&mut file, state.last_output_newline)?;
    write_bool(&mut file, state.needs_cursor_check)?;
    file.flush()?;
    Ok((path.clone(), TempFileGuard::new(path)))
}

pub fn read_shell_state_file(path: &Path) -> io::Result<ShellState> {
    let mut file = File::open(path)?;
    let mut magic = vec![0u8; STATE_MAGIC.len()];
    file.read_exact(&mut magic)?;
    if magic != STATE_MAGIC {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "state file header mismatch",
        ));
    }
    let version = read_u32(&mut file)?;
    if version != STATE_VERSION {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "state file version mismatch",
        ));
    }
    let mut state = ShellState::new();
    let count = read_u32(&mut file)? as usize;
    for _ in 0..count {
        let name = read_string(&mut file, "locals name")?;
        let value = read_string(&mut file, "locals value")?;
        state.set_var(&name, value);
    }
    let alias_count = read_u32(&mut file)? as usize;
    for _ in 0..alias_count {
        let name = read_string(&mut file, "alias name")?;
        let value = read_string(&mut file, "alias value")?;
        let global = read_bool(&mut file)?;
        state.set_alias(&name, value, global);
    }
    state.options.aliases_recursive = read_bool(&mut file)?;
    state.options.subshells_trim_newline = read_bool(&mut file)?;
    let chars_string = read_string(&mut file, "expansion chars")?;
    state.options.expansions_chars = chars_string.chars().collect();
    let handler_len = read_u32(&mut file)? as usize;
    state.options.expansions_handler = Vec::with_capacity(handler_len);
    for _ in 0..handler_len {
        let value = read_string(&mut file, "expansion handler")?;
        state.options.expansions_handler.push(value);
    }
    let locals_count = read_u32(&mut file)? as usize;
    state.locals_stack = Vec::with_capacity(locals_count);
    for _ in 0..locals_count {
        let frame_len = read_u32(&mut file)? as usize;
        let mut frame = HashMap::with_capacity(frame_len);
        for _ in 0..frame_len {
            let name = read_string(&mut file, "locals name")?;
            let value = read_string(&mut file, "locals value")?;
            frame.insert(name, value);
        }
        state.locals_stack.push(frame);
    }
    let funcs_len = read_u32(&mut file)? as usize;
    for _ in 0..funcs_len {
        let name = read_string(&mut file, "function name")?;
        let inline = read_bool(&mut file)?;
        let body = if inline {
            let body = read_string(&mut file, "function body")?;
            FunctionBody::Inline(body)
        } else {
            let line_count = read_u32(&mut file)? as usize;
            let mut lines = Vec::with_capacity(line_count);
            for _ in 0..line_count {
                let line = read_string(&mut file, "function line")?;
                lines.push(line);
            }
            FunctionBody::Block(lines)
        };
        state
            .functions
            .insert(name, FunctionDef { body });
    }
    let positional_len = read_u32(&mut file)? as usize;
    state.positional = Vec::with_capacity(positional_len);
    for _ in 0..positional_len {
        let value = read_string(&mut file, "positional arg")?;
        state.positional.push(value);
    }
    state.last_status = read_i32(&mut file)?;
    state.interactive = read_bool(&mut file)?;

    state.repl.vi_mode = read_bool(&mut file)?;
    state.repl.completion_mode = match read_u32(&mut file)? {
        0 => ReplCompletionMode::Fzf,
        1 => ReplCompletionMode::List,
        2 => ReplCompletionMode::Off,
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "invalid repl completion mode",
            ))
        }
    };
    state.repl.bracketed_paste = read_bool(&mut file)?;
    let binding_len = read_u32(&mut file)? as usize;
    state.repl.bindings = Vec::with_capacity(binding_len);
    for _ in 0..binding_len {
        let key = read_string(&mut file, "repl binding key")?;
        let action = read_string(&mut file, "repl binding action")?;
        state.repl.bindings.push(ReplBinding { key, action });
    }
    let has_prompt = read_bool(&mut file)?;
    if has_prompt {
        state.repl.prompt_command = Some(read_string(&mut file, "repl prompt")?);
    }
    let has_history = read_bool(&mut file)?;
    if has_history {
        state.repl.history_file = Some(read_string(&mut file, "repl history")?);
    }
    state.repl.generation = read_u64(&mut file)?;
    state.last_output_newline = read_bool(&mut file)?;
    state.needs_cursor_check = read_bool(&mut file)?;

    Ok(state)
}

pub fn lookup_var(state: &ShellState, name: &str) -> String {
    for frame in state.locals_stack.iter().rev() {
        if let Some(value) = frame.get(name) {
            return value.clone();
        }
    }
    if let Some(value) = state.vars.get(name) {
        return value.clone();
    }

    env::var(name).unwrap_or_default()
}

pub fn create_temp_file(prefix: &str) -> io::Result<(PathBuf, File)> {
    let dir = env::temp_dir();
    let pid = std::process::id();
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();

    for attempt in 0..1000 {
        let name = format!("ush-{prefix}-{pid}-{now}-{attempt}");
        let path = dir.join(name);
        match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
        {
            Ok(file) => return Ok((path, file)),
            Err(err) if err.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(err) => return Err(err),
        }
    }

    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "failed to create temp file",
    ))
}

fn write_u32<W: Write>(writer: &mut W, value: u32) -> io::Result<()> {
    writer.write_all(&value.to_le_bytes())
}

fn read_u32<R: Read>(reader: &mut R) -> io::Result<u32> {
    let mut buf = [0u8; 4];
    reader.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

fn write_u64<W: Write>(writer: &mut W, value: u64) -> io::Result<()> {
    writer.write_all(&value.to_le_bytes())
}

fn read_u64<R: Read>(reader: &mut R) -> io::Result<u64> {
    let mut buf = [0u8; 8];
    reader.read_exact(&mut buf)?;
    Ok(u64::from_le_bytes(buf))
}

fn write_bool<W: Write>(writer: &mut W, value: bool) -> io::Result<()> {
    writer.write_all(&[if value { 1 } else { 0 }])
}

fn read_bool<R: Read>(reader: &mut R) -> io::Result<bool> {
    let mut buf = [0u8; 1];
    reader.read_exact(&mut buf)?;
    Ok(buf[0] != 0)
}

fn write_i32<W: Write>(writer: &mut W, value: i32) -> io::Result<()> {
    writer.write_all(&value.to_le_bytes())
}

fn read_i32<R: Read>(reader: &mut R) -> io::Result<i32> {
    let mut buf = [0u8; 4];
    reader.read_exact(&mut buf)?;
    Ok(i32::from_le_bytes(buf))
}

fn write_string<W: Write>(writer: &mut W, value: &str) -> io::Result<()> {
    write_u32(writer, value.len() as u32)?;
    writer.write_all(value.as_bytes())
}

fn read_string<R: Read>(reader: &mut R, label: &str) -> io::Result<String> {
    let value_len = read_u32(reader)? as usize;
    let mut value_buf = vec![0u8; value_len];
    reader.read_exact(&mut value_buf)?;
    String::from_utf8(value_buf).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("{label} not utf-8"),
        )
    })
}
