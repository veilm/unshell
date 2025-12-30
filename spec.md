# unshell Specification

## Guiding Principles
- Minimal core implementation in Rust; everything that can live in user space (string helpers, globbing, fancy UX) does.
- Arguments are atomic unless the user explicitly re-parses data.
- Composability beats magic syntax: every transformation should look like ordinary command plumbing.

## Features

### Execution & Pipes
**Difficulty:** Easy  
Commands run left-to-right using standard fork/exec and POSIX-style pipes. There is no special syntax beyond `cmd arg | next_cmd`, and grouping still relies on parentheses for precedence without creating subshell semantics by default.
```bash
ls /var/log | grep error | tail -n 20
```

### Atomic Variables & Assignment
**Difficulty:** Medium  
`$var` always expands to exactly one argument, preserving embedded spaces and newlines; `name=value` mutates shell-local state while `export name=value` promotes it to the environment of child processes. No implicit splitting or globbing happens during expansion.
```bash
path="My Projects/unshell"
cd $path          # treated as one argument
export EDITOR=vim # propagated to children
```

### Spread Operator `...`
**Difficulty:** Hard  
`...` re-tokenizes a string using the shell's own parser. `...$var` or `...[cmd]` behaves like `eval` in place: quotes, escapes, and whitespace in the source are honored so the caller controls splitting. This is how legacy behaviors (space splitting, inline scripts) are opt-in.
```bash
files="-la ./bin \"./my file.txt\""
ls ...$files
```

### Command Substitution with Square Brackets
**Difficulty:** Medium  
Tokens that start with `[` and contain more than one character run as captures: `[cmd args]` executes `cmd`, captures stdout, trims a single trailing newline (configurable), and injects the result as one argument. A lone `[` continues to execute `/usr/bin/[` for compatibility. To avoid conflicts in strings (e.g., `echo "[module 7]"`), capture recognition only occurs for unquoted tokens; inside quotes the brackets are literal. Classic `$()` is still accepted everywhere, and `$[]` is treated the same as `[]` for callers that prefer explicit sigils.
```bash
cp [which python3] ./bin/python-system
hash="sha256:[sha256sum Cargo.lock]"
```

### Configurable Trailing-Newline Trimming
**Difficulty:** Easy  
Because many programs emit terminal newlines, captures strip exactly one trailing `\n` by default. A shell option (e.g., `set capture.trim_newline=false`) toggles the behavior at runtime for workflows that need raw output.
```bash
set capture.trim_newline=false
printf "[cat banner.bin]"
```

### Explicit `eval` Command
**Difficulty:** Easy  
`eval` takes a single argument, runs it through the parser, and executes the resulting command sequence. This makes dynamic dispatch possible even when the callee is indirect.
```bash
cmd=eval
payload='touch "generated file.txt"'
$cmd $payload
```

### Grouping with Parentheses
**Difficulty:** Easy  
Parentheses still group pipelines without invoking capture semantics, letting users control precedence or isolate redirections without new processes unless the OS requires them.
```bash
(cat foo && cat bar) | grep TODO
```

### Control Flow Blocks
**Difficulty:** Hard  
Blocks are introduced by keywords (`if`, `else`, `for`, `foreach`) followed by either a newline with indentation (tabs or consistent 4-space groups, Python-style) **or** an inline brace-delimited block. Authors can mix styles per block, but indentation inside braces is still recommended for clarity.
```bash
if test -f config.toml
    echo "config exists"
else
    echo "missing config"

if test -f config.toml {
    echo "config exists"
}
```

### Argument Loops: `for … in`
**Difficulty:** Medium  
`for name in arglist` iterates over a fully realized list of arguments (typically produced with `...`). Each iteration binds `name` without exporting it.
```bash
for server in ...[cat servers.list | quote]
    ssh $server uptime
```

### Stream Loops: `foreach`
**Difficulty:** Hard  
`cmd | foreach name` treats stdin as a stream of records (newline-delimited), assigning each trimmed line to `name` and executing the indented block for each row. The block decides what to emit downstream (e.g., `echo $name`) so `foreach` composes naturally inside pipelines, albeit in a child process, meaning mutations do not leak to the parent shell.
```bash
ls -1 | foreach file
    cp $file ../backup/
    echo $file
| grep ".txt"
```

### External Expansion Handlers
**Difficulty:** Hard  
Globs, brace expansion, or other sigils are delegated to user-space helpers registered via configuration (e.g., mapping `{` to `esh-expander curly`). When the parser sees a trigger, it runs the helper with the raw token and expects a JSON array of replacement arguments. This keeps the core small while letting users add or remove expansions without recompiling the shell.
```bash
# Example protocol (pseudo):
esh-expander glob "*.[ch]"
# -> ["main.c","util.h"]
```

### External String & Quoting Utilities
**Difficulty:** Medium  
Utilities such as `s` (string transforms), `split` (delimiter-to-newline rewrites), and `quote` (turn newline-separated input into properly quoted shell tokens) ship as standalone Rust binaries. Users can replace or omit them entirely. They enable ergonomic transformations without inflating the shell.
```bash
title=$(s $raw_title trim)
ls ...[ls | quote]
for path in ...[split "\n" $LIST]
    rm $path
```

### Minimal Built-ins & Aliases
**Difficulty:** Easy  
The shell ships only what it must: `cd`, `alias`, `unalias`, `set`, `export`, and the control keywords. Everything else is expected to be an external binary or script so users can curate their environment and keep the core auditable.
```bash
alias ll="ls -la"
unalias ll
cd /srv/www
```

## Ambiguities / Open Questions
- **Error handling mode:** Should non-zero exit codes inside pipelines or blocks abort the script (akin to `set -e`) or only fail the current step?
- **`foreach` pass-through default:** Do we implicitly forward each input line when the block produces no output, or is echoing required to keep downstream stages fed?
- **Indentation rules:** Are tabs and spaces freely mixable if they result in the same visual width, or should the parser enforce a single style per file/session?
- **Capture newline option interface:** What is the exact syntax for toggling newline trimming (e.g., `set capture.trim_newline=false` vs `set capture trim=false`)?
- **Expansion handler registration:** How does a user declare which helper handles which sigil, and can multiple handlers chain for the same token?
- **`quote`/`split` contract:** What exact escaping rules do these helpers follow so `...` can parse their output safely, especially for control characters?
- **Literal `[` command ergonomics:** Do we need a shorthand for running `/usr/bin/[`, or is requiring quotes/aliases acceptable for scripts that still call it directly?
- **Capture tokens within quotes:** Current rule is “captures are recognized only for unquoted tokens starting with `[`”; do we still want aliases like `$()`/`$[]` active inside strings, or should quotes always suppress substitution?
