# unshell Specification

## Guiding Principles
- Minimal core implementation in Rust; everything that can live in user space (string helpers, globbing, fancy UX) does.
- Arguments are atomic unless the user explicitly re-parses data.
- Composability beats magic syntax: every transformation should look like ordinary command plumbing.

## Status
- **Implemented:** Minimal Rust REPL that reads lines, runs commands via `$PATH`, prints a prompt, and supports `exit`.
- **Next up:** argument parser that respects atomic variables/`...`, square-bracket captures, control-flow keywords (`if`, `for`, `foreach`), newline trimming toggles, and the expansion handler contract.

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
Tokens that start with `[` and contain more than one character run as captures: `[cmd args]` executes `cmd`, captures stdout, trims a single trailing newline (configurable), and injects the result as one argument. A lone `[` continues to execute `/usr/bin/[` just like any other binary in `$PATH`. To avoid conflicts in strings (e.g., `echo "[module 7]"`), capture recognition only occurs for unquoted tokens; inside quotes the brackets are literal. Classic `$()` is still accepted everywhere (including inside strings), and `$[]` is treated the same as `[]` for callers that prefer explicit sigils mid-line.
```bash
cp [which python3] ./bin/python-system
hash="sha256:[sha256sum Cargo.lock]"
```

### Configurable Trailing-Newline Trimming
**Difficulty:** Easy  
Because many programs emit terminal newlines, captures strip exactly one trailing `\n` by default. A shell option toggles the behavior at runtime for workflows that need raw output. Proposed syntax: `set subshells.trim_newline false` (and `true` to re-enable).
```bash
set subshells.trim_newline false
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
Blocks are introduced by keywords (`if`, `else`, `for`, `foreach`) followed by either a newline with indentation **using hard tabs only** (Python-style but without spaces) **or** an inline brace-delimited block. Authors can mix styles per block, but indentation inside braces is still recommended for clarity.
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
`cmd | foreach name` treats stdin as a stream of records (newline-delimited), assigning each trimmed line to `name` and executing the indented block for each row. The block **does not** implicitly forward the original line; authors must `echo` (or otherwise emit) data if downstream stages should receive anything. `foreach` composes naturally inside pipelines, albeit in a child process, meaning mutations do not leak to the parent shell.
```bash
ls -1 | foreach file
	cp $file ../backup/
    echo $file
| grep ".txt"
```

### External Expansion Handlers
**Difficulty:** Hard  
Globs, brace expansion, or other sigils are delegated to a single user-space helper configured via `set`. Callers enable the characters that should trigger expansion (`set expansions.characters "@" on`, `set expansions.characters "{" on`, etc.), then point the shell at the handler binary and its fixed leading arguments (`set expansions.handler foo bar baz`). When the parser sees any unescaped, unquoted token containing a registered character, it invokes the handler as `foo bar baz <token>` and expects a JSON array of replacement arguments. The helper is responsible for parsing mixed syntax (e.g., both `@` and `{}` inside one token) and deciding how to expand it. This keeps the core small while letting users swap expansion strategies without recompiling the shell.
```bash
# Example configuration
set expansions.characters "@" on
set expansions.characters "{" on
set expansions.handler ush-expand --mode glob

# Example handler invocation
echo foo@bar{.txt,.log}
# -> shell calls: ush-expand --mode glob "foo@bar{.txt,.log}"
# -> handler prints JSON array such as ["foo@bar.txt","foo@bar.log"]
```

### External String & Quoting Utilities
**Difficulty:** Medium  
Utilities such as `s` (string transforms) and `quote` (turn newline-separated input into properly quoted shell tokens) ship as standalone Rust binaries. `quote`'s contract: read stdin, treat each line as a record, emit a space-separated list where each record is wrapped in quotes and internal quotes/backslashes are escaped so `...` can safely re-tokenize the output. Additional helpers (like a configurable-delimiter `split`) can exist in user space, but the shell core remains agnostic.
```bash
title=$(s $raw_title trim)
ls ...[ls | quote]
for path in ...[cat files.list | quote]
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
