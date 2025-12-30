# unshell Specification

## Guiding Principles
- Minimal core implementation in Rust; everything that can live in user space (string helpers, globbing, fancy UX) does.
- Arguments are atomic unless the user explicitly re-parses data.
- Composability beats magic syntax: every transformation should look like ordinary command plumbing.

## Status
- **Implemented:** `ush` builds via Cargo/Makefile, executes plaintext manifests line-by-line (with `exit` handling), supports square-bracket captures (including inline concatenation and nesting, while `[ ` stays literal), handles `$[]`/`$()` captures inside double-quoted strings, evaluates tab-indented `if` blocks in scripts, provides atomic variable assignment/expansion, supports the `...` spread operator, and runs tab-indented `for` loops in scripts. Integration fixtures cover the current surface area (`cargo test`).
- **Next up:** `foreach` control flow, newline trimming toggles, and the expansion handler contract.

## Features

### Execution & Pipes
**Difficulty:** Easy  
Commands run left-to-right using standard fork/exec and POSIX-style pipes. There is no special syntax beyond `cmd arg | next_cmd`, and grouping still relies on parentheses for precedence without creating subshell semantics by default.
```bash
ls /var/log | grep error | tail -n 20
```

### Atomic Variables & Assignment
**Difficulty:** Medium  
`$var` always expands to exactly one argument, preserving embedded spaces and newlines; `name=value` mutates shell-local state while `export name=value` promotes it to the environment of child processes. No implicit splitting or globbing happens during expansion. **Current implementation:** leading `name=value` tokens set shell-local variables (no command runs if only assignments are present), `$var` expands in unquoted tokens (including inline), and lookup checks shell-local variables before falling back to the process environment.
```bash
path="My Projects/unshell"
cd $path          # treated as one argument
export EDITOR=vim # propagated to children
```

### Spread Operator `...`
**Difficulty:** Hard  
`...` re-tokenizes a string using the shell's own parser. `...$var` or `...[cmd]` behaves like `eval` in place: quotes, escapes, and whitespace in the source are honored so the caller controls splitting. This is how legacy behaviors (space splitting, inline scripts) are opt-in. **Current implementation:** unquoted tokens starting with `...` re-tokenize their suffix after expanding captures/variables. This is distinct from inline capture concatenation (`a[pwd]`), which stays a single argument unless you explicitly use `...`.
```bash
files="-la ./bin \"./my file.txt\""
ls ...$files
```

### Command Substitution with Square Brackets
**Difficulty:** Medium  
Tokens that start with `[` and contain more than one character run as captures: `[cmd args]` executes `cmd`, captures stdout, trims a single trailing newline (configurable), and injects the result as one argument. A lone `[` continues to execute `/usr/bin/[` just like any other binary in `$PATH`. Capture recognition only triggers when `[` is immediately followed by a non-whitespace character, so `[ test -f foo ]` continues to invoke `/usr/bin/[` and `[ ]` stays literal. Unquoted tokens can concatenate captures with surrounding text (e.g., `foo-[echo bar]`), and captures may nest (e.g., `[echo [pwd]]`). To avoid conflicts in strings (e.g., `echo "[module 7]"`), capture recognition only occurs for unquoted tokens; inside quotes the brackets are literal. Classic `$()` is still accepted everywhere (including inside strings), and `$[]` is treated the same as `[]` for callers that prefer explicit sigils mid-line.
```bash
cp [which python3] ./bin/python-system
hash="sha256:[sha256sum Cargo.lock]"
```

### Double-Quoted Strings & Capture Interpolation
**Difficulty:** Medium  
Double quotes group tokens without splitting, allow escaping via `\"`, and support inline captures using `$[]` or `$()` plus `$var` interpolation. Anything else (including bare `[]`) remains literal inside strings. The shell evaluates captures, trims the trailing newline, and splices the result directly into the string without introducing extra splitting; `$var` expands to its full value without trimming. **Single quotes** are fully literal: no `$var`, `$()`, or `$[]` expansion happens inside them.
```bash
echo "hello $[echo world]" "$[pwd]"
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
Blocks are introduced by keywords (`if`, `else`, `for`, `foreach`) followed by either a newline with indentation **using hard tabs only** (Python-style but without spaces) **or** an inline brace-delimited block. Authors can mix styles per block, but indentation inside braces is still recommended for clarity. **Current implementation:** tab-indented `if` blocks within scripts (no `else`/loops yet).
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
**Current implementation:** tab-indented `for name in ...` blocks inside scripts, reusing the existing tab-indented block execution model.

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
- **Else/loop semantics:** What syntax/behavior should `else`, `elif`, `for`, and `foreach` follow (indentation vs braces priorities, variable scoping, etc.)?
