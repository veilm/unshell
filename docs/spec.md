# unshell Specification

## Guiding Principles
- Minimal core implementation in Rust; everything that can live in user space (string helpers, globbing, fancy UX) does.
- Arguments are atomic unless the user explicitly re-parses data.
- Composability beats magic syntax: every transformation should look like ordinary command plumbing.

## Features

### Execution & Pipes
Commands run left-to-right using standard fork/exec and POSIX-style pipes. `;` sequences commands unconditionally, and `&&`/`||` short-circuit on success/failure. There is no special syntax beyond `cmd arg | next_cmd`, `;`, `&&`, and `||`, and grouping still relies on parentheses for precedence without creating subshell semantics by default.
Pipeline segments may be functions or inline brace blocks; these run in a subprocess and do not mutate parent shell state.
Non-zero exit codes only update `$?` and do not emit warnings by default.
Short-circuit operators (`&&`/`||`) only evaluate the executed branch; expansions and captures in skipped branches are not evaluated.
```bash
ls /var/log | grep error | tail -n 20
echo first ; echo second
true && echo ok
false || echo ok
def list_c { ls }
list_c | grep c
{ echo test ; ls } | grep test
```
Operators must be separated by whitespace: `ls | rg foo` is valid, `ls|rg foo` is a parse error. The same whitespace rule applies to redirection operators.

### Redirection
Redirection uses explicit operator tokens and applies to the current pipeline segment only (e.g., `ls | rg foo out> log.txt` redirects `rg` output, not `ls`). Operators are recognized only in unquoted tokens.

#### Output and Error Streams
- `>` and `>>` redirect stdout to a file (overwrite vs. append). The operator must be a separate token: `echo hi > out.log` is valid, `echo hi >out.log` is a parse error.
- `out>`, `err>`, `o>`, `e>` redirect stdout/stderr to a destination. `out+err>` and `o+e>` merge stdout and stderr.
- Stream destinations are written without whitespace and are reserved keywords:
  - `out>err`, `err>out`, `out+err>out`, `out+err>err`
  - `o>e`, `e>o`, `o+e>o`, `o+e>e`
- `null` or `n` as an attached destination sends output to `/dev/null` (e.g., `out>null`, `e>n`).
- If whitespace separates the destination, it is treated as a literal path (e.g., `out> err` writes to a file named `err`).

Multiple redirections are allowed. If the same stream is redirected more than once, the last redirection wins. Redirects to other streams always use the original stdout/stderr for the command (order does not change which underlying stream is targeted).

#### Input
`<` redirects stdin from a file. Like `>`/`>>`, it must be a separate token: `cat < input.log` is valid, `cat <input.log` is a parse error.

#### Examples
```bash
cat unknown.txt out> out.log err> err.log
cat unknown.txt out+err> log.log
cat unknown.txt out+err>> log.log
cat unknown.txt o> out.log e> err.log
cat unknown.txt o+e> log.log
echo hi out> err err> /dev/null
echo hi > out.log
cat < input.log
```

### Comments
Full-line comments begin with `#`. Inline comments are recognized when `#` appears in an unquoted token and is preceded by whitespace; everything after the `#` is ignored.
```bash
echo hello # inline comment
echo "literal # inside quotes"
```

### Atomic Variables & Assignment
`$var` always expands to exactly one argument, preserving embedded spaces and newlines; `name=value` mutates shell-local state while `export name=value` promotes it to the environment of child processes. No implicit splitting or globbing happens during expansion. **Current implementation:** leading `name=value` tokens set shell-local variables (no command runs if only assignments are present), `$var` expands in unquoted tokens (including inline), and lookup checks shell-local variables before falling back to the process environment.
```bash
path="My Projects/unshell"
cd $path          # treated as one argument
export EDITOR=vim # propagated to children
```

### Spread Operator `...`
`...` re-tokenizes a string using the shell's own parser. `...$var` or `...[cmd]` behaves like `eval` in place: quotes, escapes, and whitespace in the source are honored so the caller controls splitting. This is how legacy behaviors (space splitting, inline scripts) are opt-in. **Current implementation:** unquoted tokens starting with `...` re-tokenize their suffix after expanding captures/variables, and operator tokens from the spread (`|`, `;`, `&&`, `||`) are treated as normal separators. This is distinct from inline capture concatenation (`a[pwd]`), which stays a single argument unless you explicitly use `...`.
```bash
files="-la ./bin \"./my file.txt\""
ls ...$files
```

### Command Substitution with Square Brackets
Tokens that start with `[` and contain more than one character run as captures: `[cmd args]` executes `cmd`, captures stdout, trims a single trailing newline (configurable), and injects the result as one argument. A lone `[` continues to execute `/usr/bin/[` just like any other binary in `$PATH`. Capture recognition only triggers when `[` is immediately followed by a non-whitespace character, so `[ test -f foo ]` continues to invoke `/usr/bin/[` and `[ ]` stays literal. Unquoted tokens can concatenate captures with surrounding text (e.g., `foo-[echo bar]`), and captures may nest (e.g., `[echo [pwd]]`). `$()` substitutions are also grouped as a single token even when they include spaces (e.g., `$(echo a b)`), and their output is inserted as one argument unless spread with `...`. To avoid conflicts in strings (e.g., `echo "[module 7]"`), capture recognition only occurs for unquoted tokens; inside quotes the brackets are literal. Classic `$()` is still accepted everywhere (including inside strings), and `$[]` is treated the same as `[]` for callers that prefer explicit sigils mid-line. **Current implementation:** captures run in a subshell using the same parser/executor as scripts (aliases, builtins, control flow, and pipelines). Changes inside the capture do not mutate the parent shell.
```bash
cp [which python3] ./bin/python-system
hash="sha256:[sha256sum Cargo.lock]"
```

### Double-Quoted Strings & Capture Interpolation
Double quotes group tokens without splitting, allow escaping via `\"` (all other backslashes remain literal), and support inline captures using `$[]` or `$()` plus `$var` interpolation. Anything else (including bare `[]`) remains literal inside strings. The shell evaluates captures, trims the trailing newline, and splices the result directly into the string without introducing extra splitting; `$var` expands to its full value without trimming. **Single quotes** are fully literal: no `$var`, `$()`, or `$[]` expansion happens inside them.
```bash
echo "hello $[echo world]" "$[pwd]"
```

### Configurable Trailing-Newline Trimming
Because many programs emit terminal newlines, captures strip exactly one trailing `\n` by default. A shell option toggles the behavior at runtime for workflows that need raw output. Proposed syntax: `set subshells.trim_newline false` (and `true` to re-enable).
```bash
set subshells.trim_newline false
printf "[cat banner.bin]"
```
**Current implementation:** `set subshells.trim_newline true|false` controls whether capture output removes a single trailing newline.

### Explicit `eval` Command
`eval` takes a single argument, runs it through the parser, and executes the resulting command sequence. This makes dynamic dispatch possible even when the callee is indirect.
```bash
cmd=eval
payload='touch "generated file.txt"'
$cmd $payload
```
**Current implementation:** `eval` runs in the parent shell (not a subshell) and can mutate shell state.

### Grouping with Parentheses
Parentheses still group pipelines without invoking capture semantics, letting users control precedence or isolate redirections without new processes unless the OS requires them.
```bash
(cat foo && cat bar) | grep TODO
```

### Control Flow Blocks
Blocks are introduced by keywords (`if`, `else`, `elif`, `for`, `foreach`) followed by either a newline with indentation **using hard tabs only** (Python-style but without spaces) **or** a brace-delimited block (inline or multi-line). Authors can mix styles per block, but indentation inside braces is still recommended for clarity. **Current implementation:** tab-indented and brace-delimited `if`/`else`/`elif`/`for`/`foreach` blocks within scripts.
```bash
if test -f config.toml
	echo "config exists"
else
    echo "missing config"

if test -f config.toml {
    echo "config exists"
}
```

### Functions: `def`
`def name` defines a function using either a tab-indented block or brace-delimited block. Functions run in the parent shell (no subshell), so they can mutate variables and the working directory. `return STATUS_HERE` ends the current function and sets `$?` to the given status (defaults to the last command status). `local name=value` creates a function-scoped variable; normal assignments remain global. To prevent runaway recursion, function calls are capped at a small fixed depth.
```bash
def greeting
	echo "hello $1"

def maybe
	local name=$1
	if [ $name = ok ]
		return 0
	return 2
```

### Positional Arguments
Scripts and functions expose positional args as `$1`, `$2`, ...; `$#` is the count; `$*` expands to the list (unquoted yields multiple args, quoted joins with spaces); `$?` is the last status. There is therefore no need for `$@`.
```bash
echo "args=$#"           # count
printf "%s|" $*          # each arg
printf "%s|" "$*"        # joined into one string
```

### Argument Loops: `for … in`
`for name in arglist` iterates over a fully realized list of arguments (typically produced with `...`). Each iteration binds `name` without exporting it.
```bash
for server in ...[cat servers.list | quote]
    ssh $server uptime
```
**Current implementation:** tab-indented `for name in ...` blocks inside scripts, reusing the existing tab-indented block execution model.

### Stream Loops: `foreach`
`cmd | foreach name` treats stdin as a stream of records (newline-delimited), assigning each trimmed line to `name` and executing the block for each row. The block **does not** implicitly forward the original line; authors must `echo` (or otherwise emit) data if downstream stages should receive anything. `foreach` composes naturally inside pipelines, albeit in a child process, meaning mutations do not leak to the parent shell. Tab-indented blocks are only allowed when `foreach` is the final pipeline stage; use braces to continue piping.
```bash
ls -1 | foreach file
	cp $file ../backup/
    echo $file
| grep ".txt"
```
**Current implementation:** brace or tab-indented `cmd | foreach name` blocks execute in a child process and can appear mid-pipeline; each line of upstream stdout is bound to `name`.

### External Expansion Handlers
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
**Current implementation:** `set expansions.characters CHARS on|off` controls which characters trigger expansion; `set expansions.handler ...` sets the handler command and arguments. The handler must return a JSON array of strings to splice into the argument list; handler failures or invalid JSON abort the command.

### External String & Quoting Utilities
Utilities such as `s` (string transforms) and `quote` (turn newline-separated input into properly quoted shell tokens) ship as standalone Rust binaries. `quote`'s contract: read stdin, treat each line as a record, emit a space-separated list where each record is wrapped in quotes and internal quotes/backslashes are escaped so `...` can safely re-tokenize the output. Additional helpers (like a configurable-delimiter `split`) can exist in user space, but the shell core remains agnostic.
```bash
title=$(s $raw_title trim)
ls ...[ls | quote]
for path in ...[cat files.list | quote]
	rm $path
```

### Minimal Built-ins & Aliases
The shell ships only what it must: `cd`, `alias`, `unalias`, `set`, `export`, `local`, `return`, `exit`, `builtin`, `eval`, and the control keywords. Everything else is expected to be an external binary or script so users can curate their environment and keep the core auditable.
```bash
alias ll="ls -la"
unalias ll
cd /srv/www
```
**Current implementation:** `cd` defaults to `$HOME` and treats `-` as a literal path (no `OLDPWD` shortcut), `export` sets both shell-local and process environment variables, and alias values are expanded at definition time using normal quoting rules (use single quotes or escapes to preserve `$var`). Aliases expand at command start plus optional global aliases for any token (`alias -g`).

### `builtin` Dispatch
`builtin NAME ...` forces a built-in lookup for `NAME`, bypassing functions. Normal and global aliases still apply before the command is parsed, so a global alias can rewrite the builtin invocation itself.
```bash
def cd
	echo "shadowed $*"

alias -g cd "echo alias"

cd /tmp          # -> "alias /tmp"
builtin cd /tmp  # -> "alias /tmp" (global alias rewrites the token)
```

### Alias Semantics
Aliases are parsed like normal commands: argument parsing and expansions happen before the builtin receives values. Global aliases (`alias -g`) can match any unquoted token, while normal aliases only match the first token of a command or pipeline segment.
```bash
alias greet "echo $name"
alias -g today "[date +%F]"

greet      # expands to: echo <value-of-$name-at-definition-time>
echo today # expands to: echo [date +%F]
```
**Current implementation:** alias definitions run through the same expansion rules as other commands; single quotes keep `$var` literal, double quotes expand; alias expansion repeats while `aliases.recursive` is true; quoted tokens never trigger global alias replacement; alias expansion happens before control keywords are parsed.

### Shell Settings
Settings are toggled with `set KEY VALUE` and apply to the running shell. The initial configuration is intentionally small.
```bash
set aliases.recursive false
set aliases.recursive true
```
**Current implementation:** `aliases.recursive` controls whether alias expansion repeats until it stabilizes.

### REPL (Optional)
The interactive prompt is provided by Rustyline when built with the default `repl` feature. Vi mode is the default editing mode, history is persisted based on `USH_HISTFILE`/`HISTFILE`/`XDG_DATA_HOME`/`$HOME` (see `docs/repl.md`), and completion uses `fzf` when available with a list-completion fallback. Basic highlighting colors strings and built-ins/control keywords. REPL-only settings are configured via `set`:
```bash
set repl.mode vi
set repl.mode emacs
set repl.completion.command fzf
set repl.completion.command off
set repl.bind ctrl-e end-of-line
set repl.bind alt-f forward-word
```
**Current implementation:** `repl.mode`, `repl.completion.command`, and `repl.bind` update the Rustyline session; `repl.bind` maps keys to a small set of editing actions (move, kill-line, accept-line, history search, complete, insert text).

### Startup Sourcing
Before sourcing any startup files, the shell sets `SHELL=ush` and `USH_MODE` (`repl` or `script`).

The shell sources the first existing init file from this list:

1. `/etc/unshell/init`
2. `$XDG_CONFIG_HOME/unshell/init`
3. `$HOME/.config/unshell/init`
4. `$HOME/.unshell/init`

`make install` will install `util/unshell_init` into `/etc/unshell/init` if it does not already exist.

Flags:
```bash
ush --norc          # skip all startup files
ush --rc /path/to/init  # only source this file
```

## Ambiguities / Open Questions
- **Error handling mode:** Should non-zero exit codes inside pipelines or blocks abort the script (akin to `set -e`) or only fail the current step?
- **Else/loop semantics:** What syntax/behavior should `else`, `elif`, `for`, and `foreach` follow (indentation vs braces priorities, variable scoping, etc.)?
