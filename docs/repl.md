# unshell REPL

The REPL is powered by Rustyline (default build). It starts in vi mode and provides history, file completion, and lightweight highlighting.

When output is captured (non-tty stdout), the REPL inserts an inverted `$` marker if the previous command did not end with a newline. When stdout is a tty, the REPL preserves tty semantics for interactive apps and does not capture output.

## Build Modes

- default build includes the REPL
- `make REPL=off` or `./install.sh --no-repl` builds a minimal stdin loop

## History

Resolution order:

1. `USH_HISTFILE`
2. `HISTFILE`
3. `XDG_DATA_HOME/unshell/histfile`
4. `$HOME/.local/share/histfile`

The REPL appends new entries to the history file so multiple sessions can share the same file without clobbering each other's updates.

## Startup Sourcing

Startup sourcing applies to both REPL and script runs. See `docs/spec.md` for the order and flags.

The default installer will drop a starter init at `/etc/unshell/init` if missing. It only prints a message when `USH_MODE=repl`.

## Completion

- default: `fzf` if present, Rustyline filename completion otherwise
- disable completion command: `set repl.completion.command off`
- custom completion command: `set repl.completion.command fzf` or `set repl.completion.command skim`
- prompt command: `set repl.prompt.command 'echo "unshell> "'` (runs once per line)

The REPL passes file candidates to the completion command on stdin. It expects the same output shape as `fzf --print-query` (query line + selected line). When the command is missing or disabled, Rustyline's filename completion uses prefix matching and auto-completes when there is a single candidate.
Shift-Tab (`btab`) triggers completion with the fzf cursor starting on the last match.
Hidden entries (names starting with `.`) are excluded unless the completion fragment includes a `.` segment (for example `./`, `../`, or `.config`).
When completing inside quotes, the quoted fragment is used as the completion input. If the quote is still open, file completions close the quote and add a trailing space; directory completions keep the quote open.

## Highlighting

- strings (`'...'` / `"..."`)
- built-ins and control keywords (`if`, `for`, `foreach`, `set`, etc.)
- comments starting with `#` when preceded by whitespace

## Settings (REPL-only)

```bash
set repl.mode vi
set repl.mode emacs

set repl.completion.command fzf
set repl.completion.command off

set repl.history.file default
set repl.history.file /tmp/ush-history

set repl.prompt.command 'echo "unshell> "'
set repl.prompt.command off

set repl.bracketed_paste on
set repl.bracketed_paste off

set repl.bind ctrl-e end-of-line
set repl.bind alt-f forward-word
set repl.bind tab complete
set repl.bind btab complete
set repl.bind left backward-word
set repl.bind right forward-word
set repl.bind ctrl-k kill-line
set repl.bind ctrl-a beginning-of-line
set repl.bind ctrl-e end-of-line
set repl.bind esc accept-line
```

In vi mode, `#` defaults to `comment-accept` unless overridden by a custom binding.

`set repl.history.file default` recomputes the history path using the resolution order above, even if it was already using the default path.

## Hooks

If a function named `unshell_after_command_input` is defined, the REPL calls it after history is updated and before the command executes. The raw input line is passed as `$1`.

## Key Names

- letters with modifiers: `ctrl-a`, `alt-f`
- arrows: `left`, `right`, `up`, `down`
- `tab`, `btab`, `enter`, `esc`, `backspace`

## Binding Actions

- `backward-word`
- `forward-word`
- `beginning-of-line`
- `end-of-line`
- `kill-line`
- `accept-line`
- `history-search-backward`
- `history-search-forward`
- `complete`
- `comment-accept` (submit line with `# ` prepended when in vi normal mode)
- `insert:TEXT` or `insert TEXT`

To remove a binding:

```bash
set repl.bind ctrl-e off
```

Bracketed paste defaults to off in vi mode and on in emacs mode; use `set repl.bracketed_paste on|off` to override.
