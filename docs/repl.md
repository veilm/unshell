# unshell REPL

The REPL is powered by Rustyline (default build). It starts in vi mode and provides history, file completion, and lightweight highlighting.

## Build Modes

- default build includes the REPL
- `make REPL=off` or `./install.sh --no-repl` builds a minimal stdin loop

## History

- default path: `~/.ush_history`
- override: `USH_HISTORY=/path/to/history`

## Completion

- default: `fzf` if present, list completion otherwise
- disable completion command: `set repl.completion.command off`
- custom completion command: `set repl.completion.command fzf` or `set repl.completion.command skim`

The REPL passes file candidates to the completion command on stdin. It expects the same output shape as `fzf --print-query` (query line + selected line).

## Highlighting

- strings (`'...'` / `"..."`)
- built-ins and control keywords (`if`, `for`, `foreach`, `set`, etc.)

## Settings (REPL-only)

```bash
set repl.mode vi
set repl.mode emacs

set repl.completion.command fzf
set repl.completion.command off

set repl.bind ctrl-e end-of-line
set repl.bind alt-f forward-word
set repl.bind tab complete
set repl.bind left backward-word
set repl.bind right forward-word
set repl.bind ctrl-k kill-line
set repl.bind ctrl-a beginning-of-line
set repl.bind ctrl-e end-of-line
set repl.bind esc accept-line
```

## Key Names

- letters with modifiers: `ctrl-a`, `alt-f`
- arrows: `left`, `right`, `up`, `down`
- `tab`, `enter`, `esc`, `backspace`

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
- `insert:TEXT` or `insert TEXT`

To remove a binding:

```bash
set repl.bind ctrl-e off
```
