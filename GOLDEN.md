# Unshell

Unshell is the interactive shell and script runner used on Delirium. Its user
configuration is in `/home/light/sync/config/unshell/`.

## Commit behavior: auto

Use `msk_git ca` after each logical, verified change. If the worktree already
has unstaged changes, stage only the changes made for the current task and use
`msk_git cp`. If it already has staged changes, do not make a commit.
