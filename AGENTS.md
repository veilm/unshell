# unshell Agent Guidelines

## Development Flow
1. **Understand the spec**: Read `docs/spec.md` for the project vision and open questions before coding. Update it as behavior changes or new gaps surface.
2. **Plan**: For anything beyond a trivial tweak, outline a short plan (what to build, how to test, which docs to touch) before modifying code.
3. **Implement**: Keep the core shell minimal; prefer Rust for shell internals and standalone utilities for extra helpers. Use ASCII unless the file already contains non-ASCII.
   - Keep tokenization and execution centralized so new syntax reuses the same parsing path; avoid feature-specific parsing that can change unrelated operator behavior.
4. **Tests first/always**: Add or update integration fixtures in `tests/fixtures/` that demonstrate the behavior before (or alongside) implementing it. Each `.ush` script must have `.stdout` / `.stderr` snapshot files. Run `cargo test` to validate.
   - If stderr should be empty, omit the `.stderr` file; the test harness treats missing stderr snapshots as empty.
5. **Document**: After coding, edit `docs/spec.md` and/or `README.md` to reflect what’s now implemented vs. what’s next. Every new ambiguity should be captured under in the open-questions list.
6. **Sync highlighting after builtin changes**: When builtins change, update syntax highlighting in `util/unshell.kak` and in the repl keyword list.
7. **Communicate failures**: If tests or commands fail, report the failure (do not hide it).
8. **Final review**: Summarize code changes, mention updated tests/docs, and remind of outstanding failures or TODOs in the final response.

## Testing Commands
- `cargo test` : builds `ush` and runs the integration suite (fixtures).
- `make build` : release build of `ush`.
- `make install` : install the prebuilt `ush` (requires prior `make build`).

## Spec Maintenance Checklist
- Add new features/requirements with examples.
- Append unresolved questions to the “Ambiguities / Open Questions” list if they arise during development.

## Related repos available for reference, if relevant
- ~/src/tmux/
- ~/src/rustyline/
- ~/src/yash/
