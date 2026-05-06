# gim-code-hud

An Emacs extension providing a "heads up display" buffer with live analysis of the currently active file.

Also look at @AGENTS.md

## Vision

A dedicated HUD buffer (`*gim-code-hud*`) that auto-updates as you switch files, showing:

1. **Git status** — current dirty/staged/untracked status of the file
2. **Co-change partners** — files that historically commit together with this file (à la Adam Tornhill's Code Maat / Code as a Crime Scene)
3. **Contributors** — people who have touched the file, with touch counts, sorted descending
4. **Purpose summary** — LLM-generated ≤3-sentence description of what the file does (cached hourly per file)
5. **History summary** — LLM-generated ≤5-sentence narrative of how the file evolved (cached daily per file)

## Code Style

- **Lexical bindings everywhere**: every file starts with `;;; -*- lexical-binding: t -*-`
- **Namespace**: all public symbols prefixed `gim-code-hud/`, internal helpers `gim-code-hud--`
- **Functional style**: prefer pure functions and data pipelines over imperative mutation
- **`cl-loop`** for iteration — preferred over `dolist`/`mapcar` when it reads more clearly
- **`dash.el`** and its threading operators (`->`, `->>`, `-->`) for list pipelines
- No global state unless necessary; use buffer-local variables for buffer-specific state
- Keep functions short and single-purpose
- **Interactive wrappers**: any operation worth calling ad-hoc should have an `;;;###autoload` `(interactive)` function

## Dependencies

- `dash.el` — list utilities and threading macros
- `s.el` — string utilities (if needed)
- `async.el` — for running git subprocesses off the main thread (use `async-start` / `async-start-process`)
- `use-package` — used for dependency declaration and configuration in the package header; prefer `use-package` forms over bare `require` where it adds clarity
- Emacs built-ins: `cl-lib`, `subr-x`, `project`, `vc`

## Reloadability

This package is developed interactively inside the same Emacs session that runs Claude Code.  Every file must be safely re-`load`-able mid-session:

- Define vars with `defvar` / `defcustom` (not `setq` at top level) so reloading does not reset live state.
- Define hooks, timers, and mode maps with `define-minor-mode` / `define-key` — idempotent by design.
- Avoid side-effectful top-level expressions (e.g. `(add-hook …)` outside a mode definition); put them inside the minor mode body or an `after-load` form.
- When the minor mode is turned off it must cleanly remove every hook and timer it added, leaving no residue.

## Architecture

```
gim-code-hud.el          — entry point, minor mode, buffer management, auto-update hook
gim-code-hud-git.el      — git status, contributor list, co-change analysis
gim-code-hud-llm.el      — Claude API integration, caching layer
gim-code-hud-render.el   — HUD buffer rendering
```

## Prototyping

Use `emacsclient` to try things out interactively before committing to an implementation.  After editing a file, reload it with `(load "/path/to/file.el")` via emacsclient to test in the live session.

**Before any destructive emacs or filesystem operation, ask the user for explicit confirmation.**

## Running Tests

Tests run inside the live Emacs session (which has all packages available) via emacsclient. A quick script for this:

```bash
./tools/run_tests.sh
```

This reloads all source files and opens the `*ert*` buffer showing pass/fail results.  To re-run without reloading: `M-x ert RET t RET` inside Emacs.

**Never byte-compile these files** (no `M-x byte-compile-file`, no `.elc` in the repo).  Stale `.elc` files shadow the source and cause all async tests to silently timeout.  If `.elc` files appear, delete them: `rm *.elc`.

## LLM Caching

- Purpose summary: keyed by `(file-path, mtime-hour)` — regenerates at most once per hour
- History summary: keyed by `(file-path, mtime-date)` — regenerates at most once per day
- Cache stored in memory (alist or hash-table); optionally persist to `~/.cache/gim-code-hud/`

## Co-change Analysis

Uses `git log --follow --name-only` to find commits that touched the current file, then counts co-occurring files across those commits. Excludes the file itself. Results sorted by co-occurrence count descending.
