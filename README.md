# gim-code-hud

An Emacs minor mode that keeps a `*gim-code-hud*` org buffer updated with live analysis of the file you are currently editing. As you switch files the HUD refreshes automatically, pulling from a per-project SQLite cache so repeated visits are instant.

## What it shows

| Section | What it contains |
|---|---|
| **Git Status** | `clean`, `dirty`, `staged`, or `untracked` for the current file |
| **Contributors** | People who have touched the file, with commit counts, sorted descending |
| **Co-changes** | Files that historically commit alongside this file, shown as a percentage of shared commits |
| **Purpose** | LLM-generated ≤3-sentence description of what the file does (cached 1 h) |
| **History** | LLM-generated ≤5-sentence narrative of how the file evolved (cached 24 h) |

## Installation

Depends on Emacs 30+, [`dash`](https://github.com/magnars/dash.el), [`s`](https://github.com/magnars/s.el), and [`async`](https://github.com/jwiegley/emacs-async). The LLM sections require the [Claude CLI](https://github.com/anthropics/claude-code) (`claude`) to be on your `PATH`.

```elisp
(use-package gim-code-hud
  :load-path "path/to/gim-code-hud"
  :commands (gim-code-hud-mode gim-code-hud/show))
```

Enable globally:

```elisp
(gim-code-hud-mode 1)
```

## Usage

| Command | Description |
|---|---|
| `M-x gim-code-hud/show` | Open or switch to the HUD buffer |
| `g` (in HUD buffer) | Force a full refresh — re-renders the template and re-fetches all sections |
| `C-u g` | Prompt for a specific section ID to refresh (invalidates only that section's cache) |
| `q` (in HUD buffer) | Bury the HUD buffer |

## Configuration

```elisp
;; Directory for the SQLite cache (default: project root detected via projectile / vc-root-dir).
;; Set this to keep the DB outside version-controlled trees.
(setq gim-code-hud-db-directory "~/.cache/gim-code-hud")

;; Claude CLI path (default: "claude")
(setq gim-code-hud-cli "/usr/local/bin/claude")

;; Claude model (default: nil = CLI default)
(setq gim-code-hud-model "claude-opus-4-5")

;; Customize the LLM prompts (both are format strings; %s = file contents / git log)
(setq gim-code-hud-purpose-prompt "In one sentence, describe what this file does.\n\n%s")
```

## Ad-hoc sections

You can define your own HUD sections directly in the org template by adding headings with a `GIM_CODE_HUD_CLI_COMMAND` property alongside the usual `GIM_CODE_HUD_ANALYSIS_ID`. The command is a shell string; `{active_file_path}` expands to the absolute path of the current file (same `{variable}` style as `{file}` in the org template).

Add sections via `gim-code-hud-org-template-suffix` (set before the package loads so it is baked into the default template value) or by customizing `gim-code-hud-org-template` directly:

```elisp
(setq gim-code-hud-org-template-suffix "
** Teachable Moment
:PROPERTIES:
:GIM_CODE_HUD_ANALYSIS_ID: teachable-moment
:GIM_CODE_HUD_CLI_COMMAND: claude -p \"What lessons does {active_file_path} hold for someone learning Rust? Two paragraphs max.\"
:GIM_CODE_HUD_TTL_SECONDS: 7200
:END:

(loading…)
")
```

Ad-hoc sections are cached in the same SQLite store as built-in sections (default TTL: 1 h). Add `GIM_CODE_HUD_TTL_SECONDS` to override the TTL for any section — built-in or ad-hoc. They participate in `gim-code-hud/refresh` — a full refresh re-fetches them; `C-u g` can target a single ad-hoc section by its ID.

## How it works

Two timers run while the HUD buffer is visible:

- **Staleness timer** (every 5 s) — checks each section's cached timestamp against its TTL; fires an async subprocess for any that have expired.
- **Flush timer** (every 0.5 s) — drains completed results into the org buffer.

A short idle-timer (0.3 s) debounces rapid buffer switches so the HUD only re-renders after you settle on a file.
