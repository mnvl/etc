# AGENTS.md

## Project Structure

- `~/etc` is the repo. Dotfiles like `~/.emacs` are symlinks into it (e.g. `~/.emacs` -> `~/etc/.emacs`).
- Always use paths relative to the repo (e.g. `.emacs`, `vscode/keybindings.json`) for git operations.

## Session Handoff (2026-02-16)

This repo now has aligned VSCode/Emacs keybinding workflows focused on `C-x`/`C-c`/`C-t` muscle memory and paragraph navigation.

## Commits Created

1. `a56a43d`  
   `Add missing VSCode Emacs-style keybindings`

2. `400811c`  
   `Add Emacs C-t test/debug workflow and smarter test command detection`

3. `487bad2`  
   `Align Emacs and VSCode keybindings and navigation workflow`

## What Was Implemented

### VSCode (`vscode/keybindings.json`)

- Added missing Emacs-like prefix bindings under:
  - `alt+x ...` for file/recent/open/editor-group actions
  - `alt+c ...` for code actions, format, impl, declaration, rename, references, indent/outdent
  - `alt+t ...` test/debug workflow already present and kept
- Added paragraph-like navigation:
  - `alt+n`: jump to next blank line (`^\\s*$`)
  - `alt+p`: jump to previous blank line (`^\\s*$`)
- Removed mapping the user did not want:
  - `alt+c shift+s`

### Emacs (`.emacs`)

- Added `C-t` and `M-t` prefix map for test/debug workflow:
  - `c/f/l/r/s/d/b`
- Added fallback test/debug helpers (`compile`, `gdb`, optional DAP/GUD integration).
- Added smarter test command detection by language and project root:
  - Python: `pytest` (file-aware)
  - Rust: `cargo test`
  - Go: `go test ./...`
  - JS/TS: lockfile-aware package manager selection (`pnpm`, `yarn`, then `npm`)
  - C/C++ with CMake: `ctest --output-on-failure`
- Added requested mappings:
  - `M-b` -> `neotree-toggle` (sidebar toggle)
  - `M-h` -> `query-replace`
  - `C-c b` -> `my-debug-toggle-breakpoint`
  - `C-g` -> `goto-line`
  - `M-g` -> `counsel-imenu`

## Important Caveat

- `C-g` now runs `goto-line`, so default Emacs `keyboard-quit` on `C-g` is overridden.
- `Esc` is explicitly bound to `keyboard-quit` to preserve a quick cancel key.

## Remaining Notable Differences (if revisited)

- Some VSCode platform editing bindings are intentionally not mirrored in Emacs (e.g. `Ctrl/Cmd+f`, `Ctrl/Cmd+s`, selection/scroll variants).
- Some Emacs-specific bindings are intentionally not mirrored in VSCode (e.g. `F8`, `C-S-x`, `C-S-c`, `C-S-v`, `C-r`).

## Current Git State Note

There are unrelated pre-existing working tree changes outside this task (e.g. `fish/fish_variables` and multiple untracked files). Do not include them unless explicitly requested.
