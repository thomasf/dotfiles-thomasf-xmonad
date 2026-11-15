# ws

`ws` is a command-line tool for managing dynamic XMonad workspaces. It resolves structured workspace names into project directories and git worktrees, handles empty-workspace initialization, drives rofi menus for workspace and window navigation, and provides IPC to inspect and control critty terminals and per-project Emacs instances.

XMonad invokes `ws` through keybindings and workspace event hooks. In turn, `ws` communicates back to XMonad via X11 client messages (`WS_GO`, `WS_SHIFT`, `WS_FILL`, `WS_CLEAN`, `WS_RENAME`, `WS_FOCUS`) handled by `XMonad.Hooks.WorkspaceIPC`. (Linux only).

## Workspace Naming

Workspace names follow the format `<project>[@<worktree>][.<suffix>]`, where `<project>[@<worktree>]` forms the prefix. For example, in `hello@test.bot`:

- **Project** (`hello`): Determines the working directory. Defaults to the ghq repository matching the project name, or an explicit override saved via `ws dir -set`.
- **Worktree** (`test`): Selects the git worktree `<repo>@<worktree>` located alongside the main repository checkout.
- **Suffix** (`bot`): Dictates what `ws action` launches when opening an empty workspace, and whether critty terminals enable socket remote control.

Workspaces sharing the same prefix represent different views of the same project context (e.g. `hello.term`, `hello.emacs`, and `hello.www`). Commands like `ws fill hello` distribute these views across all connected monitors at once.

## Commands

Run `ws <command> -h` for full flag descriptions and options.

### Workspaces and Windows

| Command | Description |
|---|---|
| `ws pick` | Interactive workspace switcher via rofi (prints chosen name, or switches immediately with `-go`). Pressing `Ctrl+Tab` after `project@` lists repository branches and jumps to the chosen worktree, creating it if needed. |
| `ws go`, `ws shift` | Switch to a workspace or move the focused window there. Automatically creates workspaces that do not yet exist. |
| `ws fill` | Distribute all workspaces matching a given prefix across available screens. |
| `ws clean` | Remove empty, hidden workspaces. |
| `ws rename` | Rename the current workspace. |
| `ws window` | Interactive window switcher via rofi showing window class, workspace, and process hierarchy. Supports focusing, bringing the window to the current workspace, or closing it. |

### Directories and Launching

| Command | Description |
|---|---|
| `ws name` | Print the current workspace name normalized for file names and sockets. Use `-d` for the raw display name, `-p` for the prefix, and `-s` for the suffix. |
| `ws dir` | Print the root directory for a workspace. Use `-set` / `-unset` to save or clear custom prefix mappings. |
| `ws action` | Run the startup action for an empty workspace. Executes `w.<project>` from `$PATH` if present, falling back to suffix rules. |
| `ws run` | Launch an executable from `$PATH` in the workspace directory via rofi. Keybindings: `Alt+t` runs in a terminal, `Alt+s` runs with `sudo`, and `Alt+h` keeps the terminal open after exit. |
| `ws term` | Launch a terminal emulator (prefers critty) with `$WS_WORKSPACE` set to the target workspace. |
| `ws rofi` | Run rofi positioned on the current XMonad screen. |

Default suffix actions (when no `w.<project>` script exists):

| Suffix | Action |
|---|---|
| `www`, `doc` | `www-window` |
| `term`, `terminal`, `bot`, `test` | `term` (prompts to run `pathenv` if present) |
| `emacs` | `e` (emacsclient wrapper) |
| `files` | `nautilus` in the workspace directory |

### Git Worktrees (`ws wt`)

`ws wt` manages git worktrees associated with a project. A worktree named `name` lives at `<repo>@<name>` alongside the main checkout, with corresponding workspace prefixes named `project@name` (e.g. branch `feat/x` maps to worktree `feat-x`).

| Command | Description |
|---|---|
| `ws wt list` | List workspace prefixes for the main checkout and all worktrees. Pass `-l` to include paths and branches. |
| `ws wt add` | Non-interactively create a worktree, checking in order for an existing local branch, a matching remote tracking branch, or creating a new branch from `HEAD`. |
| `ws wt init` | Interactively set up a worktree for a workspace, selecting an existing branch or specifying the start point for a new branch. (Offered automatically by `ws action` when a worktree directory is missing). |
| `ws wt remove` | Remove a worktree directory and its stored path mapping. The underlying git branch is retained, and `ws` prints the command to delete it manually if desired. |

### Terminals and Emacs (`ws rc`, `ws emacs`)

`ws term` enables critty socket remote control for terminals started on `bot`, `logs`, and `test` workspaces. A terminal's identifier defaults to its workspace name, with suffix numbers (`+2`, `+3`, ...) distinguishing multiple terminals in the same workspace.

Target names resolve relative to the caller's current workspace (via `$WS_WORKSPACE`): from `dpg@x.bot`, `.logs` targets `dpg@x.logs`, and `+2` targets the second terminal on `dpg@x.bot`.

| Command | Description |
|---|---|
| `ws rc list` | List controllable terminals with their workspace, PID, working directory, and running command. |
| `ws rc read` | Dump terminal screen contents. Use `-lines <n>` or `-history` for scrollback, or `-visible` to capture all on-screen terminals. |
| `ws rc send` | Send text or keystrokes to a terminal. `-paste` uses bracketed paste mode; `-cr` appends Enter. |
| `ws read` | Read text from Emacs buffers and controllable terminals across workspaces and screens. Defaults to visible on-screen sources. |
| `ws emacs list` | List running Emacs servers with their workspace, PID, working directory, and selected buffer. |
| `ws emacs read` | Read visible window text, full buffer (`-full`), recent lines (`-lines <n>`), or active marked region (`-region`). |

This allows scripts and background agents (e.g. running in `.bot`) to inspect logs, run tests, and read Emacs buffers across the desktop:

```sh
ws rc read .test                       # Screen of this project's test terminal
ws rc read -lines 200 .logs            # Last 200 lines from the logs terminal
ws rc read -visible -except .bot       # All visible terminals except the caller
ws rc send -cr .test 'go test ./...'   # Trigger a test run in the test terminal
ws read                                # Everything visible on screen (Emacs + terminals)
ws read -except .bot                   # Everything visible except the caller's terminal
ws read -global                        # All Emacs buffers and terminals across all workspaces
ws emacs list -global                  # List all running Emacs servers across workspaces
ws emacs read -region                  # Active selection in Emacs (workspace or visible)
ws emacs read                          # Text of all visible windows across running Emacs servers
ws emacs read -full                    # Complete buffer contents
ws emacs read -lines 100               # Last 100 lines of buffer
ws emacs read -global                  # All windows across all running Emacs servers
ws emacs read -region -no-header       # Region without file/line header
ws emacs read dotfiles                 # Buffers from the 'dotfiles' Emacs server
```

## Configuration and Cache Files

- `~/.config/wsdirs/<prefix>`: Custom workspace directory mappings saved with `ws dir -set`.
- `~/.config/theme/palette.json`: Theme colors used for rofi menus.
- `$XDG_CACHE_HOME/ws/` (`~/.cache/ws/` by default): Cached `$PATH` binaries, launch history for `ws run`, and rofi font character width metrics.

## Installation and Build

Install via the dotfiles CLI from anywhere in the `xmonad` repository:

```sh
dotfiles install cmd/ws
```

Standard Go commands also work directly in this directory:

```sh
go build
go test ./...
```

**Runtime dependencies:** `rofi`, `git`, and `ghq`. Terminal remote control (`ws rc`) requires `critty`.

