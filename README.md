# alloc-scan.el

An Emacs plugin that analyzes OCaml compiler output to highlight memory allocation points in source code. This is a port of the alloc_scan Neovim plugin to Emacs.

## Features

- **Allocation Highlighting**: Highlights memory allocation points in OCaml source code
- **Virtual Text**: Shows allocation details (block count, tag) as overlay text
- **Smart File Selection**: Use minibuffer completion to select .cmx.dump files
- **Customizable**: Configurable highlighting faces and options

## Installation

### Manual Installation

1. Clone or download this repository
2. Add the directory to your Emacs `load-path`
3. Require the package in your init file:

```elisp
(add-to-list 'load-path "/path/to/alloc-scan.el")
(require 'alloc-scan)
```

### Package Manager Installation

If using `use-package`:

```elisp
(use-package alloc-scan
  :load-path "/path/to/alloc-scan.el"
  :commands (alloc-scan alloc-scan-file alloc-scan-clear)
  :bind (("C-c a s" . alloc-scan)
         ("C-c a f" . alloc-scan-file)
         ("C-c a c" . alloc-scan-clear)))
```

## Usage

### Commands

- `M-x alloc-scan` - Scan current buffer for allocations
  - Opens Dired to select a .cmx.dump file
  - Highlights allocation points in the current buffer
  
- `M-x alloc-scan-file` - Scan a specific file for allocations
  - Prompts for a source file to scan
  - Opens Dired to select a .cmx.dump file
  - Opens the source file and highlights allocations
  
- `M-x alloc-scan-clear` - Clear all allocation highlights in the current buffer

### Workflow

1. Compile your OCaml project with allocation tracking enabled
2. Use `M-x alloc-scan` in an OCaml source buffer
3. Select the appropriate `.cmx.dump` file from the minibuffer prompt
4. Allocation points will be highlighted with virtual text showing details

## Suggested Keybindings

Add these to your Emacs configuration:

```elisp
;; Global keybindings
(global-set-key (kbd "C-c a s") 'alloc-scan)
(global-set-key (kbd "C-c a f") 'alloc-scan-file)
(global-set-key (kbd "C-c a c") 'alloc-scan-clear)

;; OCaml mode specific keybindings
(with-eval-after-load 'ocaml-mode
  (define-key ocaml-mode-map (kbd "C-c a s") 'alloc-scan)
  (define-key ocaml-mode-map (kbd "C-c a f") 'alloc-scan-file)
  (define-key ocaml-mode-map (kbd "C-c a c") 'alloc-scan-clear))
```

## Customization

### Variables

- `alloc-scan-highlight-face` - Face used to highlight allocation points (default: `highlight`)
- `alloc-scan-show-virtual-text` - Whether to show allocation details as virtual text (default: `t`)
- `alloc-scan-virtual-text-face` - Face used for virtual text (default: `font-lock-comment-face`)

### Example Configuration

```elisp
(use-package alloc-scan
  :load-path "/path/to/alloc-scan.el"
  :custom
  (alloc-scan-highlight-face 'warning)
  (alloc-scan-virtual-text-face 'font-lock-doc-face)
  (alloc-scan-show-virtual-text t)
  :bind
  (("C-c a s" . alloc-scan)
   ("C-c a f" . alloc-scan-file)
   ("C-c a c" . alloc-scan-clear)))
```

## How It Works

1. **File Discovery**: The plugin automatically searches for .cmx.dump files in your project's `_build` directory
2. **Dired Integration**: Opens Dired with the most likely dump file pre-selected for user confirmation
3. **Parsing**: Parses allocation information from the dump file using regex patterns
4. **Highlighting**: Creates overlays in the source buffer to highlight allocation points
5. **Virtual Text**: Shows allocation details (block count and tag) as overlay text

## Requirements

- Emacs 25.1 or later
- OCaml project with compiler dump files (`.cmx.dump`)

## Generating Compiler Dumps

To generate the necessary .cmx.dump files for your OCaml project:

```bash
# Build with allocation tracking
dune build --profile dev

# Or use ocamlopt directly with dump flags
ocamlopt -dump-cmm your_file.ml
```

The plugin will look for `.cmx.dump` files in your project's `_build` directory structure.

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.