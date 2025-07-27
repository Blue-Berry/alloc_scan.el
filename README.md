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

- `alloc-scan-highlight-style` - Style of highlighting (default: `'box`)
- `alloc-scan-highlight-color` - Color for highlighting (default: `"#cccccc"`)
- `alloc-scan-show-virtual-text` - Whether to show allocation details as virtual text (default: `t`)
- `alloc-scan-virtual-text-face` - Face used for virtual text (default: `font-lock-comment-face`)

### Highlight Styles

The plugin supports several highlighting styles:

- **`'box`** - Border outline around allocation (default)
- **`'underline`** - Underline the allocation text
- **`'background`** - Light background color
- **`'bold`** - Bold text with colored foreground
- **`'custom`** - Use custom face definition

### Example Configurations

#### Box Style (Default - Subtle Outline)
```elisp
;; Subtle grey outline (recommended)
(setq alloc-scan-highlight-style 'box
      alloc-scan-highlight-color "#999999")

;; Very light grey outline
(setq alloc-scan-highlight-style 'box
      alloc-scan-highlight-color "#cccccc")

;; Red outline for high visibility
(setq alloc-scan-highlight-style 'box
      alloc-scan-highlight-color "#ff6b6b")
```

#### Underline Style
```elisp
;; Subtle grey underline
(setq alloc-scan-highlight-style 'underline
      alloc-scan-highlight-color "#aaaaaa")

;; Blue underline
(setq alloc-scan-highlight-style 'underline
      alloc-scan-highlight-color "#4dabf7")
```

#### Background Style
```elisp
;; Light yellow background
(setq alloc-scan-highlight-style 'background
      alloc-scan-highlight-color "#fffacd")

;; Very light grey background
(setq alloc-scan-highlight-style 'background
      alloc-scan-highlight-color "#f8f9fa")

;; Light pink background
(setq alloc-scan-highlight-style 'background
      alloc-scan-highlight-color "#ffe0e6")
```

#### Bold Style
```elisp
;; Bold dark grey text
(setq alloc-scan-highlight-style 'bold
      alloc-scan-highlight-color "#666666")

;; Bold blue text
(setq alloc-scan-highlight-style 'bold
      alloc-scan-highlight-color "#1971c2")
```

#### Custom Style
```elisp
;; Use the built-in custom face (can be customized via M-x customize-face)
(setq alloc-scan-highlight-style 'custom)

;; Or define your own custom face
(defface my-alloc-highlight
  '((t (:box (:line-width 2 :color "#ff9500" :style nil)
        :background "#fff3cd")))
  "Custom face for allocation highlighting.")

(setq alloc-scan-highlight-style 'custom)
(setq alloc-scan-highlight-face 'my-alloc-highlight)
```

### Using Emacs Customize Interface

For an interactive way to customize the plugin:

```elisp
M-x customize-group RET alloc-scan RET
```

### Complete Package Configuration

```elisp
(use-package alloc-scan
  :load-path "/path/to/alloc-scan.el"
  :custom
  (alloc-scan-highlight-style 'box)
  (alloc-scan-highlight-color "#999999")
  (alloc-scan-show-virtual-text t)
  (alloc-scan-virtual-text-face 'font-lock-doc-face)
  :bind
  (("C-c a s" . alloc-scan)
   ("C-c a f" . alloc-scan-file)
   ("C-c a c" . alloc-scan-clear)))
```

### Applying Changes

After changing settings, clear existing highlights and rescan:

```elisp
M-x alloc-scan-clear RET
M-x alloc-scan RET
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