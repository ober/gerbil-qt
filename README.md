# gerbil-qt

Qt6 Widgets bindings for Gerbil Scheme.

Build desktop GUI applications with native Qt6 widgets from Gerbil Scheme, using a thin C++ shim and Gambit's FFI.

## Requirements

- [Gerbil Scheme](https://cons.io) v0.18+
- Qt6 development libraries
- g++ with C++17 support

### Linux (Ubuntu/Debian)

```sh
sudo apt install qt6-base-dev libgl1-mesa-dev
```

### Linux (Fedora)

```sh
sudo dnf install qt6-qtbase-devel
```

### macOS (Homebrew)

```sh
brew install qt@6
export PKG_CONFIG_PATH="$(brew --prefix qt@6)/lib/pkgconfig:$PKG_CONFIG_PATH"
```

## Build

```sh
gerbil build
```

Or using Make:

```sh
make build
```

## Test

```sh
make test
```

Tests run headless using Qt's offscreen platform plugin (`QT_QPA_PLATFORM=offscreen`).

## Usage

```scheme
(import :gerbil-qt/qt)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (central (qt-widget-create))
           (layout (qt-vbox-layout-create central))
           (label (qt-label-create "Count: 0"))
           (button (qt-push-button-create "Click me!"))
           (count 0))
      (qt-layout-add-widget! layout label)
      (qt-layout-add-widget! layout button)
      (qt-on-clicked! button
        (lambda ()
          (set! count (+ count 1))
          (qt-label-set-text! label
            (string-append "Count: " (number->string count)))))
      (qt-main-window-set-central-widget! win central)
      (qt-main-window-set-title! win "Counter")
      (qt-widget-resize! win 300 200)
      (qt-widget-show! win)
      (qt-app-exec! app))))
```

Run examples with:

```sh
make demo-hello
make demo-counter
make demo-form
make demo-editor
make demo-dashboard
make demo-filebrowser
make demo-styled
make demo-settings
make demo-painter
make demo-datainput
make demo-planner
```

## API Reference

### Application Lifecycle

| Function | Description |
|----------|-------------|
| `(qt-app-create)` | Create QApplication |
| `(qt-app-exec! app)` | Run the event loop |
| `(qt-app-quit! app)` | Quit the event loop |
| `(qt-app-process-events! app)` | Process pending events (cooperative polling) |
| `(qt-app-destroy! app)` | Destroy application |
| `(with-qt-app app body ...)` | Macro: create app, run body, destroy on exit |

### Widget Base

All widget types support these operations:

| Function | Description |
|----------|-------------|
| `(qt-widget-create parent: #f)` | Create a generic widget |
| `(qt-widget-show! w)` | Show widget |
| `(qt-widget-hide! w)` | Hide widget |
| `(qt-widget-close! w)` | Close widget |
| `(qt-widget-set-enabled! w bool)` | Enable/disable |
| `(qt-widget-enabled? w)` | Check if enabled |
| `(qt-widget-resize! w width height)` | Resize |
| `(qt-widget-set-fixed-size! w width height)` | Set fixed size |
| `(qt-widget-set-minimum-size! w width height)` | Set minimum size |
| `(qt-widget-set-maximum-size! w width height)` | Set maximum size |
| `(qt-widget-set-style-sheet! w css)` | Apply CSS stylesheet |
| `(qt-widget-set-tooltip! w text)` | Set tooltip text |
| `(qt-widget-set-font-size! w size)` | Set font point size |
| `(qt-widget-destroy! w)` | Destroy widget |

### Main Window

| Function | Description |
|----------|-------------|
| `(qt-main-window-create parent: #f)` | Create main window |
| `(qt-main-window-set-title! w title)` | Set window title |
| `(qt-main-window-set-central-widget! w child)` | Set central widget |

### Layouts

| Function | Description |
|----------|-------------|
| `(qt-vbox-layout-create parent)` | Vertical box layout |
| `(qt-hbox-layout-create parent)` | Horizontal box layout |
| `(qt-layout-add-widget! layout widget)` | Add widget to layout |
| `(qt-layout-add-stretch! layout stretch: 1)` | Add stretch spacer |
| `(qt-layout-set-spacing! layout spacing)` | Set item spacing |
| `(qt-layout-set-margins! layout l t r b)` | Set content margins |

### Labels

| Function | Description |
|----------|-------------|
| `(qt-label-create text parent: #f)` | Create label |
| `(qt-label-text l)` | Get text |
| `(qt-label-set-text! l text)` | Set text |
| `(qt-label-set-alignment! l alignment)` | Set alignment (use `QT_ALIGN_*` constants) |

### Push Button

| Function | Description |
|----------|-------------|
| `(qt-push-button-create text parent: #f)` | Create button |
| `(qt-push-button-text b)` | Get button text |
| `(qt-push-button-set-text! b text)` | Set button text |
| `(qt-on-clicked! button handler)` | Connect clicked signal: `(lambda () ...)` |

### Line Edit

| Function | Description |
|----------|-------------|
| `(qt-line-edit-create parent: #f)` | Create single-line text input |
| `(qt-line-edit-text e)` | Get text |
| `(qt-line-edit-set-text! e text)` | Set text |
| `(qt-line-edit-set-placeholder! e text)` | Set placeholder text |
| `(qt-line-edit-set-read-only! e bool)` | Set read-only |
| `(qt-line-edit-set-echo-mode! e mode)` | Set echo mode (use `QT_ECHO_*` constants) |
| `(qt-on-text-changed! e handler)` | Connect textChanged: `(lambda (text) ...)` |
| `(qt-on-return-pressed! e handler)` | Connect returnPressed: `(lambda () ...)` |

### Check Box

| Function | Description |
|----------|-------------|
| `(qt-check-box-create text parent: #f)` | Create checkbox |
| `(qt-check-box-checked? c)` | Is checked? |
| `(qt-check-box-set-checked! c bool)` | Set checked state |
| `(qt-check-box-set-text! c text)` | Set label text |
| `(qt-on-toggled! c handler)` | Connect toggled: `(lambda (checked?) ...)` |

### Combo Box

| Function | Description |
|----------|-------------|
| `(qt-combo-box-create parent: #f)` | Create dropdown |
| `(qt-combo-box-add-item! c text)` | Add item |
| `(qt-combo-box-set-current-index! c index)` | Set selection |
| `(qt-combo-box-current-index c)` | Get selected index |
| `(qt-combo-box-current-text c)` | Get selected text |
| `(qt-combo-box-count c)` | Get item count |
| `(qt-combo-box-clear! c)` | Remove all items |
| `(qt-on-index-changed! c handler)` | Connect indexChanged: `(lambda (index) ...)` |

### Text Edit

| Function | Description |
|----------|-------------|
| `(qt-text-edit-create parent: #f)` | Create multi-line text editor |
| `(qt-text-edit-text e)` | Get plain text |
| `(qt-text-edit-set-text! e text)` | Set text |
| `(qt-text-edit-set-placeholder! e text)` | Set placeholder |
| `(qt-text-edit-set-read-only! e bool)` | Set read-only |
| `(qt-text-edit-append! e text)` | Append text |
| `(qt-text-edit-clear! e)` | Clear all text |
| `(qt-on-text-edit-changed! e handler)` | Connect textChanged: `(lambda () ...)` |

### Spin Box

| Function | Description |
|----------|-------------|
| `(qt-spin-box-create parent: #f)` | Create integer spinner |
| `(qt-spin-box-value s)` | Get value |
| `(qt-spin-box-set-value! s value)` | Set value |
| `(qt-spin-box-set-range! s min max)` | Set min/max range |
| `(qt-spin-box-set-single-step! s step)` | Set step increment |
| `(qt-spin-box-set-prefix! s prefix)` | Set display prefix |
| `(qt-spin-box-set-suffix! s suffix)` | Set display suffix |
| `(qt-on-value-changed! s handler)` | Connect valueChanged: `(lambda (value) ...)` |

### Dialog

| Function | Description |
|----------|-------------|
| `(qt-dialog-create parent: #f)` | Create dialog |
| `(qt-dialog-exec! d)` | Run modal dialog (returns result code) |
| `(qt-dialog-accept! d)` | Accept and close |
| `(qt-dialog-reject! d)` | Reject and close |
| `(qt-dialog-set-title! d title)` | Set dialog title |

### Message Box

| Function | Description |
|----------|-------------|
| `(qt-message-box-information parent title text)` | Info dialog |
| `(qt-message-box-warning parent title text)` | Warning dialog |
| `(qt-message-box-question parent title text)` | Question dialog |
| `(qt-message-box-critical parent title text)` | Critical error dialog |

### File Dialog

| Function | Description |
|----------|-------------|
| `(qt-file-dialog-open-file parent caption: dir: filter:)` | Open file picker |
| `(qt-file-dialog-save-file parent caption: dir: filter:)` | Save file picker |
| `(qt-file-dialog-open-directory parent caption: dir:)` | Directory picker |

### Menu Bar and Menus

| Function | Description |
|----------|-------------|
| `(qt-main-window-menu-bar win)` | Get/create menu bar |
| `(qt-menu-bar-add-menu bar title)` | Add top-level menu |
| `(qt-menu-add-menu menu title)` | Add submenu |
| `(qt-menu-add-action! menu action)` | Add action to menu |
| `(qt-menu-add-separator! menu)` | Add separator line |

### Actions

| Function | Description |
|----------|-------------|
| `(qt-action-create text parent: #f)` | Create action |
| `(qt-action-text a)` / `(qt-action-set-text! a text)` | Get/set text |
| `(qt-action-set-shortcut! a shortcut)` | Set keyboard shortcut (e.g. `"Ctrl+S"`) |
| `(qt-action-enabled? a)` / `(qt-action-set-enabled! a bool)` | Get/set enabled |
| `(qt-action-checkable? a)` / `(qt-action-set-checkable! a bool)` | Get/set checkable |
| `(qt-action-checked? a)` / `(qt-action-set-checked! a bool)` | Get/set checked |
| `(qt-action-set-tooltip! a text)` | Set tooltip |
| `(qt-action-set-status-tip! a text)` | Set status bar tip |
| `(qt-on-triggered! a handler)` | Connect triggered: `(lambda () ...)` |
| `(qt-on-action-toggled! a handler)` | Connect toggled: `(lambda (checked?) ...)` |

### Toolbar

| Function | Description |
|----------|-------------|
| `(qt-toolbar-create title parent: #f)` | Create toolbar |
| `(qt-main-window-add-toolbar! win toolbar)` | Add toolbar to window |
| `(qt-toolbar-add-action! tb action)` | Add action button |
| `(qt-toolbar-add-separator! tb)` | Add separator |
| `(qt-toolbar-add-widget! tb widget)` | Add custom widget |
| `(qt-toolbar-set-movable! tb bool)` | Set movable |
| `(qt-toolbar-set-icon-size! tb w h)` | Set icon size |

### Status Bar

| Function | Description |
|----------|-------------|
| `(qt-main-window-set-status-bar-text! win text)` | Show status message |

### List Widget

| Function | Description |
|----------|-------------|
| `(qt-list-widget-create parent: #f)` | Create list widget |
| `(qt-list-widget-add-item! l text)` | Append item |
| `(qt-list-widget-insert-item! l row text)` | Insert at position |
| `(qt-list-widget-remove-item! l row)` | Remove by row |
| `(qt-list-widget-item-text l row)` | Get item text |
| `(qt-list-widget-count l)` | Get item count |
| `(qt-list-widget-current-row l)` | Get selected row |
| `(qt-list-widget-set-current-row! l row)` | Set selected row |
| `(qt-list-widget-clear! l)` | Remove all items |
| `(qt-on-current-row-changed! l handler)` | `(lambda (row) ...)` |
| `(qt-on-item-double-clicked! l handler)` | `(lambda (row) ...)` |

### Table Widget

| Function | Description |
|----------|-------------|
| `(qt-table-widget-create rows cols parent: #f)` | Create table |
| `(qt-table-widget-set-item! t row col text)` | Set cell text |
| `(qt-table-widget-item-text t row col)` | Get cell text |
| `(qt-table-widget-set-horizontal-header! t col text)` | Set column header |
| `(qt-table-widget-set-vertical-header! t row text)` | Set row header |
| `(qt-table-widget-row-count t)` / `(qt-table-widget-set-row-count! t n)` | Row count |
| `(qt-table-widget-column-count t)` / `(qt-table-widget-set-column-count! t n)` | Column count |
| `(qt-table-widget-current-row t)` | Selected row |
| `(qt-table-widget-current-column t)` | Selected column |
| `(qt-table-widget-clear! t)` | Clear all cells |
| `(qt-on-cell-clicked! t handler)` | `(lambda () ...)` then query current row/col |

### Tab Widget

| Function | Description |
|----------|-------------|
| `(qt-tab-widget-create parent: #f)` | Create tab container |
| `(qt-tab-widget-add-tab! tw page label)` | Add a tab page |
| `(qt-tab-widget-current-index tw)` / `(qt-tab-widget-set-current-index! tw idx)` | Current tab |
| `(qt-tab-widget-count tw)` | Number of tabs |
| `(qt-tab-widget-set-tab-text! tw idx text)` | Rename tab |
| `(qt-on-tab-changed! tw handler)` | `(lambda (index) ...)` |

### Progress Bar

| Function | Description |
|----------|-------------|
| `(qt-progress-bar-create parent: #f)` | Create progress bar |
| `(qt-progress-bar-value p)` / `(qt-progress-bar-set-value! p val)` | Get/set value |
| `(qt-progress-bar-set-range! p min max)` | Set range |
| `(qt-progress-bar-set-format! p fmt)` | Set display format (`%v`, `%m`, `%p%`) |

### Slider

| Function | Description |
|----------|-------------|
| `(qt-slider-create orientation parent: #f)` | Create slider (`QT_HORIZONTAL` or `QT_VERTICAL`) |
| `(qt-slider-value s)` / `(qt-slider-set-value! s val)` | Get/set value |
| `(qt-slider-set-range! s min max)` | Set range |
| `(qt-slider-set-single-step! s step)` | Arrow key step |
| `(qt-slider-set-tick-interval! s interval)` | Tick mark spacing |
| `(qt-slider-set-tick-position! s pos)` | Tick position (use `QT_TICKS_*`) |
| `(qt-on-slider-value-changed! s handler)` | `(lambda (value) ...)` |

### Grid Layout

| Function | Description |
|----------|-------------|
| `(qt-grid-layout-create parent)` | Create grid layout |
| `(qt-grid-layout-add-widget! layout widget row col row-span: 1 col-span: 1)` | Add widget at position |
| `(qt-grid-layout-set-row-stretch! layout row stretch)` | Set row stretch factor |
| `(qt-grid-layout-set-column-stretch! layout col stretch)` | Set column stretch factor |
| `(qt-grid-layout-set-row-minimum-height! layout row height)` | Set minimum row height |
| `(qt-grid-layout-set-column-minimum-width! layout col width)` | Set minimum column width |

`qt-layout-set-spacing!` and `qt-layout-set-margins!` work on grid layouts too.

### Timer

| Function | Description |
|----------|-------------|
| `(qt-timer-create)` | Create timer (not a widget — must be explicitly destroyed) |
| `(qt-timer-start! timer msec)` | Start with interval in milliseconds |
| `(qt-timer-stop! timer)` | Stop timer |
| `(qt-timer-active? timer)` | Is timer running? |
| `(qt-timer-interval timer)` / `(qt-timer-set-interval! timer msec)` | Get/set interval |
| `(qt-timer-set-single-shot! timer bool)` | Fire only once |
| `(qt-on-timeout! timer handler)` | Connect timeout: `(lambda () ...)` |
| `(qt-timer-single-shot! msec handler)` | One-shot static convenience: `(lambda () ...)` |
| `(qt-timer-destroy! timer)` | Destroy timer |

### Clipboard

| Function | Description |
|----------|-------------|
| `(qt-clipboard-text app)` | Get clipboard text |
| `(qt-clipboard-set-text! app text)` | Set clipboard text |
| `(qt-on-clipboard-changed! app handler)` | Connect dataChanged: `(lambda () ...)` |

The clipboard is a singleton owned by QApplication — never destroy it.

### Tree Widget

| Function | Description |
|----------|-------------|
| `(qt-tree-widget-create parent: #f)` | Create tree widget |
| `(qt-tree-widget-set-column-count! t count)` | Set number of columns |
| `(qt-tree-widget-column-count t)` | Get column count |
| `(qt-tree-widget-set-header-label! t label)` | Set single header label |
| `(qt-tree-widget-set-header-labels! t labels)` | Set all header labels from a list |
| `(qt-tree-widget-set-header-item-text! t col text)` | Set individual header text |
| `(qt-tree-widget-add-top-level-item! t item)` | Add top-level item |
| `(qt-tree-widget-top-level-item-count t)` | Count top-level items |
| `(qt-tree-widget-top-level-item t index)` | Get top-level item by index |
| `(qt-tree-widget-current-item t)` | Get selected item (or `#f`) |
| `(qt-tree-widget-set-current-item! t item)` | Set selected item |
| `(qt-tree-widget-expand-item! t item)` / `(qt-tree-widget-collapse-item! t item)` | Expand/collapse item |
| `(qt-tree-widget-expand-all! t)` / `(qt-tree-widget-collapse-all! t)` | Expand/collapse all |
| `(qt-tree-widget-clear! t)` | Remove all items |
| `(qt-on-current-item-changed! t handler)` | `(lambda () ...)` |
| `(qt-on-tree-item-double-clicked! t handler)` | `(lambda () ...)` |
| `(qt-on-item-expanded! t handler)` | `(lambda () ...)` |
| `(qt-on-item-collapsed! t handler)` | `(lambda () ...)` |

### Tree Widget Item

| Function | Description |
|----------|-------------|
| `(qt-tree-item-create text)` | Create item with text (column 0) |
| `(qt-tree-item-text item column: 0)` | Get text at column |
| `(qt-tree-item-set-text! item text column: 0)` | Set text at column |
| `(qt-tree-item-add-child! parent child)` | Add child item |
| `(qt-tree-item-child-count item)` | Number of children |
| `(qt-tree-item-child item index)` | Get child by index |
| `(qt-tree-item-parent item)` | Get parent item (or `#f` for top-level) |
| `(qt-tree-item-expanded? item)` / `(qt-tree-item-set-expanded! item bool)` | Expansion state |

### Event Loop Integration

| Function | Description |
|----------|-------------|
| `(qt-start-timer! msec handler)` | Create + connect + start timer; returns timer handle |
| `(qt-run-with-timer! app msec handler)` | Start timer, run event loop, cleanup on exit |

### App-Wide Style Sheet

| Function | Description |
|----------|-------------|
| `(qt-app-set-style-sheet! app css)` | Apply CSS stylesheet to entire application |

Per-widget styling is also available via `qt-widget-set-style-sheet!`.

### Window State Management

| Function | Description |
|----------|-------------|
| `(qt-widget-show-minimized! w)` | Minimize window |
| `(qt-widget-show-maximized! w)` | Maximize window |
| `(qt-widget-show-fullscreen! w)` | Enter fullscreen |
| `(qt-widget-show-normal! w)` | Restore to normal state |
| `(qt-widget-window-state w)` | Get window state (returns `QT_WINDOW_*` constant) |
| `(qt-widget-move! w x y)` | Move widget to position |
| `(qt-widget-x w)` | Get x position |
| `(qt-widget-y w)` | Get y position |
| `(qt-widget-width w)` | Get width |
| `(qt-widget-height w)` | Get height |

### Scroll Area

| Function | Description |
|----------|-------------|
| `(qt-scroll-area-create parent: #f)` | Create scroll area |
| `(qt-scroll-area-set-widget! sa widget)` | Set scrollable content widget |
| `(qt-scroll-area-widget sa)` | Get content widget |
| `(qt-scroll-area-set-widget-resizable! sa bool)` | Auto-resize content to viewport |
| `(qt-scroll-area-set-horizontal-scrollbar-policy! sa policy)` | Set horizontal scrollbar policy |
| `(qt-scroll-area-set-vertical-scrollbar-policy! sa policy)` | Set vertical scrollbar policy |

Use `QT_SCROLLBAR_AS_NEEDED`, `QT_SCROLLBAR_ALWAYS_OFF`, or `QT_SCROLLBAR_ALWAYS_ON` for scrollbar policies.

### Splitter

| Function | Description |
|----------|-------------|
| `(qt-splitter-create orientation)` | Create splitter (`QT_HORIZONTAL` or `QT_VERTICAL`) |
| `(qt-splitter-add-widget! s widget)` | Add widget to splitter |
| `(qt-splitter-count s)` | Get number of widgets |
| `(qt-splitter-set-sizes! s sizes)` | Set pane sizes (list of 2 or 3 integers) |
| `(qt-splitter-size-at s index)` | Get size of pane at index |
| `(qt-splitter-set-stretch-factor! s index factor)` | Set stretch factor for pane |
| `(qt-splitter-set-handle-width! s width)` | Set divider handle width in pixels |
| `(qt-splitter-set-collapsible! s index bool)` | Set whether pane is collapsible |

### Keyboard Events

| Function | Description |
|----------|-------------|
| `(qt-on-key-press! widget handler)` | Install key press handler: `(lambda () ...)` |
| `(qt-last-key-code)` | Get last key code (use `QT_KEY_*` constants) |
| `(qt-last-key-modifiers)` | Get last modifier flags (use `QT_MOD_*` constants) |
| `(qt-last-key-text)` | Get last key text (printable character or empty) |

Key press handling uses the query pattern: the handler fires on each key press, then call `qt-last-key-code`, `qt-last-key-modifiers`, and `qt-last-key-text` to inspect the event.

### Pixmap (Image Loading)

| Function | Description |
|----------|-------------|
| `(qt-pixmap-load path)` | Load image from file path (returns handle even if file missing) |
| `(qt-pixmap-null? p)` | Check if pixmap loaded successfully |
| `(qt-pixmap-width p)` | Get width in pixels |
| `(qt-pixmap-height p)` | Get height in pixels |
| `(qt-pixmap-scaled p width height)` | Scale to size (returns new pixmap, keeps aspect ratio) |
| `(qt-pixmap-destroy! p)` | Destroy pixmap (not Qt-parented, must be explicit) |
| `(qt-label-set-pixmap! label pixmap)` | Display image on a QLabel |

### Icon

| Function | Description |
|----------|-------------|
| `(qt-icon-create path)` | Create icon from image file path |
| `(qt-icon-create-from-pixmap pixmap)` | Create icon from a QPixmap |
| `(qt-icon-null? icon)` | Check if icon has any available pixmaps |
| `(qt-icon-destroy! icon)` | Destroy icon (not Qt-parented, must be explicit) |
| `(qt-push-button-set-icon! button icon)` | Set icon on a push button |
| `(qt-action-set-icon! action icon)` | Set icon on an action (menus/toolbars) |
| `(qt-widget-set-window-icon! widget icon)` | Set window icon |

### Radio Button

| Function | Description |
|----------|-------------|
| `(qt-radio-button-create text parent: #f)` | Create radio button |
| `(qt-radio-button-text r)` | Get label text |
| `(qt-radio-button-set-text! r text)` | Set label text |
| `(qt-radio-button-checked? r)` | Is checked? |
| `(qt-radio-button-set-checked! r bool)` | Set checked state |
| `(qt-on-radio-toggled! r handler)` | Connect toggled: `(lambda (checked?) ...)` |

Radio buttons in the same parent widget are automatically exclusive. For explicit control, use a QButtonGroup.

### Button Group

| Function | Description |
|----------|-------------|
| `(qt-button-group-create)` | Create button group (NOT a widget, must be explicitly destroyed) |
| `(qt-button-group-add-button! bg button id)` | Add button with integer ID |
| `(qt-button-group-remove-button! bg button)` | Remove button from group |
| `(qt-button-group-checked-id bg)` | Get ID of checked button (-1 if none) |
| `(qt-button-group-exclusive? bg)` | Is exclusive? (default: `#t`) |
| `(qt-button-group-set-exclusive! bg bool)` | Toggle exclusivity |
| `(qt-on-button-group-clicked! bg handler)` | Connect idClicked: `(lambda (id) ...)` |
| `(qt-button-group-destroy! bg)` | Destroy button group |

### Group Box

| Function | Description |
|----------|-------------|
| `(qt-group-box-create title parent: #f)` | Create group box with title |
| `(qt-group-box-title gb)` | Get title |
| `(qt-group-box-set-title! gb title)` | Set title |
| `(qt-group-box-checkable? gb)` | Is checkable? |
| `(qt-group-box-set-checkable! gb bool)` | Make checkable (adds title checkbox) |
| `(qt-group-box-checked? gb)` | Is checked? (only meaningful when checkable) |
| `(qt-group-box-set-checked! gb bool)` | Set checked state |
| `(qt-on-group-box-toggled! gb handler)` | Connect toggled: `(lambda (checked?) ...)` |

Group boxes are widgets — they participate in Qt parent-child ownership. Use `qt-widget-destroy!` or let the parent destroy them.

### Constants

**Alignment:** `QT_ALIGN_LEFT`, `QT_ALIGN_RIGHT`, `QT_ALIGN_CENTER`, `QT_ALIGN_TOP`, `QT_ALIGN_BOTTOM`

**Echo Mode:** `QT_ECHO_NORMAL`, `QT_ECHO_NO_ECHO`, `QT_ECHO_PASSWORD`, `QT_ECHO_PASSWORD_ON_EDIT`

**Orientation:** `QT_HORIZONTAL`, `QT_VERTICAL`

**Tick Position:** `QT_TICKS_NONE`, `QT_TICKS_ABOVE`, `QT_TICKS_BELOW`, `QT_TICKS_BOTH_SIDES`

**Window State:** `QT_WINDOW_NO_STATE`, `QT_WINDOW_MINIMIZED`, `QT_WINDOW_MAXIMIZED`, `QT_WINDOW_FULL_SCREEN`

**Scrollbar Policy:** `QT_SCROLLBAR_AS_NEEDED`, `QT_SCROLLBAR_ALWAYS_OFF`, `QT_SCROLLBAR_ALWAYS_ON`

**Modifier Keys:** `QT_MOD_NONE`, `QT_MOD_SHIFT`, `QT_MOD_CTRL`, `QT_MOD_ALT`, `QT_MOD_META`

**Key Codes:** `QT_KEY_A` .. `QT_KEY_Z`, `QT_KEY_0` .. `QT_KEY_9`, `QT_KEY_F1` .. `QT_KEY_F12`, `QT_KEY_ESCAPE`, `QT_KEY_TAB`, `QT_KEY_BACKSPACE`, `QT_KEY_RETURN`, `QT_KEY_ENTER`, `QT_KEY_INSERT`, `QT_KEY_DELETE`, `QT_KEY_HOME`, `QT_KEY_END`, `QT_KEY_LEFT`, `QT_KEY_RIGHT`, `QT_KEY_UP`, `QT_KEY_DOWN`, `QT_KEY_PAGE_UP`, `QT_KEY_PAGE_DOWN`, `QT_KEY_SPACE`, `QT_KEY_MINUS`, `QT_KEY_PLUS`, `QT_KEY_COMMA`, `QT_KEY_PERIOD`, `QT_KEY_SLASH`

**Dock Area:** `QT_DOCK_LEFT`, `QT_DOCK_RIGHT`, `QT_DOCK_TOP`, `QT_DOCK_BOTTOM`

**Tray Message Icon:** `QT_TRAY_NO_ICON`, `QT_TRAY_INFO`, `QT_TRAY_WARNING`, `QT_TRAY_CRITICAL`

**Tray Activation:** `QT_TRAY_TRIGGER`, `QT_TRAY_CONTEXT`, `QT_TRAY_DOUBLE_CLICK`, `QT_TRAY_MIDDLE_CLICK`

**Frame Shape:** `QT_FRAME_NO_FRAME`, `QT_FRAME_BOX`, `QT_FRAME_PANEL`, `QT_FRAME_WIN_PANEL`, `QT_FRAME_HLINE`, `QT_FRAME_VLINE`, `QT_FRAME_STYLED_PANEL`

**Frame Shadow:** `QT_FRAME_PLAIN`, `QT_FRAME_RAISED`, `QT_FRAME_SUNKEN`

### Phase 8: Fonts, Colors, Dialogs, Dock, System Tray, Painter, Drag & Drop

#### QFont

| Function | Description |
|----------|-------------|
| `(qt-font-create family point-size: 12)` | Create font |
| `(qt-font-family f)` | Get font family name |
| `(qt-font-point-size f)` | Get point size |
| `(qt-font-bold? f)` | Check if bold |
| `(qt-font-set-bold! f bool)` | Set bold |
| `(qt-font-italic? f)` | Check if italic |
| `(qt-font-set-italic! f bool)` | Set italic |
| `(qt-font-destroy! f)` | Destroy font |
| `(qt-widget-set-font! w f)` | Set font on any widget |
| `(qt-widget-font w)` | Get widget's font (new copy) |

#### QColor

| Function | Description |
|----------|-------------|
| `(qt-color-create r g b alpha: 255)` | Create from RGB(A) |
| `(qt-color-create-name name)` | Create from "#rrggbb" or named color |
| `(qt-color-red c)` | Red channel (0-255) |
| `(qt-color-green c)` | Green channel |
| `(qt-color-blue c)` | Blue channel |
| `(qt-color-alpha c)` | Alpha channel |
| `(qt-color-name c)` | Hex string "#rrggbb" |
| `(qt-color-valid? c)` | Is color valid? |
| `(qt-color-destroy! c)` | Destroy color |

#### QFontDialog / QColorDialog

| Function | Description |
|----------|-------------|
| `(qt-font-dialog parent: #f)` | Show font picker, returns QFont or #f |
| `(qt-color-dialog initial: "#ffffff" parent: #f)` | Show color picker, returns QColor or #f |

#### QStackedWidget

| Function | Description |
|----------|-------------|
| `(qt-stacked-widget-create parent: #f)` | Create stacked widget |
| `(qt-stacked-widget-add-widget! sw w)` | Add page, returns index |
| `(qt-stacked-widget-set-current-index! sw idx)` | Switch page |
| `(qt-stacked-widget-current-index sw)` | Current page index |
| `(qt-stacked-widget-count sw)` | Number of pages |
| `(qt-on-stacked-changed! sw handler)` | Signal: `(lambda (idx) ...)` |

#### QDockWidget

| Function | Description |
|----------|-------------|
| `(qt-dock-widget-create title parent: #f)` | Create dockable panel |
| `(qt-dock-widget-set-widget! dw w)` | Set content widget |
| `(qt-dock-widget-widget dw)` | Get content widget |
| `(qt-dock-widget-title dw)` | Get title |
| `(qt-dock-widget-set-title! dw title)` | Set title |
| `(qt-dock-widget-set-floating! dw bool)` | Set floating |
| `(qt-dock-widget-floating? dw)` | Is floating? |
| `(qt-main-window-add-dock-widget! mw area dw)` | Dock to main window |

#### QSystemTrayIcon

| Function | Description |
|----------|-------------|
| `(qt-system-tray-icon-create icon parent: #f)` | Create tray icon |
| `(qt-system-tray-icon-set-tooltip! ti text)` | Set tooltip |
| `(qt-system-tray-icon-set-icon! ti icon)` | Change icon |
| `(qt-system-tray-icon-show! ti)` | Show in tray |
| `(qt-system-tray-icon-hide! ti)` | Hide from tray |
| `(qt-system-tray-icon-show-message! ti title msg icon-type: timeout:)` | Balloon message |
| `(qt-system-tray-icon-set-context-menu! ti menu)` | Set right-click menu |
| `(qt-on-tray-activated! ti handler)` | Signal: `(lambda (reason) ...)` |
| `(qt-system-tray-available?)` | Check if tray is available |
| `(qt-system-tray-icon-destroy! ti)` | Destroy tray icon |

#### QPainter (paint onto QPixmap)

| Function | Description |
|----------|-------------|
| `(qt-pixmap-create-blank w h)` | Create empty pixmap |
| `(qt-pixmap-fill! pm r g b alpha: 255)` | Fill with color |
| `(qt-painter-create pixmap)` | Begin painting on pixmap |
| `(qt-painter-end! p)` | Finish painting |
| `(qt-painter-destroy! p)` | Destroy painter |
| `(qt-painter-set-pen-color! p r g b alpha: 255)` | Set stroke color |
| `(qt-painter-set-pen-width! p width)` | Set stroke width |
| `(qt-painter-set-brush-color! p r g b alpha: 255)` | Set fill color |
| `(qt-painter-set-font!* p font)` | Set painter font |
| `(qt-painter-set-antialiasing! p bool)` | Enable antialiasing |
| `(qt-painter-draw-line! p x1 y1 x2 y2)` | Draw line |
| `(qt-painter-draw-rect! p x y w h)` | Draw outlined rectangle |
| `(qt-painter-fill-rect! p x y w h r g b alpha: 255)` | Draw filled rectangle |
| `(qt-painter-draw-ellipse! p x y w h)` | Draw ellipse |
| `(qt-painter-draw-text! p x y text)` | Draw text at point |
| `(qt-painter-draw-text-rect! p x y w h flags text)` | Draw text in rect |
| `(qt-painter-draw-pixmap! p x y pixmap)` | Composite pixmap |
| `(qt-painter-draw-point! p x y)` | Draw single point |
| `(qt-painter-draw-arc! p x y w h start span)` | Draw arc (1/16th degrees) |
| `(qt-painter-save! p)` | Save painter state |
| `(qt-painter-restore! p)` | Restore painter state |
| `(qt-painter-translate! p dx dy)` | Translate origin |
| `(qt-painter-rotate! p angle)` | Rotate (degrees) |
| `(qt-painter-scale! p sx sy)` | Scale |

#### Drag and Drop (text-only)

| Function | Description |
|----------|-------------|
| `(qt-widget-set-accept-drops! w bool)` | Enable/disable drop acceptance |
| `(qt-on-drop! widget handler)` | Install drop filter: `(lambda (text) ...)` |
| `(qt-drop-filter-last-text df)` | Last dropped text |
| `(qt-drop-filter-destroy! df)` | Remove drop filter |
| `(qt-drag-text! source text)` | Initiate drag with text |

### Phase 9: Practical Widgets & Dialog Enhancements

#### QDoubleSpinBox

| Function | Description |
|----------|-------------|
| `(qt-double-spin-box-create parent: #f)` | Create float spin box |
| `(qt-double-spin-box-value dsb)` | Get current value (double) |
| `(qt-double-spin-box-set-value! dsb val)` | Set value (double) |
| `(qt-double-spin-box-set-range! dsb min max)` | Set min/max range |
| `(qt-double-spin-box-set-single-step! dsb step)` | Set step increment |
| `(qt-double-spin-box-set-decimals! dsb n)` | Set decimal places |
| `(qt-double-spin-box-decimals dsb)` | Get decimal places |
| `(qt-double-spin-box-set-prefix! dsb str)` | Set display prefix (e.g. "$ ") |
| `(qt-double-spin-box-set-suffix! dsb str)` | Set display suffix (e.g. " kg") |
| `(qt-on-double-value-changed! dsb handler)` | Connect: `(lambda (double-val) ...)` |

#### QDateEdit

| Function | Description |
|----------|-------------|
| `(qt-date-edit-create parent: #f)` | Create date picker |
| `(qt-date-edit-set-date! d year month day)` | Set date |
| `(qt-date-edit-year d)` | Get year |
| `(qt-date-edit-month d)` | Get month (1-12) |
| `(qt-date-edit-day d)` | Get day (1-31) |
| `(qt-date-edit-date-string d)` | Get ISO date string ("YYYY-MM-DD") |
| `(qt-date-edit-set-minimum-date! d y m d)` | Set minimum date |
| `(qt-date-edit-set-maximum-date! d y m d)` | Set maximum date |
| `(qt-date-edit-set-calendar-popup! d bool)` | Enable calendar popup |
| `(qt-date-edit-set-display-format! d fmt)` | Set format (e.g. "yyyy-MM-dd") |
| `(qt-on-date-changed! d handler)` | Connect: `(lambda (iso-str) ...)` |

#### QTimeEdit

| Function | Description |
|----------|-------------|
| `(qt-time-edit-create parent: #f)` | Create time picker |
| `(qt-time-edit-set-time! t hour minute second)` | Set time |
| `(qt-time-edit-hour t)` | Get hour (0-23) |
| `(qt-time-edit-minute t)` | Get minute (0-59) |
| `(qt-time-edit-second t)` | Get second (0-59) |
| `(qt-time-edit-time-string t)` | Get ISO time string ("HH:MM:SS") |
| `(qt-time-edit-set-display-format! t fmt)` | Set format (e.g. "HH:mm") |
| `(qt-on-time-changed! t handler)` | Connect: `(lambda (iso-str) ...)` |

#### QFrame

| Function | Description |
|----------|-------------|
| `(qt-frame-create parent: #f)` | Create frame container |
| `(qt-frame-set-shape! f shape)` | Set frame shape constant |
| `(qt-frame-shape f)` | Get frame shape |
| `(qt-frame-set-shadow! f shadow)` | Set frame shadow constant |
| `(qt-frame-shadow f)` | Get frame shadow |
| `(qt-frame-set-line-width! f w)` | Set border width |
| `(qt-frame-line-width f)` | Get border width |
| `(qt-frame-set-mid-line-width! f w)` | Set mid-line width (for raised/sunken) |

QFrame is a QWidget — use layouts inside it and destroy via parent-child ownership.

#### QProgressDialog

| Function | Description |
|----------|-------------|
| `(qt-progress-dialog-create label cancel min max parent: #f)` | Create progress dialog |
| `(qt-progress-dialog-set-value! pd val)` | Set progress value |
| `(qt-progress-dialog-value pd)` | Get current value |
| `(qt-progress-dialog-set-range! pd min max)` | Set range |
| `(qt-progress-dialog-set-label-text! pd text)` | Update label text |
| `(qt-progress-dialog-canceled? pd)` | Check if user clicked Cancel |
| `(qt-progress-dialog-set-minimum-duration! pd ms)` | Set show delay (ms) |
| `(qt-progress-dialog-set-auto-close! pd bool)` | Auto-close on completion |
| `(qt-progress-dialog-set-auto-reset! pd bool)` | Auto-reset on completion |
| `(qt-progress-dialog-reset! pd)` | Reset to initial state |
| `(qt-on-progress-canceled! pd handler)` | Connect: `(lambda () ...)` |

#### QInputDialog

| Function | Description |
|----------|-------------|
| `(qt-input-dialog-get-text title label default: "" parent: #f)` | Get text from user; returns string or `#f` |
| `(qt-input-dialog-get-int title label value: 0 min: ... max: ... step: 1 parent: #f)` | Get integer; returns int or `#f` |
| `(qt-input-dialog-get-double title label value: 0.0 min: ... max: ... decimals: 1 parent: #f)` | Get double; returns double or `#f` |
| `(qt-input-dialog-get-item title label items current: 0 editable: #f parent: #f)` | Choose from list; returns string or `#f` |

All QInputDialog functions are blocking modal dialogs. They return the value on accept, or `#f` on cancel. The `items` parameter is a Scheme list of strings.

### Phase 10: Forms, Calendar, Rich Text, Dialog Buttons, Shortcuts

#### QFormLayout

| Function | Description |
|----------|-------------|
| `(qt-form-layout-create parent: #f)` | Create form layout (label:field rows) |
| `(qt-form-layout-add-row! form label widget)` | Add labeled row |
| `(qt-form-layout-add-row-widget! form widget)` | Add spanning widget |
| `(qt-form-layout-row-count form)` | Get number of rows |

QFormLayout reuses the layout type — `qt-layout-set-spacing!` and `qt-layout-set-margins!` work on it.

#### QCalendarWidget

| Function | Description |
|----------|-------------|
| `(qt-calendar-create parent: #f)` | Create calendar widget |
| `(qt-calendar-set-selected-date! cal year month day)` | Set selected date |
| `(qt-calendar-selected-year cal)` | Get selected year |
| `(qt-calendar-selected-month cal)` | Get selected month (1-12) |
| `(qt-calendar-selected-day cal)` | Get selected day (1-31) |
| `(qt-calendar-selected-date-string cal)` | Get ISO date string ("YYYY-MM-DD") |
| `(qt-calendar-set-minimum-date! cal year month day)` | Set minimum selectable date |
| `(qt-calendar-set-maximum-date! cal year month day)` | Set maximum selectable date |
| `(qt-calendar-set-first-day-of-week! cal day)` | Set first day (use `QT_MONDAY`..`QT_SUNDAY`) |
| `(qt-calendar-set-grid-visible! cal bool)` | Show/hide grid lines |
| `(qt-calendar-grid-visible? cal)` | Is grid visible? |
| `(qt-calendar-set-navigation-bar-visible! cal bool)` | Show/hide navigation bar |
| `(qt-on-selection-changed! cal handler)` | Selection changed: `(lambda () ...)` |
| `(qt-on-calendar-clicked! cal handler)` | Date clicked: `(lambda (iso-date) ...)` |

#### QTextBrowser

| Function | Description |
|----------|-------------|
| `(qt-text-browser-create parent: #f)` | Create rich text/HTML viewer |
| `(qt-text-browser-set-html! tb html)` | Set HTML content |
| `(qt-text-browser-set-plain-text! tb text)` | Set plain text content |
| `(qt-text-browser-plain-text tb)` | Get plain text content |
| `(qt-text-browser-set-open-external-links! tb bool)` | Enable browser launch on link click |
| `(qt-text-browser-set-source! tb url)` | Load content from URL/file |
| `(qt-text-browser-source tb)` | Get current source URL |
| `(qt-on-anchor-clicked! tb handler)` | Link clicked: `(lambda (url) ...)` |

#### QDialogButtonBox

| Function | Description |
|----------|-------------|
| `(qt-button-box-create flags parent: #f)` | Create with standard button flags (bitwise-ior) |
| `(qt-button-box-button bb role)` | Get QPushButton by standard button constant |
| `(qt-button-box-add-button! bb text role)` | Add custom button with role |
| `(qt-on-accepted! bb handler)` | Accept clicked: `(lambda () ...)` |
| `(qt-on-rejected! bb handler)` | Reject clicked: `(lambda () ...)` |
| `(qt-on-button-clicked! bb handler)` | Any button clicked: `(lambda () ...)` |

**Standard buttons:** `QT_BUTTON_OK`, `QT_BUTTON_CANCEL`, `QT_BUTTON_APPLY`, `QT_BUTTON_CLOSE`, `QT_BUTTON_YES`, `QT_BUTTON_NO`, `QT_BUTTON_RESET`, `QT_BUTTON_HELP`, `QT_BUTTON_SAVE`, `QT_BUTTON_DISCARD`

**Button roles:** `QT_BUTTON_ACCEPT_ROLE`, `QT_BUTTON_REJECT_ROLE`, `QT_BUTTON_RESET_ROLE`, `QT_BUTTON_APPLY_ROLE`, `QT_BUTTON_HELP_ROLE`

Combine standard buttons with `bitwise-ior`: `(bitwise-ior QT_BUTTON_OK QT_BUTTON_CANCEL)`

#### QShortcut

| Function | Description |
|----------|-------------|
| `(qt-shortcut-create key-sequence parent)` | Create shortcut (e.g. `"Ctrl+S"`) |
| `(qt-shortcut-set-key! sc key-sequence)` | Change key sequence |
| `(qt-shortcut-set-enabled! sc bool)` | Enable/disable |
| `(qt-shortcut-enabled? sc)` | Is enabled? |
| `(qt-on-shortcut-activated! sc handler)` | Shortcut triggered: `(lambda () ...)` |
| `(qt-shortcut-destroy! sc)` | Destroy shortcut (QObject, not QWidget) |

**Day-of-week constants:** `QT_MONDAY`, `QT_TUESDAY`, `QT_WEDNESDAY`, `QT_THURSDAY`, `QT_FRIDAY`, `QT_SATURDAY`, `QT_SUNDAY`

## Architecture

The binding uses a three-layer architecture:

1. **C++ shim** (`vendor/qt_shim.h`, `vendor/qt_shim.cpp`) -- thin `extern "C"` wrappers around Qt6 C++ classes. All Qt objects are opaque `void*` handles. Compiled as `libqt_shim.so`.

2. **FFI layer** (`libqt.ss`) -- Gambit `begin-ffi` bindings with `define-c-lambda` for each C function. Includes the callback trampoline system: 4 static trampolines (void/string/int/bool) dispatch to Scheme closures via hash table lookup.

3. **High-level API** (`qt.ss`) -- Gerbil-idiomatic wrappers with keyword arguments, boolean conversions, and the `with-qt-app` macro.

### Callback Pattern

Qt signals are connected to Scheme handlers through a callback trampoline system:

```
Qt signal -> C++ lambda -> static trampoline -> c-define'd Scheme function
  -> hash table lookup -> user's Scheme closure
```

Four callback types cover all signals: `void`, `string(text)`, `int(value)`, `bool(checked)`.

### Memory Management

Qt uses parent-child ownership: destroying a parent automatically destroys all children. No GC finalizers are used on parented widgets. The QApplication must be explicitly destroyed (or use `with-qt-app`).

## Examples

- `examples/hello.ss` -- Minimal window with label
- `examples/counter.ss` -- Button click counter
- `examples/form.ss` -- Form with text inputs, checkboxes, combo box, spin box, message boxes
- `examples/editor.ss` -- Text editor with menus, toolbar, keyboard shortcuts, file I/O
- `examples/dashboard.ss` -- Tabbed dashboard with todo list, data table, slider + progress bar
- `examples/filebrowser.ss` -- File browser with tree view, grid layout, timer clock, clipboard
- `examples/styled.ss` -- Dark-themed split-pane app with scroll area, key events, fullscreen toggle
- `examples/settings.ss` -- Settings dialog with radio buttons, button groups, group boxes, dark theme
- `examples/painter.ss` -- QPainter demo with shapes, text, transforms, compositing
- `examples/datainput.ss` -- Data input with double spin box, date/time pickers, frames, progress dialog, input dialogs
- `examples/planner.ss` -- Event planner with form layout, calendar, rich text preview, dialog buttons, shortcuts

## License

MIT
