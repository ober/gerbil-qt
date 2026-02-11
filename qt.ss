(export
  ;; Lifecycle
  qt-app-create qt-app-exec! qt-app-quit!
  qt-app-process-events! qt-app-destroy!
  with-qt-app

  ;; Widget
  qt-widget-create qt-widget-show! qt-widget-hide! qt-widget-close!
  qt-widget-set-enabled! qt-widget-enabled?
  qt-widget-set-visible! qt-widget-visible? qt-widget-set-fixed-size!
  qt-widget-set-minimum-size! qt-widget-set-maximum-size!
  qt-widget-set-minimum-width! qt-widget-set-minimum-height!
  qt-widget-set-maximum-width! qt-widget-set-maximum-height!
  qt-widget-set-cursor! qt-widget-unset-cursor!
  qt-widget-resize! qt-widget-set-style-sheet!
  qt-widget-set-tooltip! qt-widget-set-font-size!
  qt-widget-destroy!

  ;; Main Window
  qt-main-window-create qt-main-window-set-title!
  qt-main-window-set-central-widget!

  ;; Layouts
  qt-vbox-layout-create qt-hbox-layout-create
  qt-layout-add-widget! qt-layout-add-stretch!
  qt-layout-set-spacing! qt-layout-set-margins!

  ;; Labels
  qt-label-create qt-label-set-text! qt-label-text
  qt-label-set-alignment! qt-label-set-word-wrap!

  ;; Push Button
  qt-push-button-create qt-push-button-set-text! qt-push-button-text
  qt-on-clicked!

  ;; Line Edit
  qt-line-edit-create qt-line-edit-text qt-line-edit-set-text!
  qt-line-edit-set-placeholder! qt-line-edit-set-read-only!
  qt-line-edit-set-echo-mode!
  qt-on-text-changed! qt-on-return-pressed!

  ;; Check Box
  qt-check-box-create qt-check-box-checked? qt-check-box-set-checked!
  qt-check-box-set-text! qt-on-toggled!

  ;; Combo Box
  qt-combo-box-create qt-combo-box-add-item! qt-combo-box-set-current-index!
  qt-combo-box-current-index qt-combo-box-current-text
  qt-combo-box-count qt-combo-box-clear!
  qt-on-index-changed!

  ;; Text Edit
  qt-text-edit-create qt-text-edit-text qt-text-edit-set-text!
  qt-text-edit-set-placeholder! qt-text-edit-set-read-only!
  qt-text-edit-append! qt-text-edit-clear!
  qt-text-edit-scroll-to-bottom! qt-text-edit-html
  qt-on-text-edit-changed!

  ;; Spin Box
  qt-spin-box-create qt-spin-box-value qt-spin-box-set-value!
  qt-spin-box-set-range! qt-spin-box-set-single-step!
  qt-spin-box-set-prefix! qt-spin-box-set-suffix!
  qt-on-value-changed!

  ;; Dialog
  qt-dialog-create qt-dialog-exec! qt-dialog-accept! qt-dialog-reject!
  qt-dialog-set-title!

  ;; Message Box
  qt-message-box-information qt-message-box-warning
  qt-message-box-question qt-message-box-critical

  ;; File Dialog
  qt-file-dialog-open-file qt-file-dialog-save-file
  qt-file-dialog-open-directory

  ;; Menu Bar
  qt-main-window-menu-bar

  ;; Menu
  qt-menu-bar-add-menu qt-menu-add-menu
  qt-menu-add-action! qt-menu-add-separator!

  ;; Action
  qt-action-create qt-action-text qt-action-set-text!
  qt-action-set-shortcut! qt-action-set-enabled! qt-action-enabled?
  qt-action-set-checkable! qt-action-checkable?
  qt-action-set-checked! qt-action-checked?
  qt-action-set-tooltip! qt-action-set-status-tip!
  qt-on-triggered! qt-on-action-toggled!

  ;; Toolbar
  qt-toolbar-create qt-main-window-add-toolbar!
  qt-toolbar-add-action! qt-toolbar-add-separator!
  qt-toolbar-add-widget! qt-toolbar-set-movable!
  qt-toolbar-set-icon-size!

  ;; Status Bar
  qt-main-window-set-status-bar-text!

  ;; List Widget
  qt-list-widget-create qt-list-widget-add-item!
  qt-list-widget-insert-item! qt-list-widget-remove-item!
  qt-list-widget-current-row qt-list-widget-set-current-row!
  qt-list-widget-item-text qt-list-widget-count qt-list-widget-clear!
  qt-list-widget-set-item-data! qt-list-widget-item-data
  qt-on-current-row-changed! qt-on-item-double-clicked!

  ;; Table Widget
  qt-table-widget-create qt-table-widget-set-item!
  qt-table-widget-item-text
  qt-table-widget-set-horizontal-header! qt-table-widget-set-vertical-header!
  qt-table-widget-set-row-count! qt-table-widget-set-column-count!
  qt-table-widget-row-count qt-table-widget-column-count
  qt-table-widget-current-row qt-table-widget-current-column
  qt-table-widget-clear! qt-on-cell-clicked!

  ;; Tab Widget
  qt-tab-widget-create qt-tab-widget-add-tab!
  qt-tab-widget-set-current-index! qt-tab-widget-current-index
  qt-tab-widget-count qt-tab-widget-set-tab-text!
  qt-on-tab-changed!

  ;; Progress Bar
  qt-progress-bar-create qt-progress-bar-set-value!
  qt-progress-bar-value qt-progress-bar-set-range!
  qt-progress-bar-set-format!

  ;; Slider
  qt-slider-create qt-slider-set-value! qt-slider-value
  qt-slider-set-range! qt-slider-set-single-step!
  qt-slider-set-tick-interval! qt-slider-set-tick-position!
  qt-on-slider-value-changed!

  ;; Grid Layout
  qt-grid-layout-create qt-grid-layout-add-widget!
  qt-grid-layout-set-row-stretch! qt-grid-layout-set-column-stretch!
  qt-grid-layout-set-row-minimum-height! qt-grid-layout-set-column-minimum-width!

  ;; Timer
  qt-timer-create qt-timer-start! qt-timer-stop!
  qt-timer-set-single-shot! qt-timer-active?
  qt-timer-interval qt-timer-set-interval!
  qt-on-timeout! qt-timer-single-shot!
  qt-timer-destroy!

  ;; Clipboard
  qt-clipboard-text qt-clipboard-set-text! qt-on-clipboard-changed!

  ;; Tree Widget
  qt-tree-widget-create qt-tree-widget-set-column-count!
  qt-tree-widget-column-count qt-tree-widget-set-header-label!
  qt-tree-widget-set-header-labels! qt-tree-widget-set-header-item-text!
  qt-tree-widget-add-top-level-item! qt-tree-widget-top-level-item-count
  qt-tree-widget-top-level-item qt-tree-widget-current-item
  qt-tree-widget-set-current-item!
  qt-tree-widget-expand-item! qt-tree-widget-collapse-item!
  qt-tree-widget-expand-all! qt-tree-widget-collapse-all!
  qt-tree-widget-clear!
  qt-on-current-item-changed! qt-on-tree-item-double-clicked!
  qt-on-item-expanded! qt-on-item-collapsed!

  ;; Tree Widget Item
  qt-tree-item-create qt-tree-item-set-text! qt-tree-item-text
  qt-tree-item-add-child! qt-tree-item-child-count
  qt-tree-item-child qt-tree-item-parent
  qt-tree-item-set-expanded! qt-tree-item-expanded?

  ;; Event Loop Integration
  qt-start-timer! qt-run-with-timer!

  ;; App-wide Style Sheet
  qt-app-set-style-sheet!

  ;; Window State Management
  qt-widget-show-minimized! qt-widget-show-maximized!
  qt-widget-show-fullscreen! qt-widget-show-normal!
  qt-widget-window-state qt-widget-move!
  qt-widget-x qt-widget-y qt-widget-width qt-widget-height

  ;; Scroll Area
  qt-scroll-area-create qt-scroll-area-set-widget!
  qt-scroll-area-set-widget-resizable!
  qt-scroll-area-set-horizontal-scrollbar-policy!
  qt-scroll-area-set-vertical-scrollbar-policy!

  ;; Splitter
  qt-splitter-create qt-splitter-add-widget! qt-splitter-count
  qt-splitter-set-sizes! qt-splitter-size-at
  qt-splitter-set-stretch-factor! qt-splitter-set-handle-width!
  qt-splitter-set-collapsible! qt-splitter-collapsible?

  ;; Keyboard Events
  qt-on-key-press! qt-on-key-press-consuming!
  qt-last-key-code qt-last-key-modifiers qt-last-key-text

  ;; Pixmap
  qt-pixmap-load qt-pixmap-width qt-pixmap-height
  qt-pixmap-null? qt-pixmap-scaled qt-pixmap-destroy!
  qt-label-set-pixmap!

  ;; Icon
  qt-icon-create qt-icon-create-from-pixmap
  qt-icon-null? qt-icon-destroy!
  qt-push-button-set-icon! qt-action-set-icon!
  qt-widget-set-window-icon!

  ;; Radio Button
  qt-radio-button-create qt-radio-button-text qt-radio-button-set-text!
  qt-radio-button-checked? qt-radio-button-set-checked!
  qt-on-radio-toggled!

  ;; Button Group
  qt-button-group-create qt-button-group-add-button!
  qt-button-group-remove-button! qt-button-group-checked-id
  qt-button-group-set-exclusive! qt-button-group-exclusive?
  qt-on-button-group-clicked! qt-button-group-destroy!

  ;; Group Box
  qt-group-box-create qt-group-box-title qt-group-box-set-title!
  qt-group-box-set-checkable! qt-group-box-checkable?
  qt-group-box-set-checked! qt-group-box-checked?
  qt-on-group-box-toggled!

  ;; Font
  qt-font-create qt-font-family qt-font-point-size
  qt-font-bold? qt-font-set-bold! qt-font-italic? qt-font-set-italic!
  qt-font-destroy! qt-widget-set-font! qt-widget-font

  ;; Color
  qt-color-create qt-color-create-name
  qt-color-red qt-color-green qt-color-blue qt-color-alpha
  qt-color-name qt-color-valid? qt-color-destroy!

  ;; Font Dialog
  qt-font-dialog

  ;; Color Dialog
  qt-color-dialog

  ;; Stacked Widget
  qt-stacked-widget-create qt-stacked-widget-add-widget!
  qt-stacked-widget-set-current-index! qt-stacked-widget-current-index
  qt-stacked-widget-count qt-on-stacked-changed!

  ;; Dock Widget
  qt-dock-widget-create qt-dock-widget-set-widget!
  qt-dock-widget-widget qt-dock-widget-title qt-dock-widget-set-title!
  qt-dock-widget-set-floating! qt-dock-widget-floating?
  qt-main-window-add-dock-widget!

  ;; System Tray Icon
  qt-system-tray-icon-create qt-system-tray-icon-set-tooltip!
  qt-system-tray-icon-set-icon! qt-system-tray-icon-show!
  qt-system-tray-icon-hide! qt-system-tray-icon-show-message!
  qt-system-tray-icon-set-context-menu!
  qt-on-tray-activated! qt-system-tray-available?
  qt-system-tray-icon-destroy!

  ;; QPainter
  qt-pixmap-create-blank qt-pixmap-fill!
  qt-painter-create qt-painter-end! qt-painter-destroy!
  qt-painter-set-pen-color! qt-painter-set-pen-width!
  qt-painter-set-brush-color! qt-painter-set-font!*
  qt-painter-set-antialiasing!
  qt-painter-draw-line! qt-painter-draw-rect! qt-painter-fill-rect!
  qt-painter-draw-ellipse! qt-painter-draw-text! qt-painter-draw-text-rect!
  qt-painter-draw-pixmap! qt-painter-draw-point! qt-painter-draw-arc!
  qt-painter-save! qt-painter-restore!
  qt-painter-translate! qt-painter-rotate! qt-painter-scale!

  ;; Drag and Drop
  qt-widget-set-accept-drops! qt-on-drop!
  qt-drop-filter-last-text qt-drop-filter-destroy!
  qt-drag-text!

  ;; Double Spin Box
  qt-double-spin-box-create qt-double-spin-box-value qt-double-spin-box-set-value!
  qt-double-spin-box-set-range! qt-double-spin-box-set-single-step!
  qt-double-spin-box-set-decimals! qt-double-spin-box-decimals
  qt-double-spin-box-set-prefix! qt-double-spin-box-set-suffix!
  qt-on-double-value-changed!

  ;; Date Edit
  qt-date-edit-create qt-date-edit-set-date!
  qt-date-edit-year qt-date-edit-month qt-date-edit-day
  qt-date-edit-date-string
  qt-date-edit-set-minimum-date! qt-date-edit-set-maximum-date!
  qt-date-edit-set-calendar-popup! qt-date-edit-set-display-format!
  qt-on-date-changed!

  ;; Time Edit
  qt-time-edit-create qt-time-edit-set-time!
  qt-time-edit-hour qt-time-edit-minute qt-time-edit-second
  qt-time-edit-time-string qt-time-edit-set-display-format!
  qt-on-time-changed!

  ;; Frame
  qt-frame-create qt-frame-set-shape! qt-frame-shape
  qt-frame-set-shadow! qt-frame-shadow
  qt-frame-set-line-width! qt-frame-line-width
  qt-frame-set-mid-line-width!

  ;; Progress Dialog
  qt-progress-dialog-create qt-progress-dialog-set-value! qt-progress-dialog-value
  qt-progress-dialog-set-range! qt-progress-dialog-set-label-text!
  qt-progress-dialog-canceled? qt-progress-dialog-set-minimum-duration!
  qt-progress-dialog-set-auto-close! qt-progress-dialog-set-auto-reset!
  qt-progress-dialog-reset! qt-on-progress-canceled!

  ;; Input Dialog
  qt-input-dialog-get-text qt-input-dialog-get-int
  qt-input-dialog-get-double qt-input-dialog-get-item

  ;; Form Layout
  qt-form-layout-create qt-form-layout-add-row!
  qt-form-layout-add-row-widget! qt-form-layout-add-spanning-widget!
  qt-form-layout-row-count

  ;; Shortcut
  qt-shortcut-create qt-shortcut-set-key! qt-shortcut-set-enabled!
  qt-shortcut-enabled? qt-on-shortcut-activated! qt-shortcut-destroy!

  ;; Text Browser
  qt-text-browser-create qt-text-browser-set-html!
  qt-text-browser-set-plain-text! qt-text-browser-plain-text
  qt-text-browser-set-open-external-links!
  qt-text-browser-set-source! qt-text-browser-source
  qt-text-browser-scroll-to-bottom! qt-text-browser-append!
  qt-text-browser-html
  qt-on-anchor-clicked!

  ;; Dialog Button Box
  qt-button-box-create qt-button-box-button
  qt-button-box-add-button!
  qt-on-accepted! qt-on-rejected! qt-on-button-clicked!

  ;; Calendar Widget
  qt-calendar-create qt-calendar-set-selected-date!
  qt-calendar-selected-year qt-calendar-selected-month
  qt-calendar-selected-day qt-calendar-selected-date-string
  qt-calendar-set-minimum-date! qt-calendar-set-maximum-date!
  qt-calendar-set-first-day-of-week! qt-calendar-set-grid-visible!
  qt-calendar-grid-visible? qt-calendar-set-navigation-bar-visible!
  qt-on-selection-changed! qt-on-calendar-clicked!

  ;; Constants
  QT_ALIGN_LEFT QT_ALIGN_RIGHT QT_ALIGN_CENTER
  QT_ALIGN_TOP QT_ALIGN_BOTTOM
  QT_ECHO_NORMAL QT_ECHO_NO_ECHO QT_ECHO_PASSWORD QT_ECHO_PASSWORD_ON_EDIT
  QT_HORIZONTAL QT_VERTICAL
  QT_TICKS_NONE QT_TICKS_ABOVE QT_TICKS_BELOW QT_TICKS_BOTH_SIDES
  QT_WINDOW_NO_STATE QT_WINDOW_MINIMIZED QT_WINDOW_MAXIMIZED QT_WINDOW_FULL_SCREEN
  QT_SCROLLBAR_AS_NEEDED QT_SCROLLBAR_ALWAYS_OFF QT_SCROLLBAR_ALWAYS_ON
  QT_CURSOR_ARROW QT_CURSOR_CROSS QT_CURSOR_WAIT QT_CURSOR_IBEAM
  QT_CURSOR_POINTING_HAND QT_CURSOR_FORBIDDEN QT_CURSOR_BUSY
  QT_KEY_A QT_KEY_B QT_KEY_C QT_KEY_D QT_KEY_E QT_KEY_F
  QT_KEY_G QT_KEY_H QT_KEY_I QT_KEY_J QT_KEY_K QT_KEY_L
  QT_KEY_M QT_KEY_N QT_KEY_O QT_KEY_P QT_KEY_Q QT_KEY_R
  QT_KEY_S QT_KEY_T QT_KEY_U QT_KEY_V QT_KEY_W QT_KEY_X
  QT_KEY_Y QT_KEY_Z
  QT_KEY_0 QT_KEY_1 QT_KEY_2 QT_KEY_3 QT_KEY_4
  QT_KEY_5 QT_KEY_6 QT_KEY_7 QT_KEY_8 QT_KEY_9
  QT_KEY_ESCAPE QT_KEY_TAB QT_KEY_BACKSPACE QT_KEY_RETURN
  QT_KEY_ENTER QT_KEY_INSERT QT_KEY_DELETE QT_KEY_SPACE
  QT_KEY_UP QT_KEY_DOWN QT_KEY_LEFT QT_KEY_RIGHT
  QT_KEY_HOME QT_KEY_END QT_KEY_PAGE_UP QT_KEY_PAGE_DOWN
  QT_KEY_F1 QT_KEY_F2 QT_KEY_F3 QT_KEY_F4 QT_KEY_F5 QT_KEY_F6
  QT_KEY_F7 QT_KEY_F8 QT_KEY_F9 QT_KEY_F10 QT_KEY_F11 QT_KEY_F12
  QT_MOD_NONE QT_MOD_SHIFT QT_MOD_CTRL QT_MOD_ALT QT_MOD_META
  QT_DOCK_LEFT QT_DOCK_RIGHT QT_DOCK_TOP QT_DOCK_BOTTOM
  QT_TRAY_NO_ICON QT_TRAY_INFO QT_TRAY_WARNING QT_TRAY_CRITICAL
  QT_TRAY_TRIGGER QT_TRAY_CONTEXT QT_TRAY_DOUBLE_CLICK QT_TRAY_MIDDLE_CLICK
  QT_FRAME_NO_FRAME QT_FRAME_BOX QT_FRAME_PANEL QT_FRAME_WIN_PANEL
  QT_FRAME_HLINE QT_FRAME_VLINE QT_FRAME_STYLED_PANEL
  QT_FRAME_PLAIN QT_FRAME_RAISED QT_FRAME_SUNKEN
  QT_BUTTON_OK QT_BUTTON_CANCEL QT_BUTTON_APPLY QT_BUTTON_CLOSE
  QT_BUTTON_YES QT_BUTTON_NO QT_BUTTON_RESET QT_BUTTON_HELP
  QT_BUTTON_SAVE QT_BUTTON_DISCARD
  QT_BUTTON_ROLE_INVALID QT_BUTTON_ROLE_ACCEPT QT_BUTTON_ROLE_REJECT
  QT_BUTTON_ROLE_DESTRUCTIVE QT_BUTTON_ROLE_ACTION QT_BUTTON_ROLE_HELP
  QT_BUTTON_ROLE_YES QT_BUTTON_ROLE_NO
  QT_BUTTON_ROLE_APPLY QT_BUTTON_ROLE_RESET
  QT_MONDAY QT_TUESDAY QT_WEDNESDAY QT_THURSDAY
  QT_FRIDAY QT_SATURDAY QT_SUNDAY

  ;; QSettings
  qt-settings-create qt-settings-create-file
  qt-settings-set-string! qt-settings-string
  qt-settings-set-int! qt-settings-int
  qt-settings-set-double! qt-settings-double
  qt-settings-set-bool! qt-settings-bool
  qt-settings-set! qt-settings-value
  qt-settings-contains? qt-settings-remove!
  qt-settings-all-keys qt-settings-child-keys qt-settings-child-groups
  qt-settings-begin-group! qt-settings-end-group! qt-settings-group
  qt-settings-sync! qt-settings-clear!
  qt-settings-file-name qt-settings-writable?
  qt-settings-destroy!

  ;; QCompleter
  qt-completer-create qt-completer-set-model-strings!
  qt-completer-set-case-sensitivity! qt-completer-set-completion-mode!
  qt-completer-set-filter-mode! qt-completer-set-max-visible-items!
  qt-completer-completion-count qt-completer-current-completion
  qt-completer-set-completion-prefix!
  qt-on-completer-activated!
  qt-line-edit-set-completer! qt-completer-destroy!

  ;; QToolTip / QWhatsThis
  qt-tooltip-show-text! qt-tooltip-hide-text! qt-tooltip-visible?
  qt-widget-tooltip qt-widget-set-whats-this! qt-widget-whats-this

  ;; Phase 11 constants
  QT_SETTINGS_NATIVE QT_SETTINGS_INI
  QT_COMPLETER_POPUP QT_COMPLETER_INLINE QT_COMPLETER_UNFILTERED_POPUP
  QT_CASE_INSENSITIVE QT_CASE_SENSITIVE
  QT_MATCH_STARTS_WITH QT_MATCH_CONTAINS QT_MATCH_ENDS_WITH

  ;; QStandardItemModel
  qt-standard-model-create qt-standard-model-destroy!
  qt-standard-model-row-count qt-standard-model-column-count
  qt-standard-model-set-row-count! qt-standard-model-set-column-count!
  qt-standard-model-set-item! qt-standard-model-item
  qt-standard-model-insert-row! qt-standard-model-insert-column!
  qt-standard-model-remove-row! qt-standard-model-remove-column!
  qt-standard-model-clear!
  qt-standard-model-set-horizontal-header!
  qt-standard-model-set-vertical-header!

  ;; QStandardItem
  qt-standard-item-create qt-standard-item-text qt-standard-item-set-text!
  qt-standard-item-tooltip qt-standard-item-set-tooltip!
  qt-standard-item-set-editable! qt-standard-item-editable?
  qt-standard-item-set-enabled! qt-standard-item-enabled?
  qt-standard-item-set-selectable! qt-standard-item-selectable?
  qt-standard-item-set-checkable! qt-standard-item-checkable?
  qt-standard-item-set-check-state! qt-standard-item-check-state
  qt-standard-item-set-icon!
  qt-standard-item-append-row! qt-standard-item-row-count
  qt-standard-item-column-count qt-standard-item-child

  ;; QStringListModel
  qt-string-list-model-create qt-string-list-model-destroy!
  qt-string-list-model-set-strings! qt-string-list-model-strings
  qt-string-list-model-row-count

  ;; Views (common)
  qt-view-set-model! qt-view-set-selection-mode!
  qt-view-set-selection-behavior! qt-view-set-alternating-row-colors!
  qt-view-set-sorting-enabled! qt-view-set-edit-triggers!

  ;; QListView
  qt-list-view-create qt-list-view-set-flow!

  ;; QTableView
  qt-table-view-create qt-table-view-set-column-width!
  qt-table-view-set-row-height!
  qt-table-view-hide-column! qt-table-view-show-column!
  qt-table-view-hide-row! qt-table-view-show-row!
  qt-table-view-resize-columns-to-contents!
  qt-table-view-resize-rows-to-contents!

  ;; QTreeView
  qt-tree-view-create qt-tree-view-expand-all! qt-tree-view-collapse-all!
  qt-tree-view-set-indentation! qt-tree-view-indentation
  qt-tree-view-set-root-is-decorated! qt-tree-view-set-header-hidden!
  qt-tree-view-set-column-width!

  ;; QHeaderView (via view)
  qt-view-header-set-stretch-last-section!
  qt-view-header-set-section-resize-mode!
  qt-view-header-hide! qt-view-header-show!
  qt-view-header-set-default-section-size!

  ;; QSortFilterProxyModel
  qt-sort-filter-proxy-create qt-sort-filter-proxy-destroy!
  qt-sort-filter-proxy-set-source-model!
  qt-sort-filter-proxy-set-filter-regex!
  qt-sort-filter-proxy-set-filter-column!
  qt-sort-filter-proxy-set-filter-case-sensitivity!
  qt-sort-filter-proxy-set-filter-role!
  qt-sort-filter-proxy-sort! qt-sort-filter-proxy-set-sort-role!
  qt-sort-filter-proxy-set-dynamic-sort-filter!
  qt-sort-filter-proxy-invalidate-filter!
  qt-sort-filter-proxy-row-count

  ;; View signals + selection
  qt-on-view-clicked! qt-on-view-double-clicked!
  qt-on-view-activated! qt-on-view-selection-changed!
  qt-view-last-clicked-row qt-view-last-clicked-col
  qt-view-selected-rows qt-view-current-row

  ;; Phase 12 constants
  QT_DISPLAY_ROLE QT_EDIT_ROLE QT_TOOLTIP_ROLE
  QT_CHECK_STATE_ROLE QT_USER_ROLE
  QT_SELECT_NONE QT_SELECT_SINGLE QT_SELECT_MULTI
  QT_SELECT_EXTENDED QT_SELECT_CONTIGUOUS
  QT_SELECT_ITEMS QT_SELECT_ROWS QT_SELECT_COLUMNS
  QT_SORT_ASCENDING QT_SORT_DESCENDING
  QT_UNCHECKED QT_PARTIALLY_CHECKED QT_CHECKED
  QT_HEADER_INTERACTIVE QT_HEADER_FIXED
  QT_HEADER_STRETCH QT_HEADER_RESIZE_TO_CONTENTS
  QT_EDIT_NONE QT_EDIT_DOUBLE_CLICKED QT_EDIT_ALL_INPUT

  ;; Phase 13: QValidator
  qt-int-validator-create qt-double-validator-create
  qt-regex-validator-create qt-validator-destroy!
  qt-validator-validate qt-line-edit-set-validator!
  qt-line-edit-acceptable-input?

  ;; Phase 13: QPlainTextEdit
  qt-plain-text-edit-create qt-plain-text-edit-set-text!
  qt-plain-text-edit-text qt-plain-text-edit-append!
  qt-plain-text-edit-clear! qt-plain-text-edit-set-read-only!
  qt-plain-text-edit-read-only? qt-plain-text-edit-set-placeholder!
  qt-plain-text-edit-line-count qt-plain-text-edit-set-max-block-count!
  qt-plain-text-edit-cursor-line qt-plain-text-edit-cursor-column
  qt-plain-text-edit-set-line-wrap!
  qt-on-plain-text-edit-text-changed!

  ;; Phase 17: QPlainTextEdit Editor Extensions
  QT_CURSOR_NO_MOVE QT_CURSOR_START QT_CURSOR_UP QT_CURSOR_START_OF_LINE
  QT_CURSOR_START_OF_BLOCK QT_CURSOR_PREVIOUS_CHAR QT_CURSOR_PREVIOUS_BLOCK
  QT_CURSOR_END_OF_LINE QT_CURSOR_END_OF_BLOCK QT_CURSOR_NEXT_CHAR
  QT_CURSOR_NEXT_BLOCK QT_CURSOR_END QT_CURSOR_DOWN QT_CURSOR_LEFT
  QT_CURSOR_WORD_LEFT QT_CURSOR_NEXT_WORD QT_CURSOR_RIGHT
  QT_CURSOR_WORD_RIGHT QT_CURSOR_PREVIOUS_WORD
  QT_MOVE_ANCHOR QT_KEEP_ANCHOR
  QT_FIND_BACKWARD QT_FIND_CASE_SENSITIVE QT_FIND_WHOLE_WORDS
  qt-plain-text-edit-cursor-position qt-plain-text-edit-set-cursor-position!
  qt-plain-text-edit-move-cursor!
  qt-plain-text-edit-select-all! qt-plain-text-edit-selected-text
  qt-plain-text-edit-selection-start qt-plain-text-edit-selection-end
  qt-plain-text-edit-set-selection! qt-plain-text-edit-has-selection?
  qt-plain-text-edit-insert-text! qt-plain-text-edit-remove-selected-text!
  qt-plain-text-edit-undo! qt-plain-text-edit-redo!
  qt-plain-text-edit-can-undo?
  qt-plain-text-edit-cut! qt-plain-text-edit-copy! qt-plain-text-edit-paste!
  qt-plain-text-edit-text-length qt-plain-text-edit-text-range
  qt-plain-text-edit-line-from-position qt-plain-text-edit-line-end-position
  qt-plain-text-edit-find-text
  qt-plain-text-edit-ensure-cursor-visible!
  qt-plain-text-edit-center-cursor!
  qt-text-document-create qt-text-document-destroy!
  qt-plain-text-edit-document qt-plain-text-edit-set-document!
  qt-text-document-modified? qt-text-document-set-modified!

  ;; Phase 13: QToolButton
  qt-tool-button-create qt-tool-button-set-text! qt-tool-button-text
  qt-tool-button-set-icon! qt-tool-button-set-menu!
  qt-tool-button-set-popup-mode! qt-tool-button-set-auto-raise!
  qt-tool-button-set-arrow-type! qt-tool-button-set-tool-button-style!
  qt-on-tool-button-clicked!

  ;; Phase 13: Layout spacers
  qt-layout-add-spacing!

  ;; Phase 13: QSizePolicy
  qt-widget-set-size-policy! qt-layout-set-stretch-factor!

  ;; Phase 13 constants
  QT_VALIDATOR_INVALID QT_VALIDATOR_INTERMEDIATE QT_VALIDATOR_ACCEPTABLE
  QT_PLAIN_NO_WRAP QT_PLAIN_WIDGET_WRAP
  QT_DELAYED_POPUP QT_MENU_BUTTON_POPUP QT_INSTANT_POPUP
  QT_NO_ARROW QT_UP_ARROW QT_DOWN_ARROW QT_LEFT_ARROW QT_RIGHT_ARROW
  QT_TOOL_BUTTON_ICON_ONLY QT_TOOL_BUTTON_TEXT_ONLY
  QT_TOOL_BUTTON_TEXT_BESIDE_ICON QT_TOOL_BUTTON_TEXT_UNDER_ICON
  QT_SIZE_FIXED QT_SIZE_MINIMUM QT_SIZE_MINIMUM_EXPANDING
  QT_SIZE_MAXIMUM QT_SIZE_PREFERRED QT_SIZE_EXPANDING QT_SIZE_IGNORED

  ;; Phase 14: QGraphicsScene
  qt-graphics-scene-create qt-graphics-scene-add-rect!
  qt-graphics-scene-add-ellipse! qt-graphics-scene-add-line!
  qt-graphics-scene-add-text! qt-graphics-scene-add-pixmap!
  qt-graphics-scene-remove-item! qt-graphics-scene-clear!
  qt-graphics-scene-items-count qt-graphics-scene-set-background!
  qt-graphics-scene-destroy!

  ;; Phase 14: QGraphicsView
  qt-graphics-view-create qt-graphics-view-set-render-hint!
  qt-graphics-view-set-drag-mode! qt-graphics-view-fit-in-view!
  qt-graphics-view-scale! qt-graphics-view-center-on!

  ;; Phase 14: QGraphicsItem
  qt-graphics-item-set-pos! qt-graphics-item-x qt-graphics-item-y
  qt-graphics-item-set-pen! qt-graphics-item-set-brush!
  qt-graphics-item-set-flags! qt-graphics-item-set-tooltip!
  qt-graphics-item-set-zvalue! qt-graphics-item-zvalue
  qt-graphics-item-set-rotation! qt-graphics-item-set-scale!
  qt-graphics-item-set-visible!

  ;; Phase 14: PaintWidget
  qt-paint-widget-create qt-paint-widget-on-paint!
  qt-paint-widget-painter qt-paint-widget-update!
  qt-paint-widget-width qt-paint-widget-height

  ;; Phase 14 constants
  QT_ITEM_MOVABLE QT_ITEM_SELECTABLE QT_ITEM_FOCUSABLE
  QT_DRAG_NONE QT_DRAG_SCROLL QT_DRAG_RUBBER_BAND
  QT_RENDER_ANTIALIASING QT_RENDER_SMOOTH_PIXMAP QT_RENDER_TEXT_ANTIALIASING

  ;; Phase 15: QProcess
  qt-process-create qt-process-start! qt-process-write!
  qt-process-close-write! qt-process-read-stdout qt-process-read-stderr
  qt-process-wait-for-finished qt-process-exit-code qt-process-state
  qt-process-kill! qt-process-terminate!
  qt-process-on-finished! qt-process-on-ready-read!
  qt-process-destroy!

  ;; Phase 15: QWizard / QWizardPage
  qt-wizard-create qt-wizard-add-page! qt-wizard-set-start-id!
  qt-wizard-current-id qt-wizard-set-title! qt-wizard-exec!
  qt-wizard-page-create qt-wizard-page-set-title!
  qt-wizard-page-set-subtitle! qt-wizard-page-set-layout!
  qt-wizard-on-current-changed!

  ;; Phase 15: QMdiArea / QMdiSubWindow
  qt-mdi-area-create qt-mdi-area-add-sub-window!
  qt-mdi-area-remove-sub-window! qt-mdi-area-active-sub-window
  qt-mdi-area-sub-window-count qt-mdi-area-cascade!
  qt-mdi-area-tile! qt-mdi-area-set-view-mode!
  qt-mdi-sub-window-set-title! qt-mdi-area-on-sub-window-activated!

  ;; Phase 16: QDial
  qt-dial-create qt-dial-set-value! qt-dial-value qt-dial-set-range!
  qt-dial-set-notches-visible! qt-dial-set-wrapping! qt-dial-on-value-changed!

  ;; Phase 16: QLCDNumber
  qt-lcd-create qt-lcd-display-int! qt-lcd-display-double!
  qt-lcd-display-string! qt-lcd-set-mode! qt-lcd-set-segment-style!

  ;; Phase 16: QToolBox
  qt-tool-box-create qt-tool-box-add-item! qt-tool-box-set-current-index!
  qt-tool-box-current-index qt-tool-box-count qt-tool-box-set-item-text!
  qt-tool-box-on-current-changed!

  ;; Phase 16: QUndoStack
  qt-undo-stack-create qt-undo-stack-push!
  qt-undo-stack-undo! qt-undo-stack-redo!
  qt-undo-stack-can-undo? qt-undo-stack-can-redo?
  qt-undo-stack-undo-text qt-undo-stack-redo-text
  qt-undo-stack-clear! qt-undo-stack-create-undo-action
  qt-undo-stack-create-redo-action qt-undo-stack-destroy!

  ;; Phase 16: QFileSystemModel
  qt-file-system-model-create qt-file-system-model-set-root-path!
  qt-file-system-model-set-filter! qt-file-system-model-set-name-filters!
  qt-file-system-model-file-path qt-tree-view-set-file-system-root!
  qt-file-system-model-destroy!

  ;; Phase 15 constants
  QT_PROCESS_NOT_RUNNING QT_PROCESS_STARTING QT_PROCESS_RUNNING
  QT_MDI_SUBWINDOW QT_MDI_TABBED

  ;; Phase 16 constants
  QT_LCD_DEC QT_LCD_HEX QT_LCD_OCT QT_LCD_BIN
  QT_LCD_OUTLINE QT_LCD_FILLED QT_LCD_FLAT
  QT_DIR_DIRS QT_DIR_FILES QT_DIR_HIDDEN QT_DIR_NO_DOT_AND_DOT_DOT

  ;; Callback management
  unregister-qt-handler!
  qt-disconnect-all!

  ;; Resource-safety macros
  with-painter with-font with-color with-pixmap
  with-icon with-settings)

(import :gerbil-qt/libqt
        :std/srfi/13
        :std/sugar)

;;; ---- Lifecycle ----

(def (qt-app-create)
  (qt_application_create))

(def (qt-app-exec! app)
  (qt_application_exec app))

(def (qt-app-quit! app)
  (qt_application_quit app))

(def (qt-app-process-events! app)
  (qt_application_process_events app))

(def (qt-app-destroy! app)
  (let ((ids (hash-ref *qt-widget-handlers* app '())))
    (for-each unregister-qt-handler! ids)
    (hash-remove! *qt-widget-handlers* app))
  (qt_application_destroy app))

(defrule (with-qt-app app body ...)
  (let ((app (qt-app-create)))
    (try body ...
      (finally (qt-app-destroy! app)))))

;;; ---- Signal handler tracking ----
;; Maps widget/object pointer → list of callback IDs for cleanup by qt-disconnect-all!
(def *qt-widget-handlers* (make-hash-table))

(def (track-handler! obj id)
  (let ((ids (hash-ref *qt-widget-handlers* obj '())))
    (hash-put! *qt-widget-handlers* obj (cons id ids)))
  id)

(def (untrack-handler! id)
  (hash-for-each
   (lambda (obj ids)
     (let ((new-ids (filter (lambda (x) (not (= x id))) ids)))
       (if (null? new-ids)
         (hash-remove! *qt-widget-handlers* obj)
         (hash-put! *qt-widget-handlers* obj new-ids))))
   *qt-widget-handlers*))

;;; ---- Widget ----

(def (qt-widget-create parent: (parent #f))
  (qt_widget_create parent))

(def (qt-widget-show! w)
  (qt_widget_show w))

(def (qt-widget-hide! w)
  (qt_widget_hide w))

(def (qt-widget-close! w)
  (qt_widget_close w))

(def (qt-widget-set-enabled! w enabled)
  (qt_widget_set_enabled w (if enabled 1 0)))

(def (qt-widget-enabled? w)
  (not (= (qt_widget_is_enabled w) 0)))

(def (qt-widget-set-visible! w visible)
  (qt_widget_set_visible w (if visible 1 0)))

(def (qt-widget-visible? w)
  (not (= (qt_widget_is_visible w) 0)))

(def (qt-widget-set-fixed-size! w width height)
  (qt_widget_set_fixed_size w width height))

(def (qt-widget-set-minimum-size! w width height)
  (qt_widget_set_minimum_size w width height))

(def (qt-widget-set-maximum-size! w width height)
  (qt_widget_set_maximum_size w width height))

(def (qt-widget-set-minimum-width! w width)
  (qt_widget_set_minimum_width w width))

(def (qt-widget-set-minimum-height! w height)
  (qt_widget_set_minimum_height w height))

(def (qt-widget-set-maximum-width! w width)
  (qt_widget_set_maximum_width w width))

(def (qt-widget-set-maximum-height! w height)
  (qt_widget_set_maximum_height w height))

(def (qt-widget-set-cursor! w shape)
  (qt_widget_set_cursor w shape))

(def (qt-widget-unset-cursor! w)
  (qt_widget_unset_cursor w))

(def (qt-widget-resize! w width height)
  (qt_widget_resize w width height))

(def (qt-widget-set-style-sheet! w css)
  (qt_widget_set_style_sheet w css))

(def (qt-widget-set-tooltip! w text)
  (qt_widget_set_tooltip w text))

(def (qt-widget-set-font-size! w size)
  (qt_widget_set_font_size w size))

(def (qt-widget-destroy! w)
  ;; Clean up Scheme handler tracking for this widget
  (let ((ids (hash-ref *qt-widget-handlers* w '())))
    (for-each (lambda (id)
                (unregister-qt-handler! id)
                ;; Destroy drop filter C++ object if applicable
                (let ((df (hash-ref *qt-drop-filter-ptrs* id #f)))
                  (when df
                    (qt_drop_filter_destroy df)
                    (hash-remove! *qt-drop-filter-ptrs* id))))
              ids)
    (hash-remove! *qt-widget-handlers* w))
  ;; Clean up secondary tracking tables if applicable
  (hash-remove! *qt-undo-stack-handlers* w)
  (hash-remove! *qt-process-handlers* w)
  (qt_widget_destroy w))

;;; ---- Main Window ----

(def (qt-main-window-create parent: (parent #f))
  (qt_main_window_create parent))

(def (qt-main-window-set-title! w title)
  (qt_main_window_set_title w title))

(def (qt-main-window-set-central-widget! w child)
  (qt_main_window_set_central_widget w child))

;;; ---- Layouts ----

(def (qt-vbox-layout-create parent)
  (qt_vbox_layout_create parent))

(def (qt-hbox-layout-create parent)
  (qt_hbox_layout_create parent))

(def (qt-layout-add-widget! layout widget)
  (qt_layout_add_widget layout widget))

(def (qt-layout-add-stretch! layout stretch: (stretch 1))
  (qt_layout_add_stretch layout stretch))

(def (qt-layout-set-spacing! layout spacing)
  (qt_layout_set_spacing layout spacing))

(def (qt-layout-set-margins! layout left top right bottom)
  (qt_layout_set_margins layout left top right bottom))

;;; ---- Labels ----

(def (qt-label-create text parent: (parent #f))
  (qt_label_create text parent))

(def (qt-label-set-text! l text)
  (qt_label_set_text l text))

(def (qt-label-text l)
  (qt_label_text l))

(def (qt-label-set-alignment! l alignment)
  (qt_label_set_alignment l alignment))

(def (qt-label-set-word-wrap! l wrap)
  (qt_label_set_word_wrap l (if wrap 1 0)))

;;; ---- Push Button ----

(def (qt-push-button-create text parent: (parent #f))
  (qt_push_button_create text parent))

(def (qt-push-button-set-text! b text)
  (qt_push_button_set_text b text))

(def (qt-push-button-text b)
  (qt_push_button_text b))

(def (qt-on-clicked! button handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_push_button_on_clicked button id)
    (track-handler! button id)))

;;; ---- Line Edit ----

(def (qt-line-edit-create parent: (parent #f))
  (qt_line_edit_create parent))

(def (qt-line-edit-text e)
  (qt_line_edit_text e))

(def (qt-line-edit-set-text! e text)
  (qt_line_edit_set_text e text))

(def (qt-line-edit-set-placeholder! e text)
  (qt_line_edit_set_placeholder e text))

(def (qt-line-edit-set-read-only! e read-only)
  (qt_line_edit_set_read_only e (if read-only 1 0)))

(def (qt-line-edit-set-echo-mode! e mode)
  (qt_line_edit_set_echo_mode e mode))

(def (qt-on-text-changed! line-edit handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_line_edit_on_text_changed line-edit id)
    (track-handler! line-edit id)))

(def (qt-on-return-pressed! line-edit handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_line_edit_on_return_pressed line-edit id)
    (track-handler! line-edit id)))

;;; ---- Check Box ----

(def (qt-check-box-create text parent: (parent #f))
  (qt_check_box_create text parent))

(def (qt-check-box-checked? c)
  (not (= (qt_check_box_is_checked c) 0)))

(def (qt-check-box-set-checked! c checked)
  (qt_check_box_set_checked c (if checked 1 0)))

(def (qt-check-box-set-text! c text)
  (qt_check_box_set_text c text))

(def (qt-on-toggled! check-box handler)
  (let ((id (register-qt-bool-handler! handler)))
    (raw_qt_check_box_on_toggled check-box id)
    (track-handler! check-box id)))

;;; ---- Combo Box ----

(def (qt-combo-box-create parent: (parent #f))
  (qt_combo_box_create parent))

(def (qt-combo-box-add-item! c text)
  (qt_combo_box_add_item c text))

(def (qt-combo-box-set-current-index! c index)
  (qt_combo_box_set_current_index c index))

(def (qt-combo-box-current-index c)
  (qt_combo_box_current_index c))

(def (qt-combo-box-current-text c)
  (qt_combo_box_current_text c))

(def (qt-combo-box-count c)
  (qt_combo_box_count c))

(def (qt-combo-box-clear! c)
  (qt_combo_box_clear c))

(def (qt-on-index-changed! combo-box handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_combo_box_on_current_index_changed combo-box id)
    (track-handler! combo-box id)))

;;; ---- Text Edit ----

(def (qt-text-edit-create parent: (parent #f))
  (qt_text_edit_create parent))

(def (qt-text-edit-text e)
  (qt_text_edit_text e))

(def (qt-text-edit-set-text! e text)
  (qt_text_edit_set_text e text))

(def (qt-text-edit-set-placeholder! e text)
  (qt_text_edit_set_placeholder e text))

(def (qt-text-edit-set-read-only! e read-only)
  (qt_text_edit_set_read_only e (if read-only 1 0)))

(def (qt-text-edit-append! e text)
  (qt_text_edit_append e text))

(def (qt-text-edit-clear! e)
  (qt_text_edit_clear e))

(def (qt-text-edit-scroll-to-bottom! e)
  (qt_text_edit_scroll_to_bottom e))

(def (qt-text-edit-html e)
  (qt_text_edit_html e))

(def (qt-on-text-edit-changed! text-edit handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_text_edit_on_text_changed text-edit id)
    (track-handler! text-edit id)))

;;; ---- Spin Box ----

(def (qt-spin-box-create parent: (parent #f))
  (qt_spin_box_create parent))

(def (qt-spin-box-value s)
  (qt_spin_box_value s))

(def (qt-spin-box-set-value! s value)
  (qt_spin_box_set_value s value))

(def (qt-spin-box-set-range! s minimum maximum)
  (qt_spin_box_set_range s minimum maximum))

(def (qt-spin-box-set-single-step! s step)
  (qt_spin_box_set_single_step s step))

(def (qt-spin-box-set-prefix! s prefix)
  (qt_spin_box_set_prefix s prefix))

(def (qt-spin-box-set-suffix! s suffix)
  (qt_spin_box_set_suffix s suffix))

(def (qt-on-value-changed! spin-box handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_spin_box_on_value_changed spin-box id)
    (track-handler! spin-box id)))

;;; ---- Dialog ----

(def (qt-dialog-create parent: (parent #f))
  (qt_dialog_create parent))

(def (qt-dialog-exec! d)
  (qt_dialog_exec d))

(def (qt-dialog-accept! d)
  (qt_dialog_accept d))

(def (qt-dialog-reject! d)
  (qt_dialog_reject d))

(def (qt-dialog-set-title! d title)
  (qt_dialog_set_title d title))

;;; ---- Message Box ----

(def (qt-message-box-information parent title text)
  (qt_message_box_information parent title text))

(def (qt-message-box-warning parent title text)
  (qt_message_box_warning parent title text))

(def (qt-message-box-question parent title text)
  (qt_message_box_question parent title text))

(def (qt-message-box-critical parent title text)
  (qt_message_box_critical parent title text))

;;; ---- File Dialog ----

(def (qt-file-dialog-open-file parent caption: (caption "Open File")
                               dir: (dir "") filter: (filter ""))
  (qt_file_dialog_open_file parent caption dir filter))

(def (qt-file-dialog-save-file parent caption: (caption "Save File")
                               dir: (dir "") filter: (filter ""))
  (qt_file_dialog_save_file parent caption dir filter))

(def (qt-file-dialog-open-directory parent caption: (caption "Open Directory")
                                    dir: (dir ""))
  (qt_file_dialog_open_directory parent caption dir))

;;; ---- Menu Bar ----

(def (qt-main-window-menu-bar win)
  (qt_main_window_menu_bar win))

;;; ---- Menu ----

(def (qt-menu-bar-add-menu bar title)
  (qt_menu_bar_add_menu bar title))

(def (qt-menu-add-menu menu title)
  (qt_menu_add_menu menu title))

(def (qt-menu-add-action! menu action)
  (qt_menu_add_action menu action))

(def (qt-menu-add-separator! menu)
  (qt_menu_add_separator menu))

;;; ---- Action ----

(def (qt-action-create text parent: (parent #f))
  (qt_action_create text parent))

(def (qt-action-text a)
  (qt_action_text a))

(def (qt-action-set-text! a text)
  (qt_action_set_text a text))

(def (qt-action-set-shortcut! a shortcut)
  (qt_action_set_shortcut a shortcut))

(def (qt-action-set-enabled! a enabled)
  (qt_action_set_enabled a (if enabled 1 0)))

(def (qt-action-enabled? a)
  (not (= (qt_action_is_enabled a) 0)))

(def (qt-action-set-checkable! a checkable)
  (qt_action_set_checkable a (if checkable 1 0)))

(def (qt-action-checkable? a)
  (not (= (qt_action_is_checkable a) 0)))

(def (qt-action-set-checked! a checked)
  (qt_action_set_checked a (if checked 1 0)))

(def (qt-action-checked? a)
  (not (= (qt_action_is_checked a) 0)))

(def (qt-action-set-tooltip! a text)
  (qt_action_set_tooltip a text))

(def (qt-action-set-status-tip! a text)
  (qt_action_set_status_tip a text))

(def (qt-on-triggered! action handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_action_on_triggered action id)
    (track-handler! action id)))

(def (qt-on-action-toggled! action handler)
  (let ((id (register-qt-bool-handler! handler)))
    (raw_qt_action_on_toggled action id)
    (track-handler! action id)))

;;; ---- Toolbar ----

(def (qt-toolbar-create title parent: (parent #f))
  (qt_toolbar_create title parent))

(def (qt-main-window-add-toolbar! win toolbar)
  (qt_main_window_add_toolbar win toolbar))

(def (qt-toolbar-add-action! toolbar action)
  (qt_toolbar_add_action toolbar action))

(def (qt-toolbar-add-separator! toolbar)
  (qt_toolbar_add_separator toolbar))

(def (qt-toolbar-add-widget! toolbar widget)
  (qt_toolbar_add_widget toolbar widget))

(def (qt-toolbar-set-movable! toolbar movable)
  (qt_toolbar_set_movable toolbar (if movable 1 0)))

(def (qt-toolbar-set-icon-size! toolbar width height)
  (qt_toolbar_set_icon_size toolbar width height))

;;; ---- Status Bar ----

(def (qt-main-window-set-status-bar-text! win text)
  (qt_main_window_set_status_bar_text win text))

;;; ---- List Widget ----

(def (qt-list-widget-create parent: (parent #f))
  (qt_list_widget_create parent))

(def (qt-list-widget-add-item! l text)
  (qt_list_widget_add_item l text))

(def (qt-list-widget-insert-item! l row text)
  (qt_list_widget_insert_item l row text))

(def (qt-list-widget-remove-item! l row)
  (qt_list_widget_remove_item l row))

(def (qt-list-widget-current-row l)
  (qt_list_widget_current_row l))

(def (qt-list-widget-set-current-row! l row)
  (qt_list_widget_set_current_row l row))

(def (qt-list-widget-item-text l row)
  (qt_list_widget_item_text l row))

(def (qt-list-widget-count l)
  (qt_list_widget_count l))

(def (qt-list-widget-clear! l)
  (qt_list_widget_clear l))

(def (qt-list-widget-set-item-data! l row data)
  (qt_list_widget_set_item_data l row data))

(def (qt-list-widget-item-data l row)
  (qt_list_widget_item_data l row))

(def (qt-on-current-row-changed! list-widget handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_list_widget_on_current_row_changed list-widget id)
    (track-handler! list-widget id)))

(def (qt-on-item-double-clicked! list-widget handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_list_widget_on_item_double_clicked list-widget id)
    (track-handler! list-widget id)))

;;; ---- Table Widget ----

(def (qt-table-widget-create rows cols parent: (parent #f))
  (qt_table_widget_create rows cols parent))

(def (qt-table-widget-set-item! t row col text)
  (qt_table_widget_set_item t row col text))

(def (qt-table-widget-item-text t row col)
  (qt_table_widget_item_text t row col))

(def (qt-table-widget-set-horizontal-header! t col text)
  (qt_table_widget_set_horizontal_header_item t col text))

(def (qt-table-widget-set-vertical-header! t row text)
  (qt_table_widget_set_vertical_header_item t row text))

(def (qt-table-widget-set-row-count! t count)
  (qt_table_widget_set_row_count t count))

(def (qt-table-widget-set-column-count! t count)
  (qt_table_widget_set_column_count t count))

(def (qt-table-widget-row-count t)
  (qt_table_widget_row_count t))

(def (qt-table-widget-column-count t)
  (qt_table_widget_column_count t))

(def (qt-table-widget-current-row t)
  (qt_table_widget_current_row t))

(def (qt-table-widget-current-column t)
  (qt_table_widget_current_column t))

(def (qt-table-widget-clear! t)
  (qt_table_widget_clear t))

(def (qt-on-cell-clicked! table handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_table_widget_on_cell_clicked table id)
    (track-handler! table id)))

;;; ---- Tab Widget ----

(def (qt-tab-widget-create parent: (parent #f))
  (qt_tab_widget_create parent))

(def (qt-tab-widget-add-tab! t page label)
  (qt_tab_widget_add_tab t page label))

(def (qt-tab-widget-set-current-index! t index)
  (qt_tab_widget_set_current_index t index))

(def (qt-tab-widget-current-index t)
  (qt_tab_widget_current_index t))

(def (qt-tab-widget-count t)
  (qt_tab_widget_count t))

(def (qt-tab-widget-set-tab-text! t index text)
  (qt_tab_widget_set_tab_text t index text))

(def (qt-on-tab-changed! tab-widget handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_tab_widget_on_current_changed tab-widget id)
    (track-handler! tab-widget id)))

;;; ---- Progress Bar ----

(def (qt-progress-bar-create parent: (parent #f))
  (qt_progress_bar_create parent))

(def (qt-progress-bar-set-value! p value)
  (qt_progress_bar_set_value p value))

(def (qt-progress-bar-value p)
  (qt_progress_bar_value p))

(def (qt-progress-bar-set-range! p minimum maximum)
  (qt_progress_bar_set_range p minimum maximum))

(def (qt-progress-bar-set-format! p format)
  (qt_progress_bar_set_format p format))

;;; ---- Slider ----

(def (qt-slider-create orientation parent: (parent #f))
  (qt_slider_create orientation parent))

(def (qt-slider-set-value! s value)
  (qt_slider_set_value s value))

(def (qt-slider-value s)
  (qt_slider_value s))

(def (qt-slider-set-range! s minimum maximum)
  (qt_slider_set_range s minimum maximum))

(def (qt-slider-set-single-step! s step)
  (qt_slider_set_single_step s step))

(def (qt-slider-set-tick-interval! s interval)
  (qt_slider_set_tick_interval s interval))

(def (qt-slider-set-tick-position! s position)
  (qt_slider_set_tick_position s position))

(def (qt-on-slider-value-changed! slider handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_slider_on_value_changed slider id)
    (track-handler! slider id)))

;;; ---- Grid Layout ----

(def (qt-grid-layout-create parent)
  (qt_grid_layout_create parent))

(def (qt-grid-layout-add-widget! layout widget row col
                                  row-span: (row-span 1)
                                  col-span: (col-span 1))
  (qt_grid_layout_add_widget layout widget row col row-span col-span))

(def (qt-grid-layout-set-row-stretch! layout row stretch)
  (qt_grid_layout_set_row_stretch layout row stretch))

(def (qt-grid-layout-set-column-stretch! layout col stretch)
  (qt_grid_layout_set_column_stretch layout col stretch))

(def (qt-grid-layout-set-row-minimum-height! layout row height)
  (qt_grid_layout_set_row_minimum_height layout row height))

(def (qt-grid-layout-set-column-minimum-width! layout col width)
  (qt_grid_layout_set_column_minimum_width layout col width))

;;; ---- Timer ----

(def (qt-timer-create)
  (qt_timer_create))

(def (qt-timer-start! timer msec)
  (qt_timer_start timer msec))

(def (qt-timer-stop! timer)
  (qt_timer_stop timer))

(def (qt-timer-set-single-shot! timer single-shot)
  (qt_timer_set_single_shot timer (if single-shot 1 0)))

(def (qt-timer-active? timer)
  (not (= (qt_timer_is_active timer) 0)))

(def (qt-timer-interval timer)
  (qt_timer_interval timer))

(def (qt-timer-set-interval! timer msec)
  (qt_timer_set_interval timer msec))

(def (qt-on-timeout! timer handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_timer_on_timeout timer id)
    (track-handler! timer id)))

(def (qt-timer-single-shot! msec handler)
  ;; Wrap handler to auto-unregister after firing (single-shot = one use).
  (let* ((id #f)
         (wrapped (lambda ()
                    (unregister-qt-handler! id)
                    (handler))))
    (set! id (register-qt-void-handler! wrapped))
    (raw_qt_timer_single_shot msec id)
    id))

(def (qt-timer-destroy! timer)
  (let ((ids (hash-ref *qt-widget-handlers* timer '())))
    (for-each unregister-qt-handler! ids)
    (hash-remove! *qt-widget-handlers* timer))
  (qt_timer_destroy timer))

;;; ---- Clipboard ----

(def (qt-clipboard-text app)
  (qt_clipboard_text app))

(def (qt-clipboard-set-text! app text)
  (qt_clipboard_set_text app text))

(def (qt-on-clipboard-changed! app handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_clipboard_on_changed app id)
    (track-handler! app id)))

;;; ---- Tree Widget ----

(def (qt-tree-widget-create parent: (parent #f))
  (qt_tree_widget_create parent))

(def (qt-tree-widget-set-column-count! t count)
  (qt_tree_widget_set_column_count t count))

(def (qt-tree-widget-column-count t)
  (qt_tree_widget_column_count t))

(def (qt-tree-widget-set-header-label! t label)
  (qt_tree_widget_set_header_label t label))

(def (qt-tree-widget-set-header-item-text! t col text)
  (qt_tree_widget_set_header_item_text t col text))

(def (qt-tree-widget-set-header-labels! t labels)
  (qt_tree_widget_set_column_count t (length labels))
  (let loop ((i 0) (rest labels))
    (when (pair? rest)
      (qt_tree_widget_set_header_item_text t i (car rest))
      (loop (+ i 1) (cdr rest)))))

(def (qt-tree-widget-add-top-level-item! t item)
  (qt_tree_widget_add_top_level_item t item))

(def (qt-tree-widget-top-level-item-count t)
  (qt_tree_widget_top_level_item_count t))

(def (qt-tree-widget-top-level-item t index)
  (qt_tree_widget_top_level_item t index))

(def (qt-tree-widget-current-item t)
  (qt_tree_widget_current_item t))

(def (qt-tree-widget-set-current-item! t item)
  (qt_tree_widget_set_current_item t item))

(def (qt-tree-widget-expand-item! t item)
  (qt_tree_widget_expand_item t item))

(def (qt-tree-widget-collapse-item! t item)
  (qt_tree_widget_collapse_item t item))

(def (qt-tree-widget-expand-all! t)
  (qt_tree_widget_expand_all t))

(def (qt-tree-widget-collapse-all! t)
  (qt_tree_widget_collapse_all t))

(def (qt-tree-widget-clear! t)
  (qt_tree_widget_clear t))

(def (qt-on-current-item-changed! tree handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_tree_widget_on_current_item_changed tree id)
    (track-handler! tree id)))

(def (qt-on-tree-item-double-clicked! tree handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_tree_widget_on_item_double_clicked tree id)
    (track-handler! tree id)))

(def (qt-on-item-expanded! tree handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_tree_widget_on_item_expanded tree id)
    (track-handler! tree id)))

(def (qt-on-item-collapsed! tree handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_tree_widget_on_item_collapsed tree id)
    (track-handler! tree id)))

;;; ---- Tree Widget Item ----

(def (qt-tree-item-create text)
  (qt_tree_item_create text))

(def (qt-tree-item-set-text! item text column: (column 0))
  (qt_tree_item_set_text item column text))

(def (qt-tree-item-text item column: (column 0))
  (qt_tree_item_text item column))

(def (qt-tree-item-add-child! parent child)
  (qt_tree_item_add_child parent child))

(def (qt-tree-item-child-count item)
  (qt_tree_item_child_count item))

(def (qt-tree-item-child item index)
  (qt_tree_item_child item index))

(def (qt-tree-item-parent item)
  (qt_tree_item_parent item))

(def (qt-tree-item-set-expanded! item expanded)
  (qt_tree_item_set_expanded item (if expanded 1 0)))

(def (qt-tree-item-expanded? item)
  (not (= (qt_tree_item_is_expanded item) 0)))

;;; ---- Event Loop Integration ----

(def (qt-start-timer! msec handler)
  (let ((timer (qt-timer-create)))
    (qt-on-timeout! timer handler)
    (qt-timer-start! timer msec)
    timer))

(def (qt-run-with-timer! app msec handler)
  (let ((timer (qt-start-timer! msec handler)))
    (try (qt-app-exec! app)
      (finally
        (qt-timer-stop! timer)
        (qt-timer-destroy! timer)))))

;;; ---- App-wide Style Sheet ----

(def (qt-app-set-style-sheet! app css)
  (qt_application_set_style_sheet app css))

;;; ---- Window State Management ----

(def (qt-widget-show-minimized! w)
  (qt_widget_show_minimized w))

(def (qt-widget-show-maximized! w)
  (qt_widget_show_maximized w))

(def (qt-widget-show-fullscreen! w)
  (qt_widget_show_fullscreen w))

(def (qt-widget-show-normal! w)
  (qt_widget_show_normal w))

(def (qt-widget-window-state w)
  (qt_widget_window_state w))

(def (qt-widget-move! w x y)
  (qt_widget_move w x y))

(def (qt-widget-x w)
  (qt_widget_x w))

(def (qt-widget-y w)
  (qt_widget_y w))

(def (qt-widget-width w)
  (qt_widget_width w))

(def (qt-widget-height w)
  (qt_widget_height w))

;;; ---- Scroll Area ----

(def (qt-scroll-area-create parent: (parent #f))
  (qt_scroll_area_create parent))

(def (qt-scroll-area-set-widget! s w)
  (qt_scroll_area_set_widget s w))

(def (qt-scroll-area-set-widget-resizable! s resizable)
  (qt_scroll_area_set_widget_resizable s (if resizable 1 0)))

(def (qt-scroll-area-set-horizontal-scrollbar-policy! s policy)
  (qt_scroll_area_set_horizontal_scrollbar_policy s policy))

(def (qt-scroll-area-set-vertical-scrollbar-policy! s policy)
  (qt_scroll_area_set_vertical_scrollbar_policy s policy))

;;; ---- Splitter ----

(def (qt-splitter-create orientation parent: (parent #f))
  (qt_splitter_create orientation parent))

(def (qt-splitter-add-widget! s w)
  (qt_splitter_add_widget s w))

(def (qt-splitter-count s)
  (qt_splitter_count s))

(def (qt-splitter-set-sizes! s sizes)
  (case (length sizes)
    ((2) (qt_splitter_set_sizes_2 s (car sizes) (cadr sizes)))
    ((3) (qt_splitter_set_sizes_3 s (car sizes) (cadr sizes) (caddr sizes)))
    (else (error "qt-splitter-set-sizes!: expected 2 or 3 sizes" sizes))))

(def (qt-splitter-size-at s index)
  (qt_splitter_size_at s index))

(def (qt-splitter-set-stretch-factor! s index stretch)
  (qt_splitter_set_stretch_factor s index stretch))

(def (qt-splitter-set-handle-width! s width)
  (qt_splitter_set_handle_width s width))

(def (qt-splitter-set-collapsible! s index collapsible)
  (qt_splitter_set_collapsible s index (if collapsible 1 0)))

(def (qt-splitter-collapsible? s index)
  (not (= (qt_splitter_is_collapsible s index) 0)))

;;; ---- Keyboard Events ----

(def (qt-on-key-press! widget handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_widget_install_key_handler widget id)
    (track-handler! widget id)))

(def (qt-on-key-press-consuming! widget handler)
  "Install key handler that consumes key events (widget doesn't process them)."
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_widget_install_key_handler_consuming widget id)
    (track-handler! widget id)))

(def (qt-last-key-code)
  (qt_last_key_code))

(def (qt-last-key-modifiers)
  (qt_last_key_modifiers))

(def (qt-last-key-text)
  (qt_last_key_text))

;;; ---- Pixmap ----

(def (qt-pixmap-load path)
  (qt_pixmap_load path))

(def (qt-pixmap-width p)
  (qt_pixmap_width p))

(def (qt-pixmap-height p)
  (qt_pixmap_height p))

(def (qt-pixmap-null? p)
  (not (= (qt_pixmap_is_null p) 0)))

(def (qt-pixmap-scaled p width height)
  (qt_pixmap_scaled p width height))

(def (qt-pixmap-destroy! p)
  (qt_pixmap_destroy p))

(def (qt-label-set-pixmap! label pixmap)
  (qt_label_set_pixmap label pixmap))

;;; ---- Icon ----

(def (qt-icon-create path)
  (qt_icon_create path))

(def (qt-icon-create-from-pixmap pixmap)
  (qt_icon_create_from_pixmap pixmap))

(def (qt-icon-null? icon)
  (not (= (qt_icon_is_null icon) 0)))

(def (qt-icon-destroy! icon)
  (qt_icon_destroy icon))

(def (qt-push-button-set-icon! button icon)
  (qt_push_button_set_icon button icon))

(def (qt-action-set-icon! action icon)
  (qt_action_set_icon action icon))

(def (qt-widget-set-window-icon! widget icon)
  (qt_widget_set_window_icon widget icon))

;;; ---- Radio Button ----

(def (qt-radio-button-create text parent: (parent #f))
  (qt_radio_button_create text parent))

(def (qt-radio-button-text r)
  (qt_radio_button_text r))

(def (qt-radio-button-set-text! r text)
  (qt_radio_button_set_text r text))

(def (qt-radio-button-checked? r)
  (not (= (qt_radio_button_is_checked r) 0)))

(def (qt-radio-button-set-checked! r checked)
  (qt_radio_button_set_checked r (if checked 1 0)))

(def (qt-on-radio-toggled! radio handler)
  (let ((id (register-qt-bool-handler! handler)))
    (raw_qt_radio_button_on_toggled radio id)
    (track-handler! radio id)))

;;; ---- Button Group ----

(def (qt-button-group-create)
  (qt_button_group_create))

(def (qt-button-group-add-button! bg button id)
  (qt_button_group_add_button bg button id))

(def (qt-button-group-remove-button! bg button)
  (qt_button_group_remove_button bg button))

(def (qt-button-group-checked-id bg)
  (qt_button_group_checked_id bg))

(def (qt-button-group-set-exclusive! bg exclusive)
  (qt_button_group_set_exclusive bg (if exclusive 1 0)))

(def (qt-button-group-exclusive? bg)
  (not (= (qt_button_group_is_exclusive bg) 0)))

(def (qt-on-button-group-clicked! bg handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_button_group_on_id_clicked bg id)
    (track-handler! bg id)))

(def (qt-button-group-destroy! bg)
  (let ((ids (hash-ref *qt-widget-handlers* bg '())))
    (for-each unregister-qt-handler! ids)
    (hash-remove! *qt-widget-handlers* bg))
  (qt_button_group_destroy bg))

;;; ---- Group Box ----

(def (qt-group-box-create title parent: (parent #f))
  (qt_group_box_create title parent))

(def (qt-group-box-title gb)
  (qt_group_box_title gb))

(def (qt-group-box-set-title! gb title)
  (qt_group_box_set_title gb title))

(def (qt-group-box-set-checkable! gb checkable)
  (qt_group_box_set_checkable gb (if checkable 1 0)))

(def (qt-group-box-checkable? gb)
  (not (= (qt_group_box_is_checkable gb) 0)))

(def (qt-group-box-set-checked! gb checked)
  (qt_group_box_set_checked gb (if checked 1 0)))

(def (qt-group-box-checked? gb)
  (not (= (qt_group_box_is_checked gb) 0)))

(def (qt-on-group-box-toggled! gb handler)
  (let ((id (register-qt-bool-handler! handler)))
    (raw_qt_group_box_on_toggled gb id)
    (track-handler! gb id)))

;;; ---- Font ----

(def (qt-font-create family point-size: (point-size 12))
  (qt_font_create family point-size))

(def (qt-font-family f)
  (qt_font_family f))

(def (qt-font-point-size f)
  (qt_font_point_size f))

(def (qt-font-bold? f)
  (not (= (qt_font_is_bold f) 0)))

(def (qt-font-set-bold! f bold)
  (qt_font_set_bold f (if bold 1 0)))

(def (qt-font-italic? f)
  (not (= (qt_font_is_italic f) 0)))

(def (qt-font-set-italic! f italic)
  (qt_font_set_italic f (if italic 1 0)))

(def (qt-font-destroy! f)
  (when f (qt_font_destroy f)))

(def (qt-widget-set-font! w font)
  (qt_widget_set_font w font))

;; Returns a NEW QFont copy — caller must destroy with qt-font-destroy! or use with-font.
(def (qt-widget-font w)
  (qt_widget_font w))

;;; ---- Color ----

(def (qt-color-create r g b alpha: (alpha 255))
  (qt_color_create_rgb r g b alpha))

(def (qt-color-create-name name)
  (qt_color_create_name name))

(def (qt-color-red c) (qt_color_red c))
(def (qt-color-green c) (qt_color_green c))
(def (qt-color-blue c) (qt_color_blue c))
(def (qt-color-alpha c) (qt_color_alpha c))

(def (qt-color-name c) (qt_color_name c))

(def (qt-color-valid? c)
  (not (= (qt_color_is_valid c) 0)))

(def (qt-color-destroy! c)
  (qt_color_destroy c))

;;; ---- Font Dialog ----

;; Returns a QFont pointer or #f if the user cancels.
;; Caller must destroy the returned font with qt-font-destroy! when done.
(def (qt-font-dialog parent: (parent #f))
  (qt_font_dialog_get_font parent))

;;; ---- Color Dialog ----

(def (qt-color-dialog initial: (initial "#ffffff") parent: (parent #f))
  (let ((c (qt_color_dialog_get_color initial parent)))
    (if (qt-color-valid? c) c
        (begin (qt-color-destroy! c) #f))))

;;; ---- Stacked Widget ----

(def (qt-stacked-widget-create parent: (parent #f))
  (qt_stacked_widget_create parent))

(def (qt-stacked-widget-add-widget! sw w)
  (qt_stacked_widget_add_widget sw w))

(def (qt-stacked-widget-set-current-index! sw idx)
  (qt_stacked_widget_set_current_index sw idx))

(def (qt-stacked-widget-current-index sw)
  (qt_stacked_widget_current_index sw))

(def (qt-stacked-widget-count sw)
  (qt_stacked_widget_count sw))

(def (qt-on-stacked-changed! sw handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_stacked_widget_on_current_changed sw id)
    (track-handler! sw id)))

;;; ---- Dock Widget ----

(def (qt-dock-widget-create title parent: (parent #f))
  (qt_dock_widget_create title parent))

(def (qt-dock-widget-set-widget! dw w)
  (qt_dock_widget_set_widget dw w))

(def (qt-dock-widget-widget dw)
  (qt_dock_widget_widget dw))

(def (qt-dock-widget-title dw)
  (qt_dock_widget_title dw))

(def (qt-dock-widget-set-title! dw title)
  (qt_dock_widget_set_title dw title))

(def (qt-dock-widget-set-floating! dw floating)
  (qt_dock_widget_set_floating dw (if floating 1 0)))

(def (qt-dock-widget-floating? dw)
  (not (= (qt_dock_widget_is_floating dw) 0)))

(def (qt-main-window-add-dock-widget! mw area dw)
  (qt_main_window_add_dock_widget mw area dw))

;;; ---- System Tray Icon ----

(def (qt-system-tray-icon-create icon parent: (parent #f))
  (qt_system_tray_icon_create icon parent))

(def (qt-system-tray-icon-set-tooltip! ti text)
  (qt_system_tray_icon_set_tooltip ti text))

(def (qt-system-tray-icon-set-icon! ti icon)
  (qt_system_tray_icon_set_icon ti icon))

(def (qt-system-tray-icon-show! ti)
  (qt_system_tray_icon_show ti))

(def (qt-system-tray-icon-hide! ti)
  (qt_system_tray_icon_hide ti))

(def (qt-system-tray-icon-show-message! ti title msg
       icon-type: (icon-type QT_TRAY_INFO) timeout: (timeout 10000))
  (qt_system_tray_icon_show_message ti title msg icon-type timeout))

(def (qt-system-tray-icon-set-context-menu! ti menu)
  (qt_system_tray_icon_set_context_menu ti menu))

(def (qt-on-tray-activated! ti handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_system_tray_icon_on_activated ti id)
    (track-handler! ti id)))

(def (qt-system-tray-available?)
  (not (= (qt_system_tray_icon_is_available) 0)))

(def (qt-system-tray-icon-destroy! ti)
  (let ((ids (hash-ref *qt-widget-handlers* ti '())))
    (for-each unregister-qt-handler! ids)
    (hash-remove! *qt-widget-handlers* ti))
  (qt_system_tray_icon_destroy ti))

;;; ---- QPainter ----

(def (qt-pixmap-create-blank w h)
  (qt_pixmap_create_blank w h))

(def (qt-pixmap-fill! pm r g b alpha: (alpha 255))
  (qt_pixmap_fill pm r g b alpha))

(def (qt-painter-create pixmap)
  (qt_painter_create pixmap))

(def (qt-painter-end! p)
  (qt_painter_end p))

(def (qt-painter-destroy! p)
  (qt_painter_destroy p))

(def (qt-painter-set-pen-color! p r g b alpha: (alpha 255))
  (qt_painter_set_pen_color p r g b alpha))

(def (qt-painter-set-pen-width! p width)
  (qt_painter_set_pen_width p width))

(def (qt-painter-set-brush-color! p r g b alpha: (alpha 255))
  (qt_painter_set_brush_color p r g b alpha))

(def (qt-painter-set-font!* p font)
  (qt_painter_set_font_painter p font))

(def (qt-painter-set-antialiasing! p enabled)
  (qt_painter_set_antialiasing p (if enabled 1 0)))

(def (qt-painter-draw-line! p x1 y1 x2 y2)
  (qt_painter_draw_line p x1 y1 x2 y2))

(def (qt-painter-draw-rect! p x y w h)
  (qt_painter_draw_rect p x y w h))

(def (qt-painter-fill-rect! p x y w h r g b alpha: (alpha 255))
  (qt_painter_fill_rect p x y w h r g b alpha))

(def (qt-painter-draw-ellipse! p x y w h)
  (qt_painter_draw_ellipse p x y w h))

(def (qt-painter-draw-text! p x y text)
  (qt_painter_draw_text p x y text))

(def (qt-painter-draw-text-rect! p x y w h flags text)
  (qt_painter_draw_text_rect p x y w h flags text))

(def (qt-painter-draw-pixmap! p x y pixmap)
  (qt_painter_draw_pixmap p x y pixmap))

(def (qt-painter-draw-point! p x y)
  (qt_painter_draw_point p x y))

(def (qt-painter-draw-arc! p x y w h start-angle span-angle)
  (qt_painter_draw_arc p x y w h start-angle span-angle))

(def (qt-painter-save! p)
  (qt_painter_save p))

(def (qt-painter-restore! p)
  (qt_painter_restore p))

(def (qt-painter-translate! p dx dy)
  (qt_painter_translate p dx dy))

(def (qt-painter-rotate! p angle)
  (qt_painter_rotate p (exact->inexact angle)))

(def (qt-painter-scale! p sx sy)
  (qt_painter_scale p (exact->inexact sx) (exact->inexact sy)))

;;; ---- Drag and Drop ----

(def (qt-widget-set-accept-drops! w accept)
  (qt_widget_set_accept_drops w (if accept 1 0)))

;; Track drop filter pointers by callback ID for cleanup
(def *qt-drop-filter-ptrs* (make-hash-table))

(def (qt-on-drop! widget handler)
  (let* ((id (register-qt-string-handler! handler))
         (df (raw_qt_drop_filter_install widget id)))
    (hash-put! *qt-drop-filter-ptrs* id df)
    (track-handler! widget id)))

(def (qt-drop-filter-last-text id)
  (let ((df (hash-ref *qt-drop-filter-ptrs* id #f)))
    (if df (qt_drop_filter_last_text df)
        (error "qt-drop-filter-last-text: unknown drop filter ID" id))))

(def (qt-drop-filter-destroy! id)
  (let ((df (hash-ref *qt-drop-filter-ptrs* id #f)))
    (when df
      (qt_drop_filter_destroy df)
      (hash-remove! *qt-drop-filter-ptrs* id)
      (untrack-handler! id)
      (unregister-qt-handler! id))))

(def (qt-drag-text! source text)
  (qt_drag_text source text))

;;; ---- Double Spin Box ----

(def (qt-double-spin-box-create parent: (parent #f))
  (qt_double_spin_box_create parent))

(def (qt-double-spin-box-value dsb)
  (qt_double_spin_box_value dsb))

(def (qt-double-spin-box-set-value! dsb val)
  (qt_double_spin_box_set_value dsb (exact->inexact val)))

(def (qt-double-spin-box-set-range! dsb min max)
  (qt_double_spin_box_set_range dsb (exact->inexact min) (exact->inexact max)))

(def (qt-double-spin-box-set-single-step! dsb step)
  (qt_double_spin_box_set_single_step dsb (exact->inexact step)))

(def (qt-double-spin-box-set-decimals! dsb dec)
  (qt_double_spin_box_set_decimals dsb dec))

(def (qt-double-spin-box-decimals dsb)
  (qt_double_spin_box_decimals dsb))

(def (qt-double-spin-box-set-prefix! dsb prefix)
  (qt_double_spin_box_set_prefix dsb prefix))

(def (qt-double-spin-box-set-suffix! dsb suffix)
  (qt_double_spin_box_set_suffix dsb suffix))

(def (qt-on-double-value-changed! dsb handler)
  (let ((id (register-qt-string-handler!
             (lambda (str) (handler (string->number str))))))
    (raw_qt_double_spin_box_on_value_changed dsb id)
    (track-handler! dsb id)))

;;; ---- Date Edit ----

(def (qt-date-edit-create parent: (parent #f))
  (qt_date_edit_create parent))

(def (qt-date-edit-set-date! d year month day)
  (qt_date_edit_set_date d year month day))

(def (qt-date-edit-year d)
  (qt_date_edit_year d))

(def (qt-date-edit-month d)
  (qt_date_edit_month d))

(def (qt-date-edit-day d)
  (qt_date_edit_day d))

(def (qt-date-edit-date-string d)
  (qt_date_edit_date_string d))

(def (qt-date-edit-set-minimum-date! d year month day)
  (qt_date_edit_set_minimum_date d year month day))

(def (qt-date-edit-set-maximum-date! d year month day)
  (qt_date_edit_set_maximum_date d year month day))

(def (qt-date-edit-set-calendar-popup! d enabled)
  (qt_date_edit_set_calendar_popup d (if enabled 1 0)))

(def (qt-date-edit-set-display-format! d fmt)
  (qt_date_edit_set_display_format d fmt))

(def (qt-on-date-changed! d handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_date_edit_on_date_changed d id)
    (track-handler! d id)))

;;; ---- Time Edit ----

(def (qt-time-edit-create parent: (parent #f))
  (qt_time_edit_create parent))

(def (qt-time-edit-set-time! t hour minute second)
  (qt_time_edit_set_time t hour minute second))

(def (qt-time-edit-hour t)
  (qt_time_edit_hour t))

(def (qt-time-edit-minute t)
  (qt_time_edit_minute t))

(def (qt-time-edit-second t)
  (qt_time_edit_second t))

(def (qt-time-edit-time-string t)
  (qt_time_edit_time_string t))

(def (qt-time-edit-set-display-format! t fmt)
  (qt_time_edit_set_display_format t fmt))

(def (qt-on-time-changed! t handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_time_edit_on_time_changed t id)
    (track-handler! t id)))

;;; ---- Frame ----

(def (qt-frame-create parent: (parent #f))
  (qt_frame_create parent))

(def (qt-frame-set-shape! f shape)
  (qt_frame_set_frame_shape f shape))

(def (qt-frame-shape f)
  (qt_frame_frame_shape f))

(def (qt-frame-set-shadow! f shadow)
  (qt_frame_set_frame_shadow f shadow))

(def (qt-frame-shadow f)
  (qt_frame_frame_shadow f))

(def (qt-frame-set-line-width! f width)
  (qt_frame_set_line_width f width))

(def (qt-frame-line-width f)
  (qt_frame_line_width f))

(def (qt-frame-set-mid-line-width! f width)
  (qt_frame_set_mid_line_width f width))

;;; ---- Progress Dialog ----

(def (qt-progress-dialog-create label cancel-text min max parent: (parent #f))
  (qt_progress_dialog_create label cancel-text min max parent))

(def (qt-progress-dialog-set-value! pd val)
  (qt_progress_dialog_set_value pd val))

(def (qt-progress-dialog-value pd)
  (qt_progress_dialog_value pd))

(def (qt-progress-dialog-set-range! pd min max)
  (qt_progress_dialog_set_range pd min max))

(def (qt-progress-dialog-set-label-text! pd text)
  (qt_progress_dialog_set_label_text pd text))

(def (qt-progress-dialog-canceled? pd)
  (not (= (qt_progress_dialog_was_canceled pd) 0)))

(def (qt-progress-dialog-set-minimum-duration! pd ms)
  (qt_progress_dialog_set_minimum_duration pd ms))

(def (qt-progress-dialog-set-auto-close! pd auto)
  (qt_progress_dialog_set_auto_close pd (if auto 1 0)))

(def (qt-progress-dialog-set-auto-reset! pd auto)
  (qt_progress_dialog_set_auto_reset pd (if auto 1 0)))

(def (qt-progress-dialog-reset! pd)
  (qt_progress_dialog_reset pd))

(def (qt-on-progress-canceled! pd handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_progress_dialog_on_canceled pd id)
    (track-handler! pd id)))

;;; ---- Input Dialog ----

(def (qt-input-dialog-get-text title label default: (default "") parent: (parent #f))
  (let ((result (qt_input_dialog_get_text parent title label default)))
    (if (= (qt_input_dialog_was_accepted) 0) #f result)))

(def (qt-input-dialog-get-int title label value: (value 0)
                              min: (min -2147483647) max: (max 2147483647)
                              step: (step 1) parent: (parent #f))
  (let ((result (qt_input_dialog_get_int parent title label value min max step)))
    (if (= (qt_input_dialog_was_accepted) 0) #f result)))

(def (qt-input-dialog-get-double title label value: (value 0.0)
                                 min: (min -2147483647.0) max: (max 2147483647.0)
                                 decimals: (decimals 1) parent: (parent #f))
  (let ((result (qt_input_dialog_get_double parent title label value min max decimals)))
    (if (= (qt_input_dialog_was_accepted) 0) #f result)))

(def (qt-input-dialog-get-item title label items current: (current 0)
                               editable: (editable #f) parent: (parent #f))
  (for-each (lambda (a)
              (when (string-contains a "\n")
                (error "qt-input-dialog-get-item: item must not contain newlines" a)))
            items)
  (let* ((items-str (string-join items "\n"))
         (result (qt_input_dialog_get_item parent title label items-str
                                           current (if editable 1 0))))
    (if (= (qt_input_dialog_was_accepted) 0) #f result)))

;;; ---- Form Layout ----

(def (qt-form-layout-create parent: (parent #f))
  (qt_form_layout_create parent))

(def (qt-form-layout-add-row! layout label field)
  (qt_form_layout_add_row layout label field))

(def (qt-form-layout-add-row-widget! layout label-widget field)
  (qt_form_layout_add_row_widget layout label-widget field))

(def (qt-form-layout-add-spanning-widget! layout widget)
  (qt_form_layout_add_spanning_widget layout widget))

(def (qt-form-layout-row-count layout)
  (qt_form_layout_row_count layout))

;;; ---- Shortcut ----

(def (qt-shortcut-create key-sequence parent)
  (qt_shortcut_create key-sequence parent))

(def (qt-shortcut-set-key! s key-sequence)
  (qt_shortcut_set_key s key-sequence))

(def (qt-shortcut-set-enabled! s enabled)
  (qt_shortcut_set_enabled s (if enabled 1 0)))

(def (qt-shortcut-enabled? s)
  (not (= (qt_shortcut_is_enabled s) 0)))

(def (qt-on-shortcut-activated! s handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_shortcut_on_activated s id)
    (track-handler! s id)))

(def (qt-shortcut-destroy! s)
  (let ((ids (hash-ref *qt-widget-handlers* s '())))
    (for-each unregister-qt-handler! ids)
    (hash-remove! *qt-widget-handlers* s))
  (qt_shortcut_destroy s))

;;; ---- Text Browser ----

(def (qt-text-browser-create parent: (parent #f))
  (qt_text_browser_create parent))

(def (qt-text-browser-set-html! tb html)
  (qt_text_browser_set_html tb html))

(def (qt-text-browser-set-plain-text! tb text)
  (qt_text_browser_set_plain_text tb text))

(def (qt-text-browser-plain-text tb)
  (qt_text_browser_plain_text tb))

(def (qt-text-browser-set-open-external-links! tb enabled)
  (qt_text_browser_set_open_external_links tb (if enabled 1 0)))

(def (qt-text-browser-set-source! tb url)
  (qt_text_browser_set_source tb url))

(def (qt-text-browser-source tb)
  (qt_text_browser_source tb))

(def (qt-text-browser-scroll-to-bottom! tb)
  (qt_text_browser_scroll_to_bottom tb))

(def (qt-text-browser-append! tb text)
  (qt_text_browser_append tb text))

(def (qt-text-browser-html tb)
  (qt_text_browser_html tb))

(def (qt-on-anchor-clicked! tb handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_text_browser_on_anchor_clicked tb id)
    (track-handler! tb id)))

;;; ---- Dialog Button Box ----

(def (qt-button-box-create buttons parent: (parent #f))
  (qt_button_box_create buttons parent))

(def (qt-button-box-button bb standard-button)
  (qt_button_box_button bb standard-button))

(def (qt-button-box-add-button! bb button role)
  (qt_button_box_add_button bb button role))

(def (qt-on-accepted! bb handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_button_box_on_accepted bb id)
    (track-handler! bb id)))

(def (qt-on-rejected! bb handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_button_box_on_rejected bb id)
    (track-handler! bb id)))

(def (qt-on-button-clicked! bb handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_button_box_on_clicked bb id)
    (track-handler! bb id)))

;;; ---- Calendar Widget ----

(def (qt-calendar-create parent: (parent #f))
  (qt_calendar_create parent))

(def (qt-calendar-set-selected-date! c year month day)
  (qt_calendar_set_selected_date c year month day))

(def (qt-calendar-selected-year c)
  (qt_calendar_selected_year c))

(def (qt-calendar-selected-month c)
  (qt_calendar_selected_month c))

(def (qt-calendar-selected-day c)
  (qt_calendar_selected_day c))

(def (qt-calendar-selected-date-string c)
  (qt_calendar_selected_date_string c))

(def (qt-calendar-set-minimum-date! c year month day)
  (qt_calendar_set_minimum_date c year month day))

(def (qt-calendar-set-maximum-date! c year month day)
  (qt_calendar_set_maximum_date c year month day))

(def (qt-calendar-set-first-day-of-week! c day)
  (qt_calendar_set_first_day_of_week c day))

(def (qt-calendar-set-grid-visible! c visible)
  (qt_calendar_set_grid_visible c (if visible 1 0)))

(def (qt-calendar-grid-visible? c)
  (not (= (qt_calendar_is_grid_visible c) 0)))

(def (qt-calendar-set-navigation-bar-visible! c visible)
  (qt_calendar_set_navigation_bar_visible c (if visible 1 0)))

(def (qt-on-selection-changed! c handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_calendar_on_selection_changed c id)
    (track-handler! c id)))

(def (qt-on-calendar-clicked! c handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_calendar_on_clicked c id)
    (track-handler! c id)))

;;; ---- QSettings ----

(def (qt-settings-create org app)
  (qt_settings_create org app))

(def (qt-settings-create-file path format: (format QT_SETTINGS_INI))
  (qt_settings_create_file path format))

(def (qt-settings-set-string! s key value)
  (qt_settings_set_string s key value))

(def (qt-settings-string s key default: (default ""))
  (qt_settings_value_string s key default))

(def (qt-settings-set-int! s key value)
  (qt_settings_set_int s key value))

(def (qt-settings-int s key default: (default 0))
  (qt_settings_value_int s key default))

(def (qt-settings-set-double! s key value)
  (qt_settings_set_double s key value))

(def (qt-settings-double s key default: (default 0.0))
  (qt_settings_value_double s key default))

(def (qt-settings-set-bool! s key value)
  (qt_settings_set_bool s key (if value 1 0)))

(def (qt-settings-bool s key default: (default #f))
  (not (= (qt_settings_value_bool s key (if default 1 0)) 0)))

;; Generic set — dispatches on value type
(def (qt-settings-set! s key value)
  (cond
    ((string? value) (qt-settings-set-string! s key value))
    ((boolean? value) (qt-settings-set-bool! s key value))
    ((integer? value) (qt-settings-set-int! s key value))
    ((real? value) (qt-settings-set-double! s key (exact->inexact value)))
    (else (qt-settings-set-string! s key (object->string value)))))

;; Generic value — returns string by default (caller must know type for typed access)
(def (qt-settings-value s key default: (default ""))
  (qt_settings_value_string s key default))

(def (qt-settings-contains? s key)
  (not (= (qt_settings_contains s key) 0)))

(def (qt-settings-remove! s key)
  (qt_settings_remove s key))

(def (qt-settings-all-keys s)
  (let ((raw (qt_settings_all_keys s)))
    (if (string=? raw "") '()
        (string-split raw #\newline))))

(def (qt-settings-child-keys s)
  (let ((raw (qt_settings_child_keys s)))
    (if (string=? raw "") '()
        (string-split raw #\newline))))

(def (qt-settings-child-groups s)
  (let ((raw (qt_settings_child_groups s)))
    (if (string=? raw "") '()
        (string-split raw #\newline))))

(def (qt-settings-begin-group! s prefix)
  (qt_settings_begin_group s prefix))

(def (qt-settings-end-group! s)
  (qt_settings_end_group s))

(def (qt-settings-group s)
  (qt_settings_group s))

(def (qt-settings-sync! s)
  (qt_settings_sync s))

(def (qt-settings-clear! s)
  (qt_settings_clear s))

(def (qt-settings-file-name s)
  (qt_settings_file_name s))

(def (qt-settings-writable? s)
  (not (= (qt_settings_is_writable s) 0)))

(def (qt-settings-destroy! s)
  (qt_settings_destroy s))

;;; ---- QCompleter ----

(def (qt-completer-create items)
  (for-each (lambda (a)
              (when (string-contains a "\n")
                (error "qt-completer-create: item must not contain newlines" a)))
            items)
  (qt_completer_create (string-join items "\n")))

(def (qt-completer-set-model-strings! c items)
  (for-each (lambda (a)
              (when (string-contains a "\n")
                (error "qt-completer-set-model-strings!: item must not contain newlines" a)))
            items)
  (qt_completer_set_model_strings c (string-join items "\n")))

(def (qt-completer-set-case-sensitivity! c sensitive?)
  (qt_completer_set_case_sensitivity c (if sensitive? 1 0)))

(def (qt-completer-set-completion-mode! c mode)
  (qt_completer_set_completion_mode c mode))

(def (qt-completer-set-filter-mode! c mode)
  (qt_completer_set_filter_mode c mode))

(def (qt-completer-set-max-visible-items! c count)
  (qt_completer_set_max_visible_items c count))

(def (qt-completer-completion-count c)
  (qt_completer_completion_count c))

(def (qt-completer-current-completion c)
  (qt_completer_current_completion c))

(def (qt-completer-set-completion-prefix! c prefix)
  (qt_completer_set_completion_prefix c prefix))

(def (qt-on-completer-activated! c handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_completer_on_activated c id)
    (track-handler! c id)))

(def (qt-line-edit-set-completer! e c)
  (qt_line_edit_set_completer e c))

(def (qt-completer-destroy! c)
  (let ((ids (hash-ref *qt-widget-handlers* c '())))
    (for-each unregister-qt-handler! ids)
    (hash-remove! *qt-widget-handlers* c))
  (qt_completer_destroy c))

;;; ---- QToolTip / QWhatsThis ----

(def (qt-tooltip-show-text! x y text widget: (widget #f))
  (qt_tooltip_show_text x y text widget))

(def (qt-tooltip-hide-text!)
  (qt_tooltip_hide_text))

(def (qt-tooltip-visible?)
  (not (= (qt_tooltip_is_visible) 0)))

(def (qt-widget-tooltip w)
  (qt_widget_tooltip w))

(def (qt-widget-set-whats-this! w text)
  (qt_widget_set_whats_this w text))

(def (qt-widget-whats-this w)
  (qt_widget_whats_this w))

;;; ---- Phase 12: Model/View Framework ----

;;; QStandardItemModel

(def (qt-standard-model-create rows: (rows 0) cols: (cols 0) parent: (parent #f))
  (qt_standard_model_create rows cols parent))

(def (qt-standard-model-destroy! m)
  (qt_standard_model_destroy m))

(def (qt-standard-model-row-count m)
  (qt_standard_model_row_count m))

(def (qt-standard-model-column-count m)
  (qt_standard_model_column_count m))

(def (qt-standard-model-set-row-count! m rows)
  (qt_standard_model_set_row_count m rows))

(def (qt-standard-model-set-column-count! m cols)
  (qt_standard_model_set_column_count m cols))

(def (qt-standard-model-set-item! m row col item)
  (qt_standard_model_set_item m row col item))

(def (qt-standard-model-item m row col)
  (qt_standard_model_item m row col))

(def (qt-standard-model-insert-row! m row)
  (not (= (qt_standard_model_insert_row m row) 0)))

(def (qt-standard-model-insert-column! m col)
  (not (= (qt_standard_model_insert_column m col) 0)))

(def (qt-standard-model-remove-row! m row)
  (not (= (qt_standard_model_remove_row m row) 0)))

(def (qt-standard-model-remove-column! m col)
  (not (= (qt_standard_model_remove_column m col) 0)))

(def (qt-standard-model-clear! m)
  (qt_standard_model_clear m))

(def (qt-standard-model-set-horizontal-header! m col text)
  (qt_standard_model_set_horizontal_header m col text))

(def (qt-standard-model-set-vertical-header! m row text)
  (qt_standard_model_set_vertical_header m row text))

;;; QStandardItem

(def (qt-standard-item-create text: (text ""))
  (qt_standard_item_create text))

(def (qt-standard-item-text item)
  (qt_standard_item_text item))

(def (qt-standard-item-set-text! item text)
  (qt_standard_item_set_text item text))

(def (qt-standard-item-tooltip item)
  (qt_standard_item_tooltip item))

(def (qt-standard-item-set-tooltip! item text)
  (qt_standard_item_set_tooltip item text))

(def (qt-standard-item-set-editable! item val)
  (qt_standard_item_set_editable item (if val 1 0)))

(def (qt-standard-item-editable? item)
  (not (= (qt_standard_item_is_editable item) 0)))

(def (qt-standard-item-set-enabled! item val)
  (qt_standard_item_set_enabled item (if val 1 0)))

(def (qt-standard-item-enabled? item)
  (not (= (qt_standard_item_is_enabled item) 0)))

(def (qt-standard-item-set-selectable! item val)
  (qt_standard_item_set_selectable item (if val 1 0)))

(def (qt-standard-item-selectable? item)
  (not (= (qt_standard_item_is_selectable item) 0)))

(def (qt-standard-item-set-checkable! item val)
  (qt_standard_item_set_checkable item (if val 1 0)))

(def (qt-standard-item-checkable? item)
  (not (= (qt_standard_item_is_checkable item) 0)))

(def (qt-standard-item-set-check-state! item state)
  (qt_standard_item_set_check_state item state))

(def (qt-standard-item-check-state item)
  (qt_standard_item_check_state item))

(def (qt-standard-item-set-icon! item icon)
  (qt_standard_item_set_icon item icon))

(def (qt-standard-item-append-row! parent child)
  (qt_standard_item_append_row parent child))

(def (qt-standard-item-row-count item)
  (qt_standard_item_row_count item))

(def (qt-standard-item-column-count item)
  (qt_standard_item_column_count item))

(def (qt-standard-item-child item row col: (col 0))
  (qt_standard_item_child item row col))

;;; QStringListModel

(def (qt-string-list-model-create items: (items '()))
  (unless (null? items)
    (for-each (lambda (a)
                (when (string-contains a "\n")
                  (error "qt-string-list-model-create: item must not contain newlines" a)))
              items))
  (let ((str (if (null? items) "" (string-join items "\n"))))
    (qt_string_list_model_create str)))

(def (qt-string-list-model-destroy! m)
  (qt_string_list_model_destroy m))

(def (qt-string-list-model-set-strings! m items)
  (for-each (lambda (a)
              (when (string-contains a "\n")
                (error "qt-string-list-model-set-strings!: item must not contain newlines" a)))
            items)
  (let ((str (if (null? items) "" (string-join items "\n"))))
    (qt_string_list_model_set_strings m str)))

(def (qt-string-list-model-strings m)
  (let ((raw (qt_string_list_model_strings m)))
    (if (string=? raw "") '() (string-split raw #\newline))))

(def (qt-string-list-model-row-count m)
  (qt_string_list_model_row_count m))

;;; Views (common)

(def (qt-view-set-model! view model)
  (qt_view_set_model view model))

(def (qt-view-set-selection-mode! view mode)
  (qt_view_set_selection_mode view mode))

(def (qt-view-set-selection-behavior! view behavior)
  (qt_view_set_selection_behavior view behavior))

(def (qt-view-set-alternating-row-colors! view val)
  (qt_view_set_alternating_row_colors view (if val 1 0)))

(def (qt-view-set-sorting-enabled! view val)
  (qt_view_set_sorting_enabled view (if val 1 0)))

(def (qt-view-set-edit-triggers! view triggers)
  (qt_view_set_edit_triggers view triggers))

;;; QListView

(def (qt-list-view-create parent: (parent #f))
  (qt_list_view_create parent))

(def (qt-list-view-set-flow! v flow)
  (qt_list_view_set_flow v flow))

;;; QTableView

(def (qt-table-view-create parent: (parent #f))
  (qt_table_view_create parent))

(def (qt-table-view-set-column-width! v col w)
  (qt_table_view_set_column_width v col w))

(def (qt-table-view-set-row-height! v row h)
  (qt_table_view_set_row_height v row h))

(def (qt-table-view-hide-column! v col)
  (qt_table_view_hide_column v col))

(def (qt-table-view-show-column! v col)
  (qt_table_view_show_column v col))

(def (qt-table-view-hide-row! v row)
  (qt_table_view_hide_row v row))

(def (qt-table-view-show-row! v row)
  (qt_table_view_show_row v row))

(def (qt-table-view-resize-columns-to-contents! v)
  (qt_table_view_resize_columns_to_contents v))

(def (qt-table-view-resize-rows-to-contents! v)
  (qt_table_view_resize_rows_to_contents v))

;;; QTreeView

(def (qt-tree-view-create parent: (parent #f))
  (qt_tree_view_create parent))

(def (qt-tree-view-expand-all! v)
  (qt_tree_view_expand_all v))

(def (qt-tree-view-collapse-all! v)
  (qt_tree_view_collapse_all v))

(def (qt-tree-view-set-indentation! v indent)
  (qt_tree_view_set_indentation v indent))

(def (qt-tree-view-indentation v)
  (qt_tree_view_indentation v))

(def (qt-tree-view-set-root-is-decorated! v val)
  (qt_tree_view_set_root_is_decorated v (if val 1 0)))

(def (qt-tree-view-set-header-hidden! v val)
  (qt_tree_view_set_header_hidden v (if val 1 0)))

(def (qt-tree-view-set-column-width! v col w)
  (qt_tree_view_set_column_width v col w))

;;; QHeaderView (via view)

(def (qt-view-header-set-stretch-last-section! view val horizontal: (horizontal #t))
  (qt_view_header_set_stretch_last_section view (if horizontal 1 0) (if val 1 0)))

(def (qt-view-header-set-section-resize-mode! view mode horizontal: (horizontal #t))
  (qt_view_header_set_section_resize_mode view (if horizontal 1 0) mode))

(def (qt-view-header-hide! view horizontal: (horizontal #t))
  (qt_view_header_hide view (if horizontal 1 0)))

(def (qt-view-header-show! view horizontal: (horizontal #t))
  (qt_view_header_show view (if horizontal 1 0)))

(def (qt-view-header-set-default-section-size! view size horizontal: (horizontal #t))
  (qt_view_header_set_default_section_size view (if horizontal 1 0) size))

;;; QSortFilterProxyModel

(def (qt-sort-filter-proxy-create parent: (parent #f))
  (qt_sort_filter_proxy_create parent))

(def (qt-sort-filter-proxy-destroy! p)
  (qt_sort_filter_proxy_destroy p))

(def (qt-sort-filter-proxy-set-source-model! p model)
  (qt_sort_filter_proxy_set_source_model p model))

(def (qt-sort-filter-proxy-set-filter-regex! p pattern)
  (qt_sort_filter_proxy_set_filter_regex p pattern))

(def (qt-sort-filter-proxy-set-filter-column! p col)
  (qt_sort_filter_proxy_set_filter_column p col))

(def (qt-sort-filter-proxy-set-filter-case-sensitivity! p cs)
  (qt_sort_filter_proxy_set_filter_case_sensitivity p cs))

(def (qt-sort-filter-proxy-set-filter-role! p role)
  (qt_sort_filter_proxy_set_filter_role p role))

(def (qt-sort-filter-proxy-sort! p col order: (order QT_SORT_ASCENDING))
  (qt_sort_filter_proxy_sort p col order))

(def (qt-sort-filter-proxy-set-sort-role! p role)
  (qt_sort_filter_proxy_set_sort_role p role))

(def (qt-sort-filter-proxy-set-dynamic-sort-filter! p val)
  (qt_sort_filter_proxy_set_dynamic_sort_filter p (if val 1 0)))

(def (qt-sort-filter-proxy-invalidate-filter! p)
  (qt_sort_filter_proxy_invalidate_filter p))

(def (qt-sort-filter-proxy-row-count p)
  (qt_sort_filter_proxy_row_count p))

;;; View signals + selection

(def (qt-on-view-clicked! view handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_view_on_clicked view id)
    (track-handler! view id)))

(def (qt-on-view-double-clicked! view handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_view_on_double_clicked view id)
    (track-handler! view id)))

(def (qt-on-view-activated! view handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_view_on_activated view id)
    (track-handler! view id)))

(def (qt-on-view-selection-changed! view handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_view_on_selection_changed view id)
    (track-handler! view id)))

(def (qt-view-last-clicked-row)
  (qt_view_last_clicked_row))

(def (qt-view-last-clicked-col)
  (qt_view_last_clicked_col))

(def (qt-view-selected-rows view)
  (let ((raw (qt_view_selected_rows view)))
    (if (string=? raw "")
      '()
      (map string->number (string-split raw #\newline)))))

(def (qt-view-current-row view)
  (qt_view_current_row view))

;;; ---- Phase 13: Practical Polish ----

;; QValidator

(def (qt-int-validator-create minimum maximum parent: (parent #f))
  (qt_int_validator_create minimum maximum parent))

(def (qt-double-validator-create bottom top decimals: (decimals 2)
                                 parent: (parent #f))
  (qt_double_validator_create (exact->inexact bottom) (exact->inexact top) decimals parent))

(def (qt-regex-validator-create pattern parent: (parent #f))
  (qt_regex_validator_create pattern parent))

(def (qt-validator-destroy! v)
  (qt_validator_destroy v))

(def (qt-validator-validate v input)
  (qt_validator_validate v input))

(def (qt-line-edit-set-validator! edit validator)
  (qt_line_edit_set_validator edit validator))

(def (qt-line-edit-acceptable-input? edit)
  (not (= (qt_line_edit_has_acceptable_input edit) 0)))

;; QPlainTextEdit

(def (qt-plain-text-edit-create parent: (parent #f))
  (qt_plain_text_edit_create parent))

(def (qt-plain-text-edit-set-text! edit text)
  (qt_plain_text_edit_set_text edit text))

(def (qt-plain-text-edit-text edit)
  (qt_plain_text_edit_text edit))

(def (qt-plain-text-edit-append! edit text)
  (qt_plain_text_edit_append edit text))

(def (qt-plain-text-edit-clear! edit)
  (qt_plain_text_edit_clear edit))

(def (qt-plain-text-edit-set-read-only! edit read-only)
  (qt_plain_text_edit_set_read_only edit (if read-only 1 0)))

(def (qt-plain-text-edit-read-only? edit)
  (not (= (qt_plain_text_edit_is_read_only edit) 0)))

(def (qt-plain-text-edit-set-placeholder! edit text)
  (qt_plain_text_edit_set_placeholder edit text))

(def (qt-plain-text-edit-line-count edit)
  (qt_plain_text_edit_line_count edit))

(def (qt-plain-text-edit-set-max-block-count! edit count)
  (qt_plain_text_edit_set_max_block_count edit count))

(def (qt-plain-text-edit-cursor-line edit)
  (qt_plain_text_edit_cursor_line edit))

(def (qt-plain-text-edit-cursor-column edit)
  (qt_plain_text_edit_cursor_column edit))

(def (qt-plain-text-edit-set-line-wrap! edit mode)
  (qt_plain_text_edit_set_line_wrap edit mode))

(def (qt-on-plain-text-edit-text-changed! edit handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_plain_text_edit_on_text_changed edit id)
    (track-handler! edit id)))

;; Phase 17: QPlainTextEdit Editor Extensions

(def (qt-plain-text-edit-cursor-position edit)
  (qt_plain_text_edit_cursor_position edit))

(def (qt-plain-text-edit-set-cursor-position! edit pos)
  (qt_plain_text_edit_set_cursor_position edit pos))

(def (qt-plain-text-edit-move-cursor! edit operation mode: (mode QT_MOVE_ANCHOR))
  (qt_plain_text_edit_move_cursor edit operation mode))

(def (qt-plain-text-edit-select-all! edit)
  (qt_plain_text_edit_select_all edit))

(def (qt-plain-text-edit-selected-text edit)
  (qt_plain_text_edit_selected_text edit))

(def (qt-plain-text-edit-selection-start edit)
  (qt_plain_text_edit_selection_start edit))

(def (qt-plain-text-edit-selection-end edit)
  (qt_plain_text_edit_selection_end edit))

(def (qt-plain-text-edit-set-selection! edit start end)
  (qt_plain_text_edit_set_selection edit start end))

(def (qt-plain-text-edit-has-selection? edit)
  (not (= (qt_plain_text_edit_has_selection edit) 0)))

(def (qt-plain-text-edit-insert-text! edit text)
  (qt_plain_text_edit_insert_text edit text))

(def (qt-plain-text-edit-remove-selected-text! edit)
  (qt_plain_text_edit_remove_selected_text edit))

(def (qt-plain-text-edit-undo! edit)
  (qt_plain_text_edit_undo edit))

(def (qt-plain-text-edit-redo! edit)
  (qt_plain_text_edit_redo edit))

(def (qt-plain-text-edit-can-undo? edit)
  (not (= (qt_plain_text_edit_can_undo edit) 0)))

(def (qt-plain-text-edit-cut! edit)
  (qt_plain_text_edit_cut edit))

(def (qt-plain-text-edit-copy! edit)
  (qt_plain_text_edit_copy edit))

(def (qt-plain-text-edit-paste! edit)
  (qt_plain_text_edit_paste edit))

(def (qt-plain-text-edit-text-length edit)
  (qt_plain_text_edit_text_length edit))

(def (qt-plain-text-edit-text-range edit start end)
  (qt_plain_text_edit_text_range edit start end))

(def (qt-plain-text-edit-line-from-position edit pos)
  (qt_plain_text_edit_line_from_position edit pos))

(def (qt-plain-text-edit-line-end-position edit line)
  (qt_plain_text_edit_line_end_position edit line))

(def (qt-plain-text-edit-find-text edit text flags: (flags 0))
  (qt_plain_text_edit_find_text edit text flags))

(def (qt-plain-text-edit-ensure-cursor-visible! edit)
  (qt_plain_text_edit_ensure_cursor_visible edit))

(def (qt-plain-text-edit-center-cursor! edit)
  (qt_plain_text_edit_center_cursor edit))

(def (qt-text-document-create)
  (qt_text_document_create))

(def (qt-text-document-destroy! doc)
  (qt_text_document_destroy doc))

(def (qt-plain-text-edit-document edit)
  (qt_plain_text_edit_document edit))

(def (qt-plain-text-edit-set-document! edit doc)
  (qt_plain_text_edit_set_document edit doc))

(def (qt-text-document-modified? doc)
  (not (= (qt_text_document_is_modified doc) 0)))

(def (qt-text-document-set-modified! doc val)
  (qt_text_document_set_modified doc (if val 1 0)))

;; QToolButton

(def (qt-tool-button-create parent: (parent #f))
  (qt_tool_button_create parent))

(def (qt-tool-button-set-text! btn text)
  (qt_tool_button_set_text btn text))

(def (qt-tool-button-text btn)
  (qt_tool_button_text btn))

(def (qt-tool-button-set-icon! btn path)
  (qt_tool_button_set_icon btn path))

(def (qt-tool-button-set-menu! btn menu)
  (qt_tool_button_set_menu btn menu))

(def (qt-tool-button-set-popup-mode! btn mode)
  (qt_tool_button_set_popup_mode btn mode))

(def (qt-tool-button-set-auto-raise! btn auto-raise)
  (qt_tool_button_set_auto_raise btn (if auto-raise 1 0)))

(def (qt-tool-button-set-arrow-type! btn arrow)
  (qt_tool_button_set_arrow_type btn arrow))

(def (qt-tool-button-set-tool-button-style! btn style)
  (qt_tool_button_set_tool_button_style btn style))

(def (qt-on-tool-button-clicked! btn handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_tool_button_on_clicked btn id)
    (track-handler! btn id)))

;; Layout spacers

(def (qt-layout-add-spacing! layout size)
  (qt_layout_add_spacing layout size))

;; QSizePolicy

(def (qt-widget-set-size-policy! widget h-policy v-policy)
  (qt_widget_set_size_policy widget h-policy v-policy))

(def (qt-layout-set-stretch-factor! layout widget stretch)
  (qt_layout_set_stretch_factor layout widget stretch))

;; ========== Phase 14: Graphics Scene & Custom Painting ==========

;; QGraphicsScene

(def (qt-graphics-scene-create x y w h)
  (qt_graphics_scene_create (exact->inexact x) (exact->inexact y)
                            (exact->inexact w) (exact->inexact h)))

(def (qt-graphics-scene-add-rect! scene x y w h)
  (qt_graphics_scene_add_rect scene (exact->inexact x) (exact->inexact y)
                              (exact->inexact w) (exact->inexact h)))

(def (qt-graphics-scene-add-ellipse! scene x y w h)
  (qt_graphics_scene_add_ellipse scene (exact->inexact x) (exact->inexact y)
                                 (exact->inexact w) (exact->inexact h)))

(def (qt-graphics-scene-add-line! scene x1 y1 x2 y2)
  (qt_graphics_scene_add_line scene (exact->inexact x1) (exact->inexact y1)
                              (exact->inexact x2) (exact->inexact y2)))

(def (qt-graphics-scene-add-text! scene text)
  (qt_graphics_scene_add_text scene text))

(def (qt-graphics-scene-add-pixmap! scene pixmap)
  (qt_graphics_scene_add_pixmap scene pixmap))

(def (qt-graphics-scene-remove-item! scene item)
  (qt_graphics_scene_remove_item scene item))

(def (qt-graphics-scene-clear! scene)
  (qt_graphics_scene_clear scene))

(def (qt-graphics-scene-items-count scene)
  (qt_graphics_scene_items_count scene))

(def (qt-graphics-scene-set-background! scene r g b)
  (qt_graphics_scene_set_background scene r g b))

(def (qt-graphics-scene-destroy! scene)
  (qt_graphics_scene_destroy scene))

;; QGraphicsView

(def (qt-graphics-view-create scene parent: (parent #f))
  (qt_graphics_view_create scene parent))

(def (qt-graphics-view-set-render-hint! view hint on: (on #t))
  (qt_graphics_view_set_render_hint view hint (if on 1 0)))

(def (qt-graphics-view-set-drag-mode! view mode)
  (qt_graphics_view_set_drag_mode view mode))

(def (qt-graphics-view-fit-in-view! view)
  (qt_graphics_view_fit_in_view view))

(def (qt-graphics-view-scale! view sx sy)
  (qt_graphics_view_scale view (exact->inexact sx) (exact->inexact sy)))

(def (qt-graphics-view-center-on! view x y)
  (qt_graphics_view_center_on view (exact->inexact x) (exact->inexact y)))

;; QGraphicsItem

(def (qt-graphics-item-set-pos! item x y)
  (qt_graphics_item_set_pos item (exact->inexact x) (exact->inexact y)))

(def (qt-graphics-item-x item)
  (qt_graphics_item_x item))

(def (qt-graphics-item-y item)
  (qt_graphics_item_y item))

(def (qt-graphics-item-set-pen! item r g b width: (width 1))
  (qt_graphics_item_set_pen item r g b width))

(def (qt-graphics-item-set-brush! item r g b)
  (qt_graphics_item_set_brush item r g b))

(def (qt-graphics-item-set-flags! item flags)
  (qt_graphics_item_set_flags item flags))

(def (qt-graphics-item-set-tooltip! item text)
  (qt_graphics_item_set_tooltip item text))

(def (qt-graphics-item-set-zvalue! item z)
  (qt_graphics_item_set_zvalue item (exact->inexact z)))

(def (qt-graphics-item-zvalue item)
  (qt_graphics_item_zvalue item))

(def (qt-graphics-item-set-rotation! item angle)
  (qt_graphics_item_set_rotation item (exact->inexact angle)))

(def (qt-graphics-item-set-scale! item factor)
  (qt_graphics_item_set_scale item (exact->inexact factor)))

(def (qt-graphics-item-set-visible! item visible)
  (qt_graphics_item_set_visible item (if visible 1 0)))

;; PaintWidget

(def (qt-paint-widget-create parent: (parent #f))
  (qt_paint_widget_create parent))

(def (qt-paint-widget-on-paint! widget handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_paint_widget_on_paint widget id)
    (track-handler! widget id)))

(def (qt-paint-widget-painter widget)
  (qt_paint_widget_painter widget))

(def (qt-paint-widget-update! widget)
  (qt_paint_widget_update widget))

(def (qt-paint-widget-width widget)
  (qt_paint_widget_width widget))

(def (qt-paint-widget-height widget)
  (qt_paint_widget_height widget))

;;; ========== Phase 15: Process, Wizard, MDI ==========

;;; ---- QProcess ----

;; Track handler IDs per process for cleanup
(def *qt-process-handlers* (make-hash-table))

(def (qt-process-create parent: (parent #f))
  (qt_process_create parent))

(def (qt-process-start! proc program args: (args []))
  (for-each (lambda (a)
              (when (string-contains a "\n")
                (error "qt-process-start!: argument must not contain newlines" a)))
            args)
  (let ((args-str (string-join args "\n")))
    (qt_process_start proc program args-str)))

(def (qt-process-write! proc data)
  (qt_process_write proc data))

(def (qt-process-close-write! proc)
  (qt_process_close_write proc))

(def (qt-process-read-stdout proc)
  (qt_process_read_stdout proc))

(def (qt-process-read-stderr proc)
  (qt_process_read_stderr proc))

(def (qt-process-wait-for-finished proc msecs: (msecs 30000))
  (not (= 0 (qt_process_wait_for_finished proc msecs))))

(def (qt-process-exit-code proc)
  (qt_process_exit_code proc))

(def (qt-process-state proc)
  (qt_process_state proc))

(def (qt-process-kill! proc)
  (qt_process_kill proc))

(def (qt-process-terminate! proc)
  (qt_process_terminate proc))

(def (qt-process-on-finished! proc handler)
  (let ((id (register-qt-int-handler! handler)))
    (let ((ids (hash-ref *qt-process-handlers* proc '())))
      (hash-put! *qt-process-handlers* proc (cons id ids)))
    (raw_qt_process_on_finished proc id)
    (track-handler! proc id)))

(def (qt-process-on-ready-read! proc handler)
  (let ((id (register-qt-void-handler! handler)))
    (let ((ids (hash-ref *qt-process-handlers* proc '())))
      (hash-put! *qt-process-handlers* proc (cons id ids)))
    (raw_qt_process_on_ready_read proc id)
    (track-handler! proc id)))

(def (qt-process-destroy! proc)
  (let ((ids (hash-ref *qt-process-handlers* proc '())))
    (for-each unregister-qt-handler! ids)
    (hash-remove! *qt-process-handlers* proc))
  (hash-remove! *qt-widget-handlers* proc)
  (qt_process_destroy proc))

;;; ---- QWizard / QWizardPage ----

(def (qt-wizard-create parent: (parent #f))
  (qt_wizard_create parent))

(def (qt-wizard-add-page! wizard page)
  (qt_wizard_add_page wizard page))

(def (qt-wizard-set-start-id! wizard id)
  (qt_wizard_set_start_id wizard id))

(def (qt-wizard-current-id wizard)
  (qt_wizard_current_id wizard))

(def (qt-wizard-set-title! wizard title)
  (qt_wizard_set_title wizard title))

(def (qt-wizard-exec! wizard)
  (qt_wizard_exec wizard))

(def (qt-wizard-page-create parent: (parent #f))
  (qt_wizard_page_create parent))

(def (qt-wizard-page-set-title! page title)
  (qt_wizard_page_set_title page title))

(def (qt-wizard-page-set-subtitle! page subtitle)
  (qt_wizard_page_set_subtitle page subtitle))

(def (qt-wizard-page-set-layout! page layout)
  (qt_wizard_page_set_layout page layout))

(def (qt-wizard-on-current-changed! wizard handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_wizard_on_current_changed wizard id)
    (track-handler! wizard id)))

;;; ---- QMdiArea / QMdiSubWindow ----

(def (qt-mdi-area-create parent: (parent #f))
  (qt_mdi_area_create parent))

(def (qt-mdi-area-add-sub-window! area widget)
  (qt_mdi_area_add_sub_window area widget))

(def (qt-mdi-area-remove-sub-window! area sub)
  (qt_mdi_area_remove_sub_window area sub))

(def (qt-mdi-area-active-sub-window area)
  (qt_mdi_area_active_sub_window area))

(def (qt-mdi-area-sub-window-count area)
  (qt_mdi_area_sub_window_count area))

(def (qt-mdi-area-cascade! area)
  (qt_mdi_area_cascade area))

(def (qt-mdi-area-tile! area)
  (qt_mdi_area_tile area))

(def (qt-mdi-area-set-view-mode! area mode)
  (qt_mdi_area_set_view_mode area mode))

(def (qt-mdi-sub-window-set-title! sub title)
  (qt_mdi_sub_window_set_title sub title))

(def (qt-mdi-area-on-sub-window-activated! area handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_mdi_area_on_sub_window_activated area id)
    (track-handler! area id)))

;;; ---- QDial ----

(def (qt-dial-create parent: (parent #f))
  (qt_dial_create parent))

(def (qt-dial-set-value! dial value)
  (qt_dial_set_value dial value))

(def (qt-dial-value dial)
  (qt_dial_value dial))

(def (qt-dial-set-range! dial min max)
  (qt_dial_set_range dial min max))

(def (qt-dial-set-notches-visible! dial visible)
  (qt_dial_set_notches_visible dial (if visible 1 0)))

(def (qt-dial-set-wrapping! dial wrap)
  (qt_dial_set_wrapping dial (if wrap 1 0)))

(def (qt-dial-on-value-changed! dial handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_dial_on_value_changed dial id)
    (track-handler! dial id)))

;;; ---- QLCDNumber ----

(def (qt-lcd-create digits: (digits 5) parent: (parent #f))
  (qt_lcd_create digits parent))

(def (qt-lcd-display-int! lcd value)
  (qt_lcd_display_int lcd value))

(def (qt-lcd-display-double! lcd value)
  (qt_lcd_display_double lcd (exact->inexact value)))

(def (qt-lcd-display-string! lcd text)
  (qt_lcd_display_string lcd text))

(def (qt-lcd-set-mode! lcd mode)
  (qt_lcd_set_mode lcd mode))

(def (qt-lcd-set-segment-style! lcd style)
  (qt_lcd_set_segment_style lcd style))

;;; ---- QToolBox ----

(def (qt-tool-box-create parent: (parent #f))
  (qt_tool_box_create parent))

(def (qt-tool-box-add-item! toolbox widget text)
  (qt_tool_box_add_item toolbox widget text))

(def (qt-tool-box-set-current-index! toolbox idx)
  (qt_tool_box_set_current_index toolbox idx))

(def (qt-tool-box-current-index toolbox)
  (qt_tool_box_current_index toolbox))

(def (qt-tool-box-count toolbox)
  (qt_tool_box_count toolbox))

(def (qt-tool-box-set-item-text! toolbox idx text)
  (qt_tool_box_set_item_text toolbox idx text))

(def (qt-tool-box-on-current-changed! toolbox handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_tool_box_on_current_changed toolbox id)
    (track-handler! toolbox id)))

;;; ---- QUndoStack ----

;; Track handler IDs per undo stack for cleanup
(def *qt-undo-stack-handlers* (make-hash-table))

(def (qt-undo-stack-create parent: (parent #f))
  (qt_undo_stack_create parent))

(def (qt-undo-stack-push! stack text undo-handler redo-handler)
  (let ((undo-id (register-qt-void-handler! undo-handler))
        (redo-id (register-qt-void-handler! redo-handler))
        (cleanup-id #f))
    ;; Register a cleanup handler that C++ calls when the command is destroyed
    ;; (e.g. when Qt truncates undone commands after a new push).
    (set! cleanup-id
      (register-qt-void-handler!
       (lambda ()
         ;; Unregister undo, redo, and cleanup handlers from dispatch tables
         (unregister-qt-handler! undo-id)
         (unregister-qt-handler! redo-id)
         (unregister-qt-handler! cleanup-id)
         ;; Remove from undo-stack secondary tracking
         (let ((ids (hash-ref *qt-undo-stack-handlers* stack '())))
           (let ((new-ids (filter (lambda (x) (not (or (= x undo-id) (= x redo-id) (= x cleanup-id)))) ids)))
             (if (null? new-ids)
               (hash-remove! *qt-undo-stack-handlers* stack)
               (hash-put! *qt-undo-stack-handlers* stack new-ids))))
         ;; Remove from primary widget tracking
         (let ((ids (hash-ref *qt-widget-handlers* stack '())))
           (let ((new-ids (filter (lambda (x) (not (or (= x undo-id) (= x redo-id) (= x cleanup-id)))) ids)))
             (if (null? new-ids)
               (hash-remove! *qt-widget-handlers* stack)
               (hash-put! *qt-widget-handlers* stack new-ids)))))))
    ;; Track all three handler IDs
    (let ((ids (hash-ref *qt-undo-stack-handlers* stack '())))
      (hash-put! *qt-undo-stack-handlers* stack (cons* undo-id redo-id cleanup-id ids)))
    (track-handler! stack undo-id)
    (track-handler! stack redo-id)
    (track-handler! stack cleanup-id)
    (raw_qt_undo_stack_push stack text undo-id redo-id cleanup-id)))

(def (qt-undo-stack-undo! stack)
  (qt_undo_stack_undo stack))

(def (qt-undo-stack-redo! stack)
  (qt_undo_stack_redo stack))

(def (qt-undo-stack-can-undo? stack)
  (not (= 0 (qt_undo_stack_can_undo stack))))

(def (qt-undo-stack-can-redo? stack)
  (not (= 0 (qt_undo_stack_can_redo stack))))

(def (qt-undo-stack-undo-text stack)
  (qt_undo_stack_undo_text stack))

(def (qt-undo-stack-redo-text stack)
  (qt_undo_stack_redo_text stack))

(def (qt-undo-stack-clear! stack)
  (let ((ids (hash-ref *qt-undo-stack-handlers* stack '())))
    (for-each unregister-qt-handler! ids)
    (hash-put! *qt-undo-stack-handlers* stack '()))
  (hash-remove! *qt-widget-handlers* stack)
  (qt_undo_stack_clear stack))

(def (qt-undo-stack-create-undo-action stack parent: (parent #f))
  (qt_undo_stack_create_undo_action stack parent))

(def (qt-undo-stack-create-redo-action stack parent: (parent #f))
  (qt_undo_stack_create_redo_action stack parent))

(def (qt-undo-stack-destroy! stack)
  (let ((ids (hash-ref *qt-undo-stack-handlers* stack '())))
    (for-each unregister-qt-handler! ids)
    (hash-remove! *qt-undo-stack-handlers* stack))
  (hash-remove! *qt-widget-handlers* stack)
  (qt_undo_stack_destroy stack))

;;; ---- QFileSystemModel ----

(def (qt-file-system-model-create parent: (parent #f))
  (qt_file_system_model_create parent))

(def (qt-file-system-model-set-root-path! model path)
  (qt_file_system_model_set_root_path model path))

(def (qt-file-system-model-set-filter! model filters)
  (qt_file_system_model_set_filter model filters))

(def (qt-file-system-model-set-name-filters! model patterns)
  (for-each (lambda (a)
              (when (string-contains a "\n")
                (error "qt-file-system-model-set-name-filters!: item must not contain newlines" a)))
            patterns)
  (let ((patterns-str (string-join patterns "\n")))
    (qt_file_system_model_set_name_filters model patterns-str)))

(def (qt-file-system-model-file-path model row column: (column 0))
  (qt_file_system_model_file_path model row column))

(def (qt-tree-view-set-file-system-root! view model path)
  (qt_tree_view_set_file_system_root view model path))

(def (qt-file-system-model-destroy! model)
  (qt_file_system_model_destroy model))

;;; ---- Signal Disconnect ----

(def (qt-disconnect-all! obj)
  (let ((ids (hash-ref *qt-widget-handlers* obj '())))
    (for-each (lambda (id)
                (unregister-qt-handler! id)
                ;; Destroy drop filter C++ object if applicable
                (let ((df (hash-ref *qt-drop-filter-ptrs* id #f)))
                  (when df
                    (qt_drop_filter_destroy df)
                    (hash-remove! *qt-drop-filter-ptrs* id))))
              ids)
    (hash-remove! *qt-widget-handlers* obj))
  ;; Clean up secondary tracking tables if applicable
  (hash-remove! *qt-undo-stack-handlers* obj)
  (hash-remove! *qt-process-handlers* obj)
  (qt_disconnect_all obj))

;;; ---- Resource-safety macros ----

(defrule (with-painter (p pixmap) body ...)
  (let ((p (qt-painter-create pixmap)))
    (try body ...
      (finally (qt-painter-end! p) (qt-painter-destroy! p)))))

(defrule (with-font (f family . args) body ...)
  (let ((f (qt-font-create family . args)))
    (try body ...
      (finally (qt-font-destroy! f)))))

(defrule (with-color (c . args) body ...)
  (let ((c (qt-color-create . args)))
    (try body ...
      (finally (qt-color-destroy! c)))))

(defrule (with-pixmap (pm . args) body ...)
  (let ((pm (qt-pixmap-create-blank . args)))
    (try body ...
      (finally (qt-pixmap-destroy! pm)))))

(defrule (with-icon var expr body ...)
  (let ((var expr))
    (try body ...
      (finally (qt-icon-destroy! var)))))

(defrule (with-settings var expr body ...)
  (let ((var expr))
    (try body ...
      (finally (qt-settings-destroy! var)))))
