(export
  ;; Lifecycle
  qt-app-create qt-app-exec! qt-app-quit!
  qt-app-process-events! qt-app-destroy!
  with-qt-app

  ;; Widget
  qt-widget-create qt-widget-show! qt-widget-hide! qt-widget-close!
  qt-widget-set-enabled! qt-widget-enabled?
  qt-widget-set-visible! qt-widget-set-fixed-size!
  qt-widget-set-minimum-size! qt-widget-set-maximum-size!
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
  qt-label-set-alignment!

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
  qt-on-key-press! qt-last-key-code qt-last-key-modifiers qt-last-key-text

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
  QT_FRIDAY QT_SATURDAY QT_SUNDAY)

(import :gerbil-qt/libqt
        :std/srfi/13)

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
  (qt_application_destroy app))

(defrule (with-qt-app app body ...)
  (let ((app (qt-app-create)))
    (try body ...
      (finally (qt-app-destroy! app)))))

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

(def (qt-widget-set-fixed-size! w width height)
  (qt_widget_set_fixed_size w width height))

(def (qt-widget-set-minimum-size! w width height)
  (qt_widget_set_minimum_size w width height))

(def (qt-widget-set-maximum-size! w width height)
  (qt_widget_set_maximum_size w width height))

(def (qt-widget-resize! w width height)
  (qt_widget_resize w width height))

(def (qt-widget-set-style-sheet! w css)
  (qt_widget_set_style_sheet w css))

(def (qt-widget-set-tooltip! w text)
  (qt_widget_set_tooltip w text))

(def (qt-widget-set-font-size! w size)
  (qt_widget_set_font_size w size))

(def (qt-widget-destroy! w)
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

;;; ---- Push Button ----

(def (qt-push-button-create text parent: (parent #f))
  (qt_push_button_create text parent))

(def (qt-push-button-set-text! b text)
  (qt_push_button_set_text b text))

(def (qt-push-button-text b)
  (qt_push_button_text b))

(def (qt-on-clicked! button handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_push_button_on_clicked button id)))

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
    (raw_qt_line_edit_on_text_changed line-edit id)))

(def (qt-on-return-pressed! line-edit handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_line_edit_on_return_pressed line-edit id)))

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
    (raw_qt_check_box_on_toggled check-box id)))

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
    (raw_qt_combo_box_on_current_index_changed combo-box id)))

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

(def (qt-on-text-edit-changed! text-edit handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_text_edit_on_text_changed text-edit id)))

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
    (raw_qt_spin_box_on_value_changed spin-box id)))

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
    (raw_qt_action_on_triggered action id)))

(def (qt-on-action-toggled! action handler)
  (let ((id (register-qt-bool-handler! handler)))
    (raw_qt_action_on_toggled action id)))

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

(def (qt-on-current-row-changed! list-widget handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_list_widget_on_current_row_changed list-widget id)))

(def (qt-on-item-double-clicked! list-widget handler)
  (let ((id (register-qt-int-handler! handler)))
    (raw_qt_list_widget_on_item_double_clicked list-widget id)))

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
    (raw_qt_table_widget_on_cell_clicked table id)))

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
    (raw_qt_tab_widget_on_current_changed tab-widget id)))

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
    (raw_qt_slider_on_value_changed slider id)))

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
    (raw_qt_timer_on_timeout timer id)))

(def (qt-timer-single-shot! msec handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_timer_single_shot msec id)))

(def (qt-timer-destroy! timer)
  (qt_timer_destroy timer))

;;; ---- Clipboard ----

(def (qt-clipboard-text app)
  (qt_clipboard_text app))

(def (qt-clipboard-set-text! app text)
  (qt_clipboard_set_text app text))

(def (qt-on-clipboard-changed! app handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_clipboard_on_changed app id)))

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
    (raw_qt_tree_widget_on_current_item_changed tree id)))

(def (qt-on-tree-item-double-clicked! tree handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_tree_widget_on_item_double_clicked tree id)))

(def (qt-on-item-expanded! tree handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_tree_widget_on_item_expanded tree id)))

(def (qt-on-item-collapsed! tree handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_tree_widget_on_item_collapsed tree id)))

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
    (raw_qt_widget_install_key_handler widget id)))

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
    (raw_qt_radio_button_on_toggled radio id)))

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
    (raw_qt_button_group_on_id_clicked bg id)))

(def (qt-button-group-destroy! bg)
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
    (raw_qt_group_box_on_toggled gb id)))

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
  (qt_font_destroy f))

(def (qt-widget-set-font! w font)
  (qt_widget_set_font w font))

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
    (raw_qt_stacked_widget_on_current_changed sw id)))

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
    (raw_qt_system_tray_icon_on_activated ti id)))

(def (qt-system-tray-available?)
  (not (= (qt_system_tray_icon_is_available) 0)))

(def (qt-system-tray-icon-destroy! ti)
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
  (qt_painter_rotate p angle))

(def (qt-painter-scale! p sx sy)
  (qt_painter_scale p sx sy))

;;; ---- Drag and Drop ----

(def (qt-widget-set-accept-drops! w accept)
  (qt_widget_set_accept_drops w (if accept 1 0)))

(def (qt-on-drop! widget handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_drop_filter_install widget id)))

(def (qt-drop-filter-last-text df)
  (qt_drop_filter_last_text df))

(def (qt-drop-filter-destroy! df)
  (qt_drop_filter_destroy df))

(def (qt-drag-text! source text)
  (qt_drag_text source text))

;;; ---- Double Spin Box ----

(def (qt-double-spin-box-create parent: (parent #f))
  (qt_double_spin_box_create parent))

(def (qt-double-spin-box-value dsb)
  (qt_double_spin_box_value dsb))

(def (qt-double-spin-box-set-value! dsb val)
  (qt_double_spin_box_set_value dsb val))

(def (qt-double-spin-box-set-range! dsb min max)
  (qt_double_spin_box_set_range dsb min max))

(def (qt-double-spin-box-set-single-step! dsb step)
  (qt_double_spin_box_set_single_step dsb step))

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
    (raw_qt_double_spin_box_on_value_changed dsb id)))

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
    (raw_qt_date_edit_on_date_changed d id)))

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
    (raw_qt_time_edit_on_time_changed t id)))

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
    (raw_qt_progress_dialog_on_canceled pd id)))

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
    (raw_qt_shortcut_on_activated s id)))

(def (qt-shortcut-destroy! s)
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

(def (qt-on-anchor-clicked! tb handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_text_browser_on_anchor_clicked tb id)))

;;; ---- Dialog Button Box ----

(def (qt-button-box-create buttons parent: (parent #f))
  (qt_button_box_create buttons parent))

(def (qt-button-box-button bb standard-button)
  (qt_button_box_button bb standard-button))

(def (qt-button-box-add-button! bb button role)
  (qt_button_box_add_button bb button role))

(def (qt-on-accepted! bb handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_button_box_on_accepted bb id)))

(def (qt-on-rejected! bb handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_button_box_on_rejected bb id)))

(def (qt-on-button-clicked! bb handler)
  (let ((id (register-qt-void-handler! handler)))
    (raw_qt_button_box_on_clicked bb id)))

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
    (raw_qt_calendar_on_selection_changed c id)))

(def (qt-on-calendar-clicked! c handler)
  (let ((id (register-qt-string-handler! handler)))
    (raw_qt_calendar_on_clicked c id)))
