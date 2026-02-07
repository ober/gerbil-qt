(import :std/test
        :std/srfi/13
        :gerbil-qt/qt
        :gerbil-qt/libqt)

(export qt-test)

;; All tests require a QApplication instance.
;; Use QT_QPA_PLATFORM=offscreen to run headless.
;; The app is created once and shared across all tests.
(def test-app #f)

(def (ensure-app!)
  (unless test-app
    (set! test-app (qt-app-create))))

(def qt-test
  (test-suite "gerbil-qt"

    ;; ==================== Phase 1: Core ====================

    (test-case "application create"
      (ensure-app!)
      (check (not (eq? test-app #f)) => #t))

    (test-case "main window create"
      (let ((win (qt-main-window-create)))
        (check (not (eq? win #f)) => #t)
        (qt-main-window-set-title! win "Test Window")
        (qt-widget-destroy! win)))

    (test-case "widget create"
      (let ((w (qt-widget-create)))
        (check (not (eq? w #f)) => #t)
        (qt-widget-destroy! w)))

    (test-case "widget enabled round-trip"
      (let ((w (qt-widget-create)))
        (check (qt-widget-enabled? w) => #t)
        (qt-widget-set-enabled! w #f)
        (check (qt-widget-enabled? w) => #f)
        (qt-widget-set-enabled! w #t)
        (check (qt-widget-enabled? w) => #t)
        (qt-widget-destroy! w)))

    (test-case "widget visible round-trip"
      (let ((w (qt-widget-create)))
        ;; Widget not yet shown, so isVisible returns false
        (check (qt-widget-visible? w) => #f)
        (qt-widget-set-visible! w #t)
        (check (qt-widget-visible? w) => #t)
        (qt-widget-set-visible! w #f)
        (check (qt-widget-visible? w) => #f)
        (qt-widget-destroy! w)))

    (test-case "layout create"
      (let* ((w (qt-widget-create))
             (vbox (qt-vbox-layout-create w))
             (w2 (qt-widget-create))
             (hbox (qt-hbox-layout-create w2)))
        (check (not (eq? vbox #f)) => #t)
        (check (not (eq? hbox #f)) => #t)
        (qt-widget-destroy! w)
        (qt-widget-destroy! w2)))

    (test-case "label text round-trip"
      (let ((l (qt-label-create "hello")))
        (check (qt-label-text l) => "hello")
        (qt-label-set-text! l "world")
        (check (qt-label-text l) => "world")
        (qt-widget-destroy! l)))

    (test-case "push button text round-trip"
      (let ((b (qt-push-button-create "Click")))
        (check (qt-push-button-text b) => "Click")
        (qt-push-button-set-text! b "Press")
        (check (qt-push-button-text b) => "Press")
        (qt-widget-destroy! b)))

    (test-case "button clicked signal registration"
      (let ((b (qt-push-button-create "Test")))
        ;; Just verify no crash — qt-on-clicked! returns void
        (qt-on-clicked! b (lambda () #t))
        (qt-widget-destroy! b)))

    ;; ==================== Phase 2: Core Widgets ====================

    (test-case "line edit text round-trip"
      (let ((e (qt-line-edit-create)))
        (qt-line-edit-set-text! e "test input")
        (check (qt-line-edit-text e) => "test input")
        (qt-line-edit-set-text! e "")
        (check (qt-line-edit-text e) => "")
        (qt-widget-destroy! e)))

    (test-case "line edit placeholder and read-only"
      (let ((e (qt-line-edit-create)))
        (qt-line-edit-set-placeholder! e "Enter text...")
        (qt-line-edit-set-read-only! e #t)
        (qt-widget-destroy! e)))

    (test-case "line edit echo mode"
      (let ((e (qt-line-edit-create)))
        (qt-line-edit-set-echo-mode! e QT_ECHO_PASSWORD)
        (qt-line-edit-set-echo-mode! e QT_ECHO_NORMAL)
        (qt-widget-destroy! e)))

    (test-case "line edit signal registration"
      (let ((e (qt-line-edit-create)))
        (qt-on-text-changed! e (lambda (text) #t))
        (qt-on-return-pressed! e (lambda () #t))
        (qt-widget-destroy! e)))

    (test-case "check box checked round-trip"
      (let ((c (qt-check-box-create "Option")))
        (check (qt-check-box-checked? c) => #f)
        (qt-check-box-set-checked! c #t)
        (check (qt-check-box-checked? c) => #t)
        (qt-check-box-set-checked! c #f)
        (check (qt-check-box-checked? c) => #f)
        (qt-widget-destroy! c)))

    (test-case "check box set text"
      (let ((c (qt-check-box-create "Before")))
        (qt-check-box-set-text! c "After")
        (qt-widget-destroy! c)))

    (test-case "check box signal registration"
      (let ((c (qt-check-box-create "Test")))
        (qt-on-toggled! c (lambda (checked) #t))
        (qt-widget-destroy! c)))

    (test-case "combo box add items and round-trip"
      (let ((c (qt-combo-box-create)))
        (check (qt-combo-box-count c) => 0)
        (qt-combo-box-add-item! c "Apple")
        (qt-combo-box-add-item! c "Banana")
        (qt-combo-box-add-item! c "Cherry")
        (check (qt-combo-box-count c) => 3)
        (check (qt-combo-box-current-index c) => 0)
        (check (qt-combo-box-current-text c) => "Apple")
        (qt-combo-box-set-current-index! c 2)
        (check (qt-combo-box-current-index c) => 2)
        (check (qt-combo-box-current-text c) => "Cherry")
        (qt-widget-destroy! c)))

    (test-case "combo box clear"
      (let ((c (qt-combo-box-create)))
        (qt-combo-box-add-item! c "X")
        (qt-combo-box-add-item! c "Y")
        (check (qt-combo-box-count c) => 2)
        (qt-combo-box-clear! c)
        (check (qt-combo-box-count c) => 0)
        (qt-widget-destroy! c)))

    (test-case "combo box signal registration"
      (let ((c (qt-combo-box-create)))
        (qt-on-index-changed! c (lambda (idx) #t))
        (qt-widget-destroy! c)))

    (test-case "text edit text round-trip"
      (let ((e (qt-text-edit-create)))
        (qt-text-edit-set-text! e "Hello World")
        (check (qt-text-edit-text e) => "Hello World")
        (qt-widget-destroy! e)))

    (test-case "text edit append and clear"
      (let ((e (qt-text-edit-create)))
        (qt-text-edit-set-text! e "Line 1")
        (qt-text-edit-append! e "Line 2")
        (let ((text (qt-text-edit-text e)))
          ;; toPlainText returns text with newlines
          (check (string-prefix? "Line 1" text) => #t))
        (qt-text-edit-clear! e)
        (check (qt-text-edit-text e) => "")
        (qt-widget-destroy! e)))

    (test-case "text edit signal registration"
      (let ((e (qt-text-edit-create)))
        (qt-on-text-edit-changed! e (lambda () #t))
        (qt-widget-destroy! e)))

    (test-case "spin box value round-trip"
      (let ((s (qt-spin-box-create)))
        (qt-spin-box-set-range! s 0 100)
        (qt-spin-box-set-value! s 42)
        (check (qt-spin-box-value s) => 42)
        (qt-spin-box-set-value! s 0)
        (check (qt-spin-box-value s) => 0)
        (qt-widget-destroy! s)))

    (test-case "spin box range clamping"
      (let ((s (qt-spin-box-create)))
        (qt-spin-box-set-range! s 10 50)
        (qt-spin-box-set-value! s 100)
        (check (qt-spin-box-value s) => 50)
        (qt-spin-box-set-value! s -5)
        (check (qt-spin-box-value s) => 10)
        (qt-widget-destroy! s)))

    (test-case "spin box prefix and suffix"
      (let ((s (qt-spin-box-create)))
        (qt-spin-box-set-prefix! s "$")
        (qt-spin-box-set-suffix! s " USD")
        (qt-widget-destroy! s)))

    (test-case "spin box signal registration"
      (let ((s (qt-spin-box-create)))
        (qt-on-value-changed! s (lambda (val) #t))
        (qt-widget-destroy! s)))

    (test-case "dialog create"
      (let ((d (qt-dialog-create)))
        (qt-dialog-set-title! d "Test Dialog")
        (qt-widget-destroy! d)))

    ;; ==================== Phase 2: Constants ====================

    (test-case "echo mode constants"
      (check QT_ECHO_NORMAL => 0)
      (check QT_ECHO_NO_ECHO => 1)
      (check QT_ECHO_PASSWORD => 2)
      (check QT_ECHO_PASSWORD_ON_EDIT => 3))

    ;; ==================== Phase 3: Menus, Actions, Toolbars ====================

    (test-case "menu bar from main window"
      (let* ((win (qt-main-window-create))
             (bar (qt-main-window-menu-bar win)))
        (check (not (eq? bar #f)) => #t)
        (qt-widget-destroy! win)))

    (test-case "menu and submenu creation"
      (let* ((win (qt-main-window-create))
             (bar (qt-main-window-menu-bar win))
             (file-menu (qt-menu-bar-add-menu bar "File"))
             (sub-menu (qt-menu-add-menu file-menu "Recent")))
        (check (not (eq? file-menu #f)) => #t)
        (check (not (eq? sub-menu #f)) => #t)
        (qt-widget-destroy! win)))

    (test-case "action create and text round-trip"
      (let ((a (qt-action-create "Open")))
        (check (qt-action-text a) => "Open")
        (qt-action-set-text! a "Close")
        (check (qt-action-text a) => "Close")))

    (test-case "action enabled round-trip"
      (let ((a (qt-action-create "Test")))
        (check (qt-action-enabled? a) => #t)
        (qt-action-set-enabled! a #f)
        (check (qt-action-enabled? a) => #f)
        (qt-action-set-enabled! a #t)
        (check (qt-action-enabled? a) => #t)))

    (test-case "action checkable round-trip"
      (let ((a (qt-action-create "Toggle")))
        (check (qt-action-checkable? a) => #f)
        (qt-action-set-checkable! a #t)
        (check (qt-action-checkable? a) => #t)
        (qt-action-set-checked! a #t)
        (check (qt-action-checked? a) => #t)
        (qt-action-set-checked! a #f)
        (check (qt-action-checked? a) => #f)))

    (test-case "action shortcut and tips"
      (let ((a (qt-action-create "Save")))
        (qt-action-set-shortcut! a "Ctrl+S")
        (qt-action-set-tooltip! a "Save file")
        (qt-action-set-status-tip! a "Save current file")))

    (test-case "action signal registration"
      (let ((a (qt-action-create "Act")))
        (qt-on-triggered! a (lambda () #t))
        (qt-action-set-checkable! a #t)
        (qt-on-action-toggled! a (lambda (checked) #t))))

    (test-case "menu add action and separator"
      (let* ((win (qt-main-window-create))
             (bar (qt-main-window-menu-bar win))
             (menu (qt-menu-bar-add-menu bar "Edit"))
             (act (qt-action-create "Cut")))
        (qt-menu-add-action! menu act)
        (qt-menu-add-separator! menu)
        (qt-widget-destroy! win)))

    (test-case "toolbar create and add action"
      (let* ((win (qt-main-window-create))
             (tb (qt-toolbar-create "Main")))
        (qt-main-window-add-toolbar! win tb)
        (let ((act (qt-action-create "Toolbar Action")))
          (qt-toolbar-add-action! tb act)
          (qt-toolbar-add-separator! tb))
        (qt-toolbar-set-movable! tb #f)
        (qt-toolbar-set-icon-size! tb 24 24)
        (qt-widget-destroy! win)))

    (test-case "toolbar add widget"
      (let* ((win (qt-main-window-create))
             (tb (qt-toolbar-create "Tools"))
             (btn (qt-push-button-create "TB Button")))
        (qt-main-window-add-toolbar! win tb)
        (qt-toolbar-add-widget! tb btn)
        (qt-widget-destroy! win)))

    (test-case "status bar text"
      (let ((win (qt-main-window-create)))
        (qt-main-window-set-status-bar-text! win "Ready")
        (qt-widget-destroy! win)))

    ;; ==================== Phase 4: Advanced Widgets ====================

    ;; --- List Widget ---

    (test-case "list widget add and count"
      (let ((l (qt-list-widget-create)))
        (check (qt-list-widget-count l) => 0)
        (qt-list-widget-add-item! l "Item 1")
        (qt-list-widget-add-item! l "Item 2")
        (qt-list-widget-add-item! l "Item 3")
        (check (qt-list-widget-count l) => 3)
        (qt-widget-destroy! l)))

    (test-case "list widget item text"
      (let ((l (qt-list-widget-create)))
        (qt-list-widget-add-item! l "Alpha")
        (qt-list-widget-add-item! l "Beta")
        (qt-list-widget-add-item! l "Gamma")
        (check (qt-list-widget-item-text l 0) => "Alpha")
        (check (qt-list-widget-item-text l 1) => "Beta")
        (check (qt-list-widget-item-text l 2) => "Gamma")
        (qt-widget-destroy! l)))

    (test-case "list widget insert item"
      (let ((l (qt-list-widget-create)))
        (qt-list-widget-add-item! l "First")
        (qt-list-widget-add-item! l "Third")
        (qt-list-widget-insert-item! l 1 "Second")
        (check (qt-list-widget-count l) => 3)
        (check (qt-list-widget-item-text l 0) => "First")
        (check (qt-list-widget-item-text l 1) => "Second")
        (check (qt-list-widget-item-text l 2) => "Third")
        (qt-widget-destroy! l)))

    (test-case "list widget remove item"
      (let ((l (qt-list-widget-create)))
        (qt-list-widget-add-item! l "A")
        (qt-list-widget-add-item! l "B")
        (qt-list-widget-add-item! l "C")
        (qt-list-widget-remove-item! l 1)
        (check (qt-list-widget-count l) => 2)
        (check (qt-list-widget-item-text l 0) => "A")
        (check (qt-list-widget-item-text l 1) => "C")
        (qt-widget-destroy! l)))

    (test-case "list widget current row"
      (let ((l (qt-list-widget-create)))
        (qt-list-widget-add-item! l "X")
        (qt-list-widget-add-item! l "Y")
        (qt-list-widget-set-current-row! l 1)
        (check (qt-list-widget-current-row l) => 1)
        (qt-list-widget-set-current-row! l 0)
        (check (qt-list-widget-current-row l) => 0)
        (qt-widget-destroy! l)))

    (test-case "list widget clear"
      (let ((l (qt-list-widget-create)))
        (qt-list-widget-add-item! l "A")
        (qt-list-widget-add-item! l "B")
        (check (qt-list-widget-count l) => 2)
        (qt-list-widget-clear! l)
        (check (qt-list-widget-count l) => 0)
        (qt-widget-destroy! l)))

    (test-case "list widget signal registration"
      (let ((l (qt-list-widget-create)))
        (qt-on-current-row-changed! l (lambda (row) #t))
        (qt-on-item-double-clicked! l (lambda (row) #t))
        (qt-widget-destroy! l)))

    ;; --- Table Widget ---

    (test-case "table widget create with dimensions"
      (let ((t (qt-table-widget-create 3 4)))
        (check (qt-table-widget-row-count t) => 3)
        (check (qt-table-widget-column-count t) => 4)
        (qt-widget-destroy! t)))

    (test-case "table widget set/get item text"
      (let ((t (qt-table-widget-create 2 2)))
        (qt-table-widget-set-item! t 0 0 "A1")
        (qt-table-widget-set-item! t 0 1 "B1")
        (qt-table-widget-set-item! t 1 0 "A2")
        (qt-table-widget-set-item! t 1 1 "B2")
        (check (qt-table-widget-item-text t 0 0) => "A1")
        (check (qt-table-widget-item-text t 0 1) => "B1")
        (check (qt-table-widget-item-text t 1 0) => "A2")
        (check (qt-table-widget-item-text t 1 1) => "B2")
        (qt-widget-destroy! t)))

    (test-case "table widget empty cell returns empty string"
      (let ((t (qt-table-widget-create 2 2)))
        (check (qt-table-widget-item-text t 0 0) => "")
        (qt-widget-destroy! t)))

    (test-case "table widget resize row/column counts"
      (let ((t (qt-table-widget-create 2 2)))
        (qt-table-widget-set-row-count! t 5)
        (qt-table-widget-set-column-count! t 3)
        (check (qt-table-widget-row-count t) => 5)
        (check (qt-table-widget-column-count t) => 3)
        (qt-widget-destroy! t)))

    (test-case "table widget header items"
      (let ((t (qt-table-widget-create 2 3)))
        (qt-table-widget-set-horizontal-header! t 0 "Name")
        (qt-table-widget-set-horizontal-header! t 1 "Age")
        (qt-table-widget-set-horizontal-header! t 2 "City")
        (qt-table-widget-set-vertical-header! t 0 "Row 1")
        (qt-table-widget-set-vertical-header! t 1 "Row 2")
        (qt-widget-destroy! t)))

    (test-case "table widget clear"
      (let ((t (qt-table-widget-create 2 2)))
        (qt-table-widget-set-item! t 0 0 "data")
        (qt-table-widget-clear! t)
        (check (qt-table-widget-item-text t 0 0) => "")
        (qt-widget-destroy! t)))

    (test-case "table widget signal registration"
      (let ((t (qt-table-widget-create 2 2)))
        (qt-on-cell-clicked! t (lambda () #t))
        (qt-widget-destroy! t)))

    ;; --- Tab Widget ---

    (test-case "tab widget add tabs"
      (let* ((tw (qt-tab-widget-create))
             (page1 (qt-widget-create))
             (page2 (qt-widget-create)))
        (check (qt-tab-widget-count tw) => 0)
        (qt-tab-widget-add-tab! tw page1 "Tab 1")
        (qt-tab-widget-add-tab! tw page2 "Tab 2")
        (check (qt-tab-widget-count tw) => 2)
        (qt-widget-destroy! tw)))

    (test-case "tab widget current index"
      (let* ((tw (qt-tab-widget-create))
             (p1 (qt-widget-create))
             (p2 (qt-widget-create))
             (p3 (qt-widget-create)))
        (qt-tab-widget-add-tab! tw p1 "A")
        (qt-tab-widget-add-tab! tw p2 "B")
        (qt-tab-widget-add-tab! tw p3 "C")
        (check (qt-tab-widget-current-index tw) => 0)
        (qt-tab-widget-set-current-index! tw 2)
        (check (qt-tab-widget-current-index tw) => 2)
        (qt-tab-widget-set-current-index! tw 1)
        (check (qt-tab-widget-current-index tw) => 1)
        (qt-widget-destroy! tw)))

    (test-case "tab widget set tab text"
      (let* ((tw (qt-tab-widget-create))
             (p (qt-widget-create)))
        (qt-tab-widget-add-tab! tw p "Original")
        (qt-tab-widget-set-tab-text! tw 0 "Renamed")
        (qt-widget-destroy! tw)))

    (test-case "tab widget signal registration"
      (let ((tw (qt-tab-widget-create)))
        (qt-on-tab-changed! tw (lambda (idx) #t))
        (qt-widget-destroy! tw)))

    ;; --- Progress Bar ---

    (test-case "progress bar value round-trip"
      (let ((p (qt-progress-bar-create)))
        (qt-progress-bar-set-range! p 0 100)
        (qt-progress-bar-set-value! p 50)
        (check (qt-progress-bar-value p) => 50)
        (qt-progress-bar-set-value! p 0)
        (check (qt-progress-bar-value p) => 0)
        (qt-progress-bar-set-value! p 100)
        (check (qt-progress-bar-value p) => 100)
        (qt-widget-destroy! p)))

    (test-case "progress bar custom range"
      (let ((p (qt-progress-bar-create)))
        (qt-progress-bar-set-range! p 10 20)
        (qt-progress-bar-set-value! p 15)
        (check (qt-progress-bar-value p) => 15)
        (qt-widget-destroy! p)))

    (test-case "progress bar format"
      (let ((p (qt-progress-bar-create)))
        (qt-progress-bar-set-format! p "%v / %m")
        (qt-widget-destroy! p)))

    ;; --- Slider ---

    (test-case "slider value round-trip"
      (let ((s (qt-slider-create QT_HORIZONTAL)))
        (qt-slider-set-range! s 0 100)
        (qt-slider-set-value! s 75)
        (check (qt-slider-value s) => 75)
        (qt-slider-set-value! s 0)
        (check (qt-slider-value s) => 0)
        (qt-widget-destroy! s)))

    (test-case "slider vertical orientation"
      (let ((s (qt-slider-create QT_VERTICAL)))
        (qt-slider-set-range! s 0 50)
        (qt-slider-set-value! s 25)
        (check (qt-slider-value s) => 25)
        (qt-widget-destroy! s)))

    (test-case "slider tick configuration"
      (let ((s (qt-slider-create QT_HORIZONTAL)))
        (qt-slider-set-range! s 0 100)
        (qt-slider-set-single-step! s 5)
        (qt-slider-set-tick-interval! s 10)
        (qt-slider-set-tick-position! s QT_TICKS_BELOW)
        (qt-widget-destroy! s)))

    (test-case "slider signal registration"
      (let ((s (qt-slider-create QT_HORIZONTAL)))
        (qt-on-slider-value-changed! s (lambda (val) #t))
        (qt-widget-destroy! s)))

    ;; ==================== Constants ====================

    (test-case "alignment constants"
      (check QT_ALIGN_LEFT   => #x0001)
      (check QT_ALIGN_RIGHT  => #x0002)
      (check QT_ALIGN_CENTER => #x0084)
      (check QT_ALIGN_TOP    => #x0020)
      (check QT_ALIGN_BOTTOM => #x0040))

    (test-case "orientation constants"
      (check QT_HORIZONTAL => #x1)
      (check QT_VERTICAL   => #x2))

    (test-case "tick position constants"
      (check QT_TICKS_NONE       => 0)
      (check QT_TICKS_ABOVE      => 1)
      (check QT_TICKS_BELOW      => 2)
      (check QT_TICKS_BOTH_SIDES => 3))

    ;; ==================== Phase 5: Grid Layout, Timer, Clipboard, Tree ====================

    ;; --- Grid Layout ---

    (test-case "grid layout create"
      (let* ((w (qt-widget-create))
             (grid (qt-grid-layout-create w)))
        (check (not (eq? grid #f)) => #t)
        (qt-widget-destroy! w)))

    (test-case "grid layout add widget"
      (let* ((w (qt-widget-create))
             (grid (qt-grid-layout-create w))
             (l1 (qt-label-create "A"))
             (l2 (qt-label-create "B"))
             (l3 (qt-label-create "C")))
        (qt-grid-layout-add-widget! grid l1 0 0)
        (qt-grid-layout-add-widget! grid l2 0 1)
        (qt-grid-layout-add-widget! grid l3 1 0 col-span: 2)
        (qt-widget-destroy! w)))

    (test-case "grid layout stretch"
      (let* ((w (qt-widget-create))
             (grid (qt-grid-layout-create w)))
        (qt-grid-layout-set-row-stretch! grid 0 1)
        (qt-grid-layout-set-column-stretch! grid 0 2)
        (qt-widget-destroy! w)))

    (test-case "grid layout minimum sizes"
      (let* ((w (qt-widget-create))
             (grid (qt-grid-layout-create w)))
        (qt-grid-layout-set-row-minimum-height! grid 0 50)
        (qt-grid-layout-set-column-minimum-width! grid 0 100)
        (qt-widget-destroy! w)))

    (test-case "grid layout with spacing and margins"
      (let* ((w (qt-widget-create))
             (grid (qt-grid-layout-create w)))
        (qt-layout-set-spacing! grid 10)
        (qt-layout-set-margins! grid 5 5 5 5)
        (qt-widget-destroy! w)))

    ;; --- Timer ---

    (test-case "timer create and destroy"
      (let ((t (qt-timer-create)))
        (check (not (eq? t #f)) => #t)
        (qt-timer-destroy! t)))

    (test-case "timer start and stop"
      (let ((t (qt-timer-create)))
        (check (qt-timer-active? t) => #f)
        (qt-timer-start! t 1000)
        (check (qt-timer-active? t) => #t)
        (qt-timer-stop! t)
        (check (qt-timer-active? t) => #f)
        (qt-timer-destroy! t)))

    (test-case "timer interval"
      (let ((t (qt-timer-create)))
        (qt-timer-set-interval! t 500)
        (check (qt-timer-interval t) => 500)
        (qt-timer-set-interval! t 100)
        (check (qt-timer-interval t) => 100)
        (qt-timer-destroy! t)))

    (test-case "timer single shot"
      (let ((t (qt-timer-create)))
        (qt-timer-set-single-shot! t #t)
        (qt-timer-destroy! t)))

    (test-case "timer signal registration"
      (let ((t (qt-timer-create)))
        (qt-on-timeout! t (lambda () #t))
        (qt-timer-destroy! t)))

    (test-case "timer single shot convenience"
      ;; Just verify no crash
      (qt-timer-single-shot! 10000 (lambda () #t)))

    ;; --- Clipboard ---

    (test-case "clipboard set and get text"
      (qt-clipboard-set-text! test-app "hello clipboard")
      (check (qt-clipboard-text test-app) => "hello clipboard"))

    (test-case "clipboard empty text"
      (qt-clipboard-set-text! test-app "")
      (check (qt-clipboard-text test-app) => ""))

    (test-case "clipboard signal registration"
      (qt-on-clipboard-changed! test-app (lambda () #t)))

    ;; --- Tree Widget ---

    (test-case "tree widget create"
      (let ((t (qt-tree-widget-create)))
        (check (not (eq? t #f)) => #t)
        (qt-widget-destroy! t)))

    (test-case "tree widget column count"
      (let ((t (qt-tree-widget-create)))
        (qt-tree-widget-set-column-count! t 3)
        (check (qt-tree-widget-column-count t) => 3)
        (qt-widget-destroy! t)))

    (test-case "tree widget header labels"
      (let ((t (qt-tree-widget-create)))
        (qt-tree-widget-set-header-labels! t '("Name" "Size" "Type"))
        (check (qt-tree-widget-column-count t) => 3)
        (qt-widget-destroy! t)))

    (test-case "tree widget add top level items"
      (let ((t (qt-tree-widget-create))
            (item1 (qt-tree-item-create "First"))
            (item2 (qt-tree-item-create "Second")))
        (qt-tree-widget-add-top-level-item! t item1)
        (qt-tree-widget-add-top-level-item! t item2)
        (check (qt-tree-widget-top-level-item-count t) => 2)
        (qt-widget-destroy! t)))

    (test-case "tree widget top level item retrieval"
      (let* ((t (qt-tree-widget-create))
             (item (qt-tree-item-create "Test")))
        (qt-tree-widget-add-top-level-item! t item)
        (let ((retrieved (qt-tree-widget-top-level-item t 0)))
          (check (not (eq? retrieved #f)) => #t)
          (check (qt-tree-item-text retrieved) => "Test"))
        (qt-widget-destroy! t)))

    (test-case "tree item text round-trip"
      (let ((item (qt-tree-item-create "Hello")))
        (check (qt-tree-item-text item) => "Hello")
        (qt-tree-item-set-text! item "World")
        (check (qt-tree-item-text item) => "World")))

    (test-case "tree item multi-column"
      (let* ((t (qt-tree-widget-create))
             (item (qt-tree-item-create "Name")))
        (qt-tree-widget-set-column-count! t 3)
        (qt-tree-item-set-text! item "Size" column: 1)
        (qt-tree-item-set-text! item "Type" column: 2)
        (qt-tree-widget-add-top-level-item! t item)
        (check (qt-tree-item-text item column: 0) => "Name")
        (check (qt-tree-item-text item column: 1) => "Size")
        (check (qt-tree-item-text item column: 2) => "Type")
        (qt-widget-destroy! t)))

    (test-case "tree item children"
      (let* ((t (qt-tree-widget-create))
             (parent (qt-tree-item-create "Parent"))
             (child1 (qt-tree-item-create "Child 1"))
             (child2 (qt-tree-item-create "Child 2")))
        (qt-tree-item-add-child! parent child1)
        (qt-tree-item-add-child! parent child2)
        (qt-tree-widget-add-top-level-item! t parent)
        (check (qt-tree-item-child-count parent) => 2)
        (check (qt-tree-item-text (qt-tree-item-child parent 0)) => "Child 1")
        (check (qt-tree-item-text (qt-tree-item-child parent 1)) => "Child 2")
        (qt-widget-destroy! t)))

    (test-case "tree item parent navigation"
      (let* ((t (qt-tree-widget-create))
             (root (qt-tree-item-create "Root"))
             (child (qt-tree-item-create "Child")))
        (qt-tree-item-add-child! root child)
        (qt-tree-widget-add-top-level-item! t root)
        (let ((p (qt-tree-item-parent child)))
          (check (not (eq? p #f)) => #t)
          (check (qt-tree-item-text p) => "Root"))
        (qt-widget-destroy! t)))

    (test-case "tree widget expand and collapse"
      (let* ((t (qt-tree-widget-create))
             (root (qt-tree-item-create "Root"))
             (child (qt-tree-item-create "Child")))
        (qt-tree-item-add-child! root child)
        (qt-tree-widget-add-top-level-item! t root)
        (qt-tree-widget-expand-all! t)
        (qt-tree-widget-collapse-all! t)
        (qt-tree-widget-expand-item! t root)
        (qt-tree-widget-collapse-item! t root)
        (qt-widget-destroy! t)))

    (test-case "tree widget clear"
      (let* ((t (qt-tree-widget-create))
             (item (qt-tree-item-create "Temp")))
        (qt-tree-widget-add-top-level-item! t item)
        (check (qt-tree-widget-top-level-item-count t) => 1)
        (qt-tree-widget-clear! t)
        (check (qt-tree-widget-top-level-item-count t) => 0)
        (qt-widget-destroy! t)))

    (test-case "tree widget signal registration"
      (let ((t (qt-tree-widget-create)))
        (qt-on-current-item-changed! t (lambda () #t))
        (qt-on-tree-item-double-clicked! t (lambda () #t))
        (qt-on-item-expanded! t (lambda () #t))
        (qt-on-item-collapsed! t (lambda () #t))
        (qt-widget-destroy! t)))

    ;; --- Event Loop Integration ---

    (test-case "start-timer creates active timer"
      (let ((timer (qt-start-timer! 1000 (lambda () #t))))
        (check (qt-timer-active? timer) => #t)
        (qt-timer-stop! timer)
        (qt-timer-destroy! timer)))

    ;; ==================== Phase 6: Style Sheets, Window State, ScrollArea, Splitter, Keys ====================

    ;; --- App-wide Style Sheet ---

    (test-case "app-wide style sheet"
      (qt-app-set-style-sheet! test-app "QLabel { color: red; }")
      ;; Reset to empty
      (qt-app-set-style-sheet! test-app ""))

    ;; --- Window State Management ---

    (test-case "widget geometry getters"
      (let ((w (qt-widget-create)))
        (qt-widget-resize! w 300 200)
        ;; width/height should reflect the resize
        (check (qt-widget-width w) => 300)
        (check (qt-widget-height w) => 200)
        (qt-widget-destroy! w)))

    (test-case "widget move and position"
      (let ((w (qt-widget-create)))
        (qt-widget-move! w 100 200)
        (check (qt-widget-x w) => 100)
        (check (qt-widget-y w) => 200)
        (qt-widget-destroy! w)))

    (test-case "window state constants"
      (check QT_WINDOW_NO_STATE    => #x00)
      (check QT_WINDOW_MINIMIZED   => #x01)
      (check QT_WINDOW_MAXIMIZED   => #x02)
      (check QT_WINDOW_FULL_SCREEN => #x04))

    (test-case "window state query"
      (let ((w (qt-widget-create)))
        ;; Default state should be no-state
        (check (qt-widget-window-state w) => QT_WINDOW_NO_STATE)
        (qt-widget-destroy! w)))

    (test-case "show normal"
      (let ((w (qt-widget-create)))
        (qt-widget-show-normal! w)
        (qt-widget-destroy! w)))

    ;; --- Scroll Area ---

    (test-case "scroll area create"
      (let ((sa (qt-scroll-area-create)))
        (check (not (eq? sa #f)) => #t)
        (qt-widget-destroy! sa)))

    (test-case "scroll area set widget"
      (let ((sa (qt-scroll-area-create))
            (content (qt-widget-create)))
        (qt-scroll-area-set-widget! sa content)
        (qt-scroll-area-set-widget-resizable! sa #t)
        (qt-widget-destroy! sa)))

    (test-case "scroll area scrollbar policies"
      (let ((sa (qt-scroll-area-create)))
        (qt-scroll-area-set-horizontal-scrollbar-policy! sa QT_SCROLLBAR_ALWAYS_OFF)
        (qt-scroll-area-set-vertical-scrollbar-policy! sa QT_SCROLLBAR_ALWAYS_ON)
        (qt-widget-destroy! sa)))

    (test-case "scrollbar policy constants"
      (check QT_SCROLLBAR_AS_NEEDED  => 0)
      (check QT_SCROLLBAR_ALWAYS_OFF => 1)
      (check QT_SCROLLBAR_ALWAYS_ON  => 2))

    ;; --- Splitter ---

    (test-case "splitter create and add widgets"
      (let ((sp (qt-splitter-create QT_HORIZONTAL))
            (w1 (qt-widget-create))
            (w2 (qt-widget-create)))
        (qt-splitter-add-widget! sp w1)
        (qt-splitter-add-widget! sp w2)
        (check (qt-splitter-count sp) => 2)
        (qt-widget-destroy! sp)))

    (test-case "splitter set sizes 2-pane"
      (let ((sp (qt-splitter-create QT_HORIZONTAL))
            (w1 (qt-widget-create))
            (w2 (qt-widget-create)))
        (qt-splitter-add-widget! sp w1)
        (qt-splitter-add-widget! sp w2)
        (qt-splitter-set-sizes! sp '(200 300))
        (qt-widget-destroy! sp)))

    (test-case "splitter 3-pane"
      (let ((sp (qt-splitter-create QT_VERTICAL))
            (w1 (qt-widget-create))
            (w2 (qt-widget-create))
            (w3 (qt-widget-create)))
        (qt-splitter-add-widget! sp w1)
        (qt-splitter-add-widget! sp w2)
        (qt-splitter-add-widget! sp w3)
        (check (qt-splitter-count sp) => 3)
        (qt-splitter-set-sizes! sp '(100 200 100))
        (qt-widget-destroy! sp)))

    (test-case "splitter stretch factor and handle width"
      (let ((sp (qt-splitter-create QT_HORIZONTAL))
            (w1 (qt-widget-create))
            (w2 (qt-widget-create)))
        (qt-splitter-add-widget! sp w1)
        (qt-splitter-add-widget! sp w2)
        (qt-splitter-set-stretch-factor! sp 0 1)
        (qt-splitter-set-stretch-factor! sp 1 2)
        (qt-splitter-set-handle-width! sp 8)
        (qt-widget-destroy! sp)))

    (test-case "splitter collapsible"
      (let ((sp (qt-splitter-create QT_HORIZONTAL))
            (w1 (qt-widget-create))
            (w2 (qt-widget-create)))
        (qt-splitter-add-widget! sp w1)
        (qt-splitter-add-widget! sp w2)
        (qt-splitter-set-collapsible! sp 0 #f)
        (check (qt-splitter-collapsible? sp 0) => #f)
        (qt-splitter-set-collapsible! sp 0 #t)
        (check (qt-splitter-collapsible? sp 0) => #t)
        (qt-widget-destroy! sp)))

    ;; --- Keyboard Events ---

    (test-case "key handler registration"
      (let ((w (qt-widget-create)))
        (qt-on-key-press! w (lambda () #t))
        (qt-widget-destroy! w)))

    (test-case "key query functions"
      ;; Just verify no crash (no key pressed yet, returns defaults)
      (check (qt-last-key-code) => 0)
      (check (qt-last-key-modifiers) => 0)
      (check (qt-last-key-text) => ""))

    (test-case "key letter constants"
      (check QT_KEY_A => #x41)
      (check QT_KEY_Z => #x5a))

    (test-case "key digit constants"
      (check QT_KEY_0 => #x30)
      (check QT_KEY_9 => #x39))

    (test-case "key special constants"
      (check QT_KEY_ESCAPE    => #x01000000)
      (check QT_KEY_RETURN    => #x01000004)
      (check QT_KEY_SPACE     => #x20)
      (check QT_KEY_F1        => #x01000030)
      (check QT_KEY_F11       => #x0100003a)
      (check QT_KEY_F12       => #x0100003b)
      (check QT_KEY_UP        => #x01000013)
      (check QT_KEY_DOWN      => #x01000015)
      (check QT_KEY_LEFT      => #x01000012)
      (check QT_KEY_RIGHT     => #x01000014))

    (test-case "modifier constants"
      (check QT_MOD_NONE  => #x00000000)
      (check QT_MOD_SHIFT => #x02000000)
      (check QT_MOD_CTRL  => #x04000000)
      (check QT_MOD_ALT   => #x08000000)
      (check QT_MOD_META  => #x10000000))

    ;; ==================== Phase 7: Images, Icons, Radio Buttons, GroupBox ====================

    ;; --- Pixmap ---

    (test-case "pixmap load nonexistent returns null"
      (let ((pm (qt-pixmap-load "/nonexistent/path.png")))
        (check (qt-pixmap-null? pm) => #t)
        (check (qt-pixmap-width pm) => 0)
        (check (qt-pixmap-height pm) => 0)
        (qt-pixmap-destroy! pm)))

    (test-case "pixmap scaled from null"
      (let* ((pm (qt-pixmap-load "/nonexistent.png"))
             (scaled (qt-pixmap-scaled pm 100 100)))
        (check (qt-pixmap-null? scaled) => #t)
        (qt-pixmap-destroy! scaled)
        (qt-pixmap-destroy! pm)))

    (test-case "label set pixmap"
      (let ((l (qt-label-create ""))
            (pm (qt-pixmap-load "/nonexistent.png")))
        (qt-label-set-pixmap! l pm)
        (qt-pixmap-destroy! pm)
        (qt-widget-destroy! l)))

    ;; --- Icon ---

    (test-case "icon create from path"
      (let ((icon (qt-icon-create "/nonexistent/icon.png")))
        ;; QIcon is lazy — isNull() only checks if no filename was set,
        ;; not whether the file exists. A path was set, so not null.
        (check (not (eq? icon #f)) => #t)
        (qt-icon-destroy! icon)))

    (test-case "icon create from pixmap"
      (let* ((pm (qt-pixmap-load "/nonexistent.png"))
             (icon (qt-icon-create-from-pixmap pm)))
        (check (qt-icon-null? icon) => #t)
        (qt-icon-destroy! icon)
        (qt-pixmap-destroy! pm)))

    (test-case "button set icon"
      (let ((btn (qt-push-button-create "OK"))
            (icon (qt-icon-create "/nonexistent.png")))
        (qt-push-button-set-icon! btn icon)
        (qt-icon-destroy! icon)
        (qt-widget-destroy! btn)))

    (test-case "action set icon"
      (let ((act (qt-action-create "Save"))
            (icon (qt-icon-create "/nonexistent.png")))
        (qt-action-set-icon! act icon)
        (qt-icon-destroy! icon)))

    (test-case "widget set window icon"
      (let ((w (qt-widget-create))
            (icon (qt-icon-create "/nonexistent.png")))
        (qt-widget-set-window-icon! w icon)
        (qt-icon-destroy! icon)
        (qt-widget-destroy! w)))

    ;; --- Radio Button ---

    (test-case "radio button create and text"
      (let ((r (qt-radio-button-create "Option A")))
        (check (qt-radio-button-text r) => "Option A")
        (qt-radio-button-set-text! r "Option B")
        (check (qt-radio-button-text r) => "Option B")
        (qt-widget-destroy! r)))

    (test-case "radio button checked round-trip"
      ;; A standalone radio button can be checked but not unchecked
      ;; (Qt radio behavior: unchecking requires another in the group).
      ;; Test with a button group for proper exclusivity.
      (let ((bg (qt-button-group-create))
            (r1 (qt-radio-button-create "A"))
            (r2 (qt-radio-button-create "B")))
        (qt-button-group-add-button! bg r1 0)
        (qt-button-group-add-button! bg r2 1)
        (check (qt-radio-button-checked? r1) => #f)
        (check (qt-radio-button-checked? r2) => #f)
        (qt-radio-button-set-checked! r1 #t)
        (check (qt-radio-button-checked? r1) => #t)
        ;; Checking r2 unchecks r1 (exclusive group)
        (qt-radio-button-set-checked! r2 #t)
        (check (qt-radio-button-checked? r1) => #f)
        (check (qt-radio-button-checked? r2) => #t)
        (qt-button-group-destroy! bg)
        (qt-widget-destroy! r1)
        (qt-widget-destroy! r2)))

    (test-case "radio button signal registration"
      (let ((r (qt-radio-button-create "Toggle")))
        (qt-on-radio-toggled! r (lambda (checked) #t))
        (qt-widget-destroy! r)))

    ;; --- Button Group ---

    (test-case "button group create and destroy"
      (let ((bg (qt-button-group-create)))
        (check (not (eq? bg #f)) => #t)
        (qt-button-group-destroy! bg)))

    (test-case "button group exclusive round-trip"
      (let ((bg (qt-button-group-create)))
        (check (qt-button-group-exclusive? bg) => #t)
        (qt-button-group-set-exclusive! bg #f)
        (check (qt-button-group-exclusive? bg) => #f)
        (qt-button-group-set-exclusive! bg #t)
        (check (qt-button-group-exclusive? bg) => #t)
        (qt-button-group-destroy! bg)))

    (test-case "button group add radio buttons"
      (let ((bg (qt-button-group-create))
            (r1 (qt-radio-button-create "A"))
            (r2 (qt-radio-button-create "B"))
            (r3 (qt-radio-button-create "C")))
        (qt-button-group-add-button! bg r1 0)
        (qt-button-group-add-button! bg r2 1)
        (qt-button-group-add-button! bg r3 2)
        ;; No button checked yet
        (check (qt-button-group-checked-id bg) => -1)
        ;; Check one
        (qt-radio-button-set-checked! r2 #t)
        (check (qt-button-group-checked-id bg) => 1)
        ;; Exclusivity: checking another unchecks previous
        (qt-radio-button-set-checked! r3 #t)
        (check (qt-button-group-checked-id bg) => 2)
        (check (qt-radio-button-checked? r2) => #f)
        (qt-button-group-destroy! bg)
        (qt-widget-destroy! r1)
        (qt-widget-destroy! r2)
        (qt-widget-destroy! r3)))

    (test-case "button group remove button"
      (let ((bg (qt-button-group-create))
            (r1 (qt-radio-button-create "X"))
            (r2 (qt-radio-button-create "Y")))
        (qt-button-group-add-button! bg r1 0)
        (qt-button-group-add-button! bg r2 1)
        (qt-button-group-remove-button! bg r1)
        ;; r1 no longer in group; check r2
        (qt-radio-button-set-checked! r2 #t)
        (check (qt-button-group-checked-id bg) => 1)
        (qt-button-group-destroy! bg)
        (qt-widget-destroy! r1)
        (qt-widget-destroy! r2)))

    (test-case "button group signal registration"
      (let ((bg (qt-button-group-create)))
        (qt-on-button-group-clicked! bg (lambda (id) #t))
        (qt-button-group-destroy! bg)))

    ;; --- Group Box ---

    (test-case "group box create and title"
      (let ((gb (qt-group-box-create "Settings")))
        (check (qt-group-box-title gb) => "Settings")
        (qt-group-box-set-title! gb "Options")
        (check (qt-group-box-title gb) => "Options")
        (qt-widget-destroy! gb)))

    (test-case "group box checkable round-trip"
      (let ((gb (qt-group-box-create "Features")))
        (check (qt-group-box-checkable? gb) => #f)
        (qt-group-box-set-checkable! gb #t)
        (check (qt-group-box-checkable? gb) => #t)
        (qt-group-box-set-checked! gb #f)
        (check (qt-group-box-checked? gb) => #f)
        (qt-group-box-set-checked! gb #t)
        (check (qt-group-box-checked? gb) => #t)
        (qt-widget-destroy! gb)))

    (test-case "group box with layout and radio buttons"
      (let* ((gb (qt-group-box-create "Choose"))
             (layout (qt-vbox-layout-create gb))
             (r1 (qt-radio-button-create "Option 1"))
             (r2 (qt-radio-button-create "Option 2")))
        (qt-layout-add-widget! layout r1)
        (qt-layout-add-widget! layout r2)
        (qt-radio-button-set-checked! r1 #t)
        (check (qt-radio-button-checked? r1) => #t)
        (qt-widget-destroy! gb)))

    (test-case "group box signal registration"
      (let ((gb (qt-group-box-create "Toggleable")))
        (qt-group-box-set-checkable! gb #t)
        (qt-on-group-box-toggled! gb (lambda (checked) #t))
        (qt-widget-destroy! gb)))

    ;; ==================== Phase 8a: Font ====================

    (test-case "font create and query family"
      (ensure-app!)
      (let ((f (qt-font-create "Monospace" point-size: 14)))
        (check (qt-font-point-size f) => 14)
        (qt-font-destroy! f)))

    (test-case "font bold and italic"
      (ensure-app!)
      (let ((f (qt-font-create "Sans")))
        (check (qt-font-bold? f) => #f)
        (check (qt-font-italic? f) => #f)
        (qt-font-set-bold! f #t)
        (check (qt-font-bold? f) => #t)
        (qt-font-set-italic! f #t)
        (check (qt-font-italic? f) => #t)
        (qt-font-destroy! f)))

    (test-case "widget set/get font"
      (ensure-app!)
      (let* ((label (qt-label-create "Test"))
             (f (qt-font-create "Monospace" point-size: 18)))
        (qt-font-set-bold! f #t)
        (qt-widget-set-font! label f)
        (let ((f2 (qt-widget-font label)))
          (check (qt-font-point-size f2) => 18)
          (check (qt-font-bold? f2) => #t)
          (qt-font-destroy! f2))
        (qt-font-destroy! f)
        (qt-widget-destroy! label)))

    ;; ==================== Phase 8a: Color ====================

    (test-case "color create RGB and query channels"
      (ensure-app!)
      (let ((c (qt-color-create 255 128 0)))
        (check (qt-color-red c) => 255)
        (check (qt-color-green c) => 128)
        (check (qt-color-blue c) => 0)
        (check (qt-color-alpha c) => 255)
        (check (qt-color-valid? c) => #t)
        (qt-color-destroy! c)))

    (test-case "color create with alpha"
      (ensure-app!)
      (let ((c (qt-color-create 100 200 50 alpha: 128)))
        (check (qt-color-red c) => 100)
        (check (qt-color-green c) => 200)
        (check (qt-color-blue c) => 50)
        (check (qt-color-alpha c) => 128)
        (qt-color-destroy! c)))

    (test-case "color create from name"
      (ensure-app!)
      (let ((c (qt-color-create-name "#ff0000")))
        (check (qt-color-red c) => 255)
        (check (qt-color-green c) => 0)
        (check (qt-color-blue c) => 0)
        (check (qt-color-valid? c) => #t)
        (qt-color-destroy! c)))

    (test-case "color name returns hex"
      (ensure-app!)
      (let ((c (qt-color-create 255 0 0)))
        (check (qt-color-name c) => "#ff0000")
        (qt-color-destroy! c)))

    (test-case "invalid color from bad name"
      (ensure-app!)
      (let ((c (qt-color-create-name "not-a-color")))
        (check (qt-color-valid? c) => #f)
        (qt-color-destroy! c)))

    ;; ==================== Phase 8a: Dialogs ====================
    ;; Note: QFontDialog::getFont() and QColorDialog::getColor() are
    ;; blocking modal dialogs — cannot be tested in offscreen mode.
    ;; The API wrappers are simple pass-throughs; tested via examples.

    ;; ==================== Phase 8b: Stacked Widget ====================

    (test-case "stacked widget create/add/count"
      (ensure-app!)
      (let* ((sw (qt-stacked-widget-create))
             (p1 (qt-widget-create))
             (p2 (qt-widget-create)))
        (check (qt-stacked-widget-count sw) => 0)
        (qt-stacked-widget-add-widget! sw p1)
        (qt-stacked-widget-add-widget! sw p2)
        (check (qt-stacked-widget-count sw) => 2)
        (qt-widget-destroy! sw)))

    (test-case "stacked widget current index"
      (ensure-app!)
      (let* ((sw (qt-stacked-widget-create))
             (p1 (qt-widget-create))
             (p2 (qt-widget-create)))
        (qt-stacked-widget-add-widget! sw p1)
        (qt-stacked-widget-add-widget! sw p2)
        (check (qt-stacked-widget-current-index sw) => 0)
        (qt-stacked-widget-set-current-index! sw 1)
        (check (qt-stacked-widget-current-index sw) => 1)
        (qt-widget-destroy! sw)))

    (test-case "stacked widget signal registration"
      (ensure-app!)
      (let ((sw (qt-stacked-widget-create)))
        (qt-on-stacked-changed! sw (lambda (idx) #t))
        (qt-widget-destroy! sw)))

    ;; ==================== Phase 8b: Dock Widget ====================

    (test-case "dock widget create with title"
      (ensure-app!)
      (let ((dw (qt-dock-widget-create "Panel")))
        (check (qt-dock-widget-title dw) => "Panel")
        (qt-widget-destroy! dw)))

    (test-case "dock widget set/get title"
      (ensure-app!)
      (let ((dw (qt-dock-widget-create "Old")))
        (qt-dock-widget-set-title! dw "New")
        (check (qt-dock-widget-title dw) => "New")
        (qt-widget-destroy! dw)))

    (test-case "dock widget set content widget"
      (ensure-app!)
      (let* ((dw (qt-dock-widget-create "Dock"))
             (content (qt-label-create "Content")))
        (qt-dock-widget-set-widget! dw content)
        (check (not (eq? (qt-dock-widget-widget dw) #f)) => #t)
        (qt-widget-destroy! dw)))

    (test-case "dock widget floating"
      (ensure-app!)
      (let ((dw (qt-dock-widget-create "Dock")))
        ;; Default is not floating when standalone
        (qt-dock-widget-set-floating! dw #t)
        (check (qt-dock-widget-floating? dw) => #t)
        (qt-widget-destroy! dw)))

    (test-case "main window add dock widget"
      (ensure-app!)
      (let* ((win (qt-main-window-create))
             (dw (qt-dock-widget-create "Side Panel"))
             (content (qt-label-create "Docked")))
        (qt-dock-widget-set-widget! dw content)
        (qt-main-window-add-dock-widget! win QT_DOCK_LEFT dw)
        (qt-widget-destroy! win)))

    ;; ==================== Phase 8c: System Tray Icon ====================

    (test-case "system tray available check"
      (ensure-app!)
      ;; Just verify it returns a boolean (may be #f in offscreen)
      (let ((avail (qt-system-tray-available?)))
        (check (boolean? avail) => #t)))

    ;; ==================== Phase 8d: QPainter ====================

    (test-case "blank pixmap create and fill"
      (ensure-app!)
      (let ((pm (qt-pixmap-create-blank 200 100)))
        (check (qt-pixmap-width pm) => 200)
        (check (qt-pixmap-height pm) => 100)
        (qt-pixmap-fill! pm 255 0 0)
        (check (qt-pixmap-null? pm) => #f)
        (qt-pixmap-destroy! pm)))

    (test-case "painter create/draw/end/destroy"
      (ensure-app!)
      (let* ((pm (qt-pixmap-create-blank 100 100))
             (p (qt-painter-create pm)))
        (qt-painter-set-pen-color! p 0 0 0)
        (qt-painter-set-pen-width! p 2)
        (qt-painter-draw-line! p 0 0 100 100)
        (qt-painter-draw-rect! p 10 10 80 80)
        (qt-painter-draw-ellipse! p 20 20 60 60)
        (qt-painter-draw-point! p 50 50)
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-pixmap-destroy! pm)))

    (test-case "painter fill rect and brush"
      (ensure-app!)
      (let* ((pm (qt-pixmap-create-blank 100 100))
             (p (qt-painter-create pm)))
        (qt-painter-fill-rect! p 0 0 50 50 255 0 0)
        (qt-painter-set-brush-color! p 0 255 0)
        (qt-painter-draw-rect! p 50 50 50 50)
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-pixmap-destroy! pm)))

    (test-case "painter draw text"
      (ensure-app!)
      (let* ((pm (qt-pixmap-create-blank 200 50))
             (p (qt-painter-create pm)))
        (qt-painter-draw-text! p 10 30 "Hello World")
        (qt-painter-draw-text-rect! p 0 0 200 50 QT_ALIGN_CENTER "Centered")
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-pixmap-destroy! pm)))

    (test-case "painter save/restore state"
      (ensure-app!)
      (let* ((pm (qt-pixmap-create-blank 100 100))
             (p (qt-painter-create pm)))
        (qt-painter-save! p)
        (qt-painter-set-pen-color! p 255 0 0)
        (qt-painter-translate! p 50 50)
        (qt-painter-draw-line! p -20 0 20 0)
        (qt-painter-restore! p)
        ;; Pen and transform restored
        (qt-painter-draw-line! p 0 0 10 10)
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-pixmap-destroy! pm)))

    (test-case "painter set font"
      (ensure-app!)
      (let* ((pm (qt-pixmap-create-blank 200 50))
             (p (qt-painter-create pm))
             (f (qt-font-create "Monospace" point-size: 20)))
        (qt-painter-set-font!* p f)
        (qt-painter-draw-text! p 10 30 "Mono")
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-font-destroy! f)
        (qt-pixmap-destroy! pm)))

    (test-case "painter antialiasing and transforms"
      (ensure-app!)
      (let* ((pm (qt-pixmap-create-blank 100 100))
             (p (qt-painter-create pm)))
        (qt-painter-set-antialiasing! p #t)
        (qt-painter-translate! p 50 50)
        (qt-painter-rotate! p 45.0)
        (qt-painter-scale! p 2.0 2.0)
        (qt-painter-draw-line! p -10 0 10 0)
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-pixmap-destroy! pm)))

    (test-case "painter draw arc"
      (ensure-app!)
      (let* ((pm (qt-pixmap-create-blank 100 100))
             (p (qt-painter-create pm)))
        ;; Draw quarter circle (angles in 1/16th degrees)
        (qt-painter-draw-arc! p 10 10 80 80 0 (* 90 16))
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-pixmap-destroy! pm)))

    (test-case "painter draw pixmap composite"
      (ensure-app!)
      (let* ((bg (qt-pixmap-create-blank 100 100))
             (fg (qt-pixmap-create-blank 50 50))
             (p (qt-painter-create bg)))
        (qt-pixmap-fill! bg 255 255 255)
        (qt-pixmap-fill! fg 255 0 0)
        (qt-painter-draw-pixmap! p 25 25 fg)
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-pixmap-destroy! fg)
        (qt-pixmap-destroy! bg)))

    (test-case "paint then display on label"
      (ensure-app!)
      (let* ((pm (qt-pixmap-create-blank 100 100))
             (p (qt-painter-create pm))
             (label (qt-label-create "")))
        (qt-pixmap-fill! pm 0 0 255)
        (qt-painter-draw-text! p 10 50 "Qt")
        (qt-painter-end! p)
        (qt-painter-destroy! p)
        (qt-label-set-pixmap! label pm)
        ;; Verify label accepted the pixmap (no crash)
        (qt-widget-destroy! label)
        (qt-pixmap-destroy! pm)))

    ;; ==================== Phase 8e: Drag and Drop ====================

    (test-case "set accept drops"
      (ensure-app!)
      (let ((w (qt-widget-create)))
        (qt-widget-set-accept-drops! w #t)
        ;; No crash
        (qt-widget-destroy! w)))

    (test-case "drop filter install and destroy"
      (ensure-app!)
      (let* ((w (qt-widget-create))
             (df (qt-on-drop! w (lambda (text) #t))))
        ;; Verify we got a non-null handle
        (check (not (eq? df #f)) => #t)
        (qt-drop-filter-destroy! df)
        (qt-widget-destroy! w)))

    ;; ==================== Phase 9: Practical Widgets & Dialogs ====================

    ;; --- Double Spin Box ---

    (test-case "double spin box create"
      (ensure-app!)
      (let ((dsb (qt-double-spin-box-create)))
        (check (not (eq? dsb #f)) => #t)
        (qt-widget-destroy! dsb)))

    (test-case "double spin box value round-trip"
      (ensure-app!)
      (let ((dsb (qt-double-spin-box-create)))
        (qt-double-spin-box-set-range! dsb 0.0 100.0)
        (qt-double-spin-box-set-decimals! dsb 4)
        (qt-double-spin-box-set-value! dsb 42.5)
        (check (qt-double-spin-box-value dsb) => 42.5)
        (qt-widget-destroy! dsb)))

    (test-case "double spin box range and step"
      (ensure-app!)
      (let ((dsb (qt-double-spin-box-create)))
        (qt-double-spin-box-set-range! dsb -10.0 10.0)
        (qt-double-spin-box-set-single-step! dsb 0.5)
        ;; Value outside range gets clamped
        (qt-double-spin-box-set-value! dsb 20.0)
        (check (qt-double-spin-box-value dsb) => 10.0)
        (qt-widget-destroy! dsb)))

    (test-case "double spin box decimals"
      (ensure-app!)
      (let ((dsb (qt-double-spin-box-create)))
        (qt-double-spin-box-set-decimals! dsb 3)
        (check (qt-double-spin-box-decimals dsb) => 3)
        (qt-widget-destroy! dsb)))

    (test-case "double spin box prefix and suffix"
      (ensure-app!)
      (let ((dsb (qt-double-spin-box-create)))
        (qt-double-spin-box-set-prefix! dsb "$")
        (qt-double-spin-box-set-suffix! dsb " USD")
        ;; No crash — prefix/suffix are display-only, no getter
        (qt-widget-destroy! dsb)))

    (test-case "double spin box signal registration"
      (ensure-app!)
      (let ((dsb (qt-double-spin-box-create)))
        (qt-on-double-value-changed! dsb (lambda (val) #t))
        (qt-widget-destroy! dsb)))

    ;; --- Date Edit ---

    (test-case "date edit create"
      (ensure-app!)
      (let ((d (qt-date-edit-create)))
        (check (not (eq? d #f)) => #t)
        (qt-widget-destroy! d)))

    (test-case "date edit set/get date"
      (ensure-app!)
      (let ((d (qt-date-edit-create)))
        (qt-date-edit-set-date! d 2025 6 15)
        (check (qt-date-edit-year d) => 2025)
        (check (qt-date-edit-month d) => 6)
        (check (qt-date-edit-day d) => 15)
        (qt-widget-destroy! d)))

    (test-case "date edit date string"
      (ensure-app!)
      (let ((d (qt-date-edit-create)))
        (qt-date-edit-set-date! d 2025 1 9)
        (check (qt-date-edit-date-string d) => "2025-01-09")
        (qt-widget-destroy! d)))

    (test-case "date edit min/max"
      (ensure-app!)
      (let ((d (qt-date-edit-create)))
        (qt-date-edit-set-minimum-date! d 2020 1 1)
        (qt-date-edit-set-maximum-date! d 2030 12 31)
        ;; No crash
        (qt-widget-destroy! d)))

    (test-case "date edit calendar popup"
      (ensure-app!)
      (let ((d (qt-date-edit-create)))
        (qt-date-edit-set-calendar-popup! d #t)
        ;; No crash
        (qt-widget-destroy! d)))

    (test-case "date edit signal registration"
      (ensure-app!)
      (let ((d (qt-date-edit-create)))
        (qt-on-date-changed! d (lambda (iso-str) #t))
        (qt-widget-destroy! d)))

    ;; --- Time Edit ---

    (test-case "time edit create"
      (ensure-app!)
      (let ((t (qt-time-edit-create)))
        (check (not (eq? t #f)) => #t)
        (qt-widget-destroy! t)))

    (test-case "time edit set/get time"
      (ensure-app!)
      (let ((t (qt-time-edit-create)))
        (qt-time-edit-set-time! t 14 30 45)
        (check (qt-time-edit-hour t) => 14)
        (check (qt-time-edit-minute t) => 30)
        (check (qt-time-edit-second t) => 45)
        (qt-widget-destroy! t)))

    (test-case "time edit time string"
      (ensure-app!)
      (let ((t (qt-time-edit-create)))
        (qt-time-edit-set-time! t 9 5 0)
        (check (qt-time-edit-time-string t) => "09:05:00")
        (qt-widget-destroy! t)))

    (test-case "time edit display format"
      (ensure-app!)
      (let ((t (qt-time-edit-create)))
        (qt-time-edit-set-display-format! t "HH:mm")
        ;; No crash
        (qt-widget-destroy! t)))

    (test-case "time edit signal registration"
      (ensure-app!)
      (let ((t (qt-time-edit-create)))
        (qt-on-time-changed! t (lambda (iso-str) #t))
        (qt-widget-destroy! t)))

    ;; --- Frame ---

    (test-case "frame create"
      (ensure-app!)
      (let ((f (qt-frame-create)))
        (check (not (eq? f #f)) => #t)
        (qt-widget-destroy! f)))

    (test-case "frame shape/shadow round-trip"
      (ensure-app!)
      (let ((f (qt-frame-create)))
        (qt-frame-set-shape! f QT_FRAME_BOX)
        (check (qt-frame-shape f) => QT_FRAME_BOX)
        (qt-frame-set-shadow! f QT_FRAME_SUNKEN)
        (check (qt-frame-shadow f) => QT_FRAME_SUNKEN)
        (qt-widget-destroy! f)))

    (test-case "frame line width"
      (ensure-app!)
      (let ((f (qt-frame-create)))
        (qt-frame-set-line-width! f 3)
        (check (qt-frame-line-width f) => 3)
        (qt-widget-destroy! f)))

    (test-case "frame as container with layout"
      (ensure-app!)
      (let* ((f (qt-frame-create))
             (layout (qt-vbox-layout-create f))
             (label (qt-label-create "inside frame")))
        (qt-frame-set-shape! f QT_FRAME_PANEL)
        (qt-frame-set-shadow! f QT_FRAME_RAISED)
        (qt-layout-add-widget! layout label)
        (qt-widget-destroy! f)))

    ;; --- Progress Dialog ---

    (test-case "progress dialog create"
      (ensure-app!)
      (let ((pd (qt-progress-dialog-create "Loading..." "Cancel" 0 100)))
        (check (not (eq? pd #f)) => #t)
        (qt-widget-destroy! pd)))

    (test-case "progress dialog value round-trip"
      (ensure-app!)
      (let ((pd (qt-progress-dialog-create "Working..." "Cancel" 0 100)))
        (qt-progress-dialog-set-value! pd 42)
        (check (qt-progress-dialog-value pd) => 42)
        (qt-widget-destroy! pd)))

    (test-case "progress dialog range"
      (ensure-app!)
      (let ((pd (qt-progress-dialog-create "Step..." "Cancel" 0 10)))
        (qt-progress-dialog-set-range! pd 0 200)
        (qt-progress-dialog-set-value! pd 150)
        (check (qt-progress-dialog-value pd) => 150)
        (qt-widget-destroy! pd)))

    (test-case "progress dialog label text"
      (ensure-app!)
      (let ((pd (qt-progress-dialog-create "Old label" "Cancel" 0 100)))
        (qt-progress-dialog-set-label-text! pd "New label")
        ;; No crash
        (qt-widget-destroy! pd)))

    (test-case "progress dialog not initially canceled"
      (ensure-app!)
      (let ((pd (qt-progress-dialog-create "Test" "Cancel" 0 100)))
        (check (qt-progress-dialog-canceled? pd) => #f)
        (qt-widget-destroy! pd)))

    (test-case "progress dialog min-duration and auto settings"
      (ensure-app!)
      (let ((pd (qt-progress-dialog-create "Test" "Cancel" 0 100)))
        (qt-progress-dialog-set-minimum-duration! pd 500)
        (qt-progress-dialog-set-auto-close! pd #t)
        (qt-progress-dialog-set-auto-reset! pd #f)
        ;; No crash
        (qt-widget-destroy! pd)))

    (test-case "progress dialog reset"
      (ensure-app!)
      (let ((pd (qt-progress-dialog-create "Test" "Cancel" 0 100)))
        (qt-progress-dialog-set-value! pd 50)
        (qt-progress-dialog-reset! pd)
        ;; After reset, Qt sets value to minimum-1 (i.e. -1 when min=0)
        (check (qt-progress-dialog-value pd) => -1)
        (qt-widget-destroy! pd)))

    (test-case "progress dialog signal registration"
      (ensure-app!)
      (let ((pd (qt-progress-dialog-create "Test" "Cancel" 0 100)))
        (qt-on-progress-canceled! pd (lambda () #t))
        (qt-widget-destroy! pd)))

    ;; --- Input Dialog ---
    ;; Note: QInputDialog static methods are blocking modal dialogs.
    ;; Cannot test the actual dialogs in offscreen mode. Only test was_accepted.

    (test-case "input dialog functions exist"
      (ensure-app!)
      ;; Verify the high-level wrappers are callable (bound)
      ;; Can't test actual dialogs — they're blocking modal
      (check (procedure? qt-input-dialog-get-text) => #t)
      (check (procedure? qt-input-dialog-get-int) => #t)
      (check (procedure? qt-input-dialog-get-double) => #t)
      (check (procedure? qt-input-dialog-get-item) => #t))

    ;; --- Frame Constants ---

    (test-case "frame constants"
      (check QT_FRAME_NO_FRAME     => 0)
      (check QT_FRAME_BOX          => 1)
      (check QT_FRAME_PANEL        => 2)
      (check QT_FRAME_WIN_PANEL    => 3)
      (check QT_FRAME_HLINE        => 4)
      (check QT_FRAME_VLINE        => 5)
      (check QT_FRAME_STYLED_PANEL => 6)
      (check QT_FRAME_PLAIN        => #x0010)
      (check QT_FRAME_RAISED       => #x0020)
      (check QT_FRAME_SUNKEN       => #x0030))

    ;; ==================== Integration: Layout + Widgets ====================

    (test-case "main window with layout and mixed widgets"
      (let* ((win (qt-main-window-create))
             (central (qt-widget-create))
             (layout (qt-vbox-layout-create central))
             (label (qt-label-create "Title"))
             (edit (qt-line-edit-create))
             (btn (qt-push-button-create "OK"))
             (check-box (qt-check-box-create "Agree")))
        (qt-layout-add-widget! layout label)
        (qt-layout-add-widget! layout edit)
        (qt-layout-add-widget! layout btn)
        (qt-layout-add-widget! layout check-box)
        (qt-layout-set-spacing! layout 10)
        (qt-layout-set-margins! layout 5 5 5 5)
        (qt-layout-add-stretch! layout)
        (qt-main-window-set-central-widget! win central)
        (qt-main-window-set-title! win "Integration Test")
        ;; Verify widgets still work after being laid out
        (qt-label-set-text! label "Updated Title")
        (check (qt-label-text label) => "Updated Title")
        (qt-line-edit-set-text! edit "typed")
        (check (qt-line-edit-text edit) => "typed")
        (qt-widget-destroy! win)))

    ;; ==================== Phase 10: Forms, Dialogs, Shortcuts, Calendar, Rich Text ====================

    ;; --- Form Layout ---

    (test-case "form layout create"
      (let* ((w (qt-widget-create))
             (fl (qt-form-layout-create parent: w)))
        (check (not (eq? fl #f)) => #t)
        (check (qt-form-layout-row-count fl) => 0)
        (qt-widget-destroy! w)))

    (test-case "form layout add rows"
      (let* ((w (qt-widget-create))
             (fl (qt-form-layout-create parent: w))
             (edit1 (qt-line-edit-create parent: w))
             (edit2 (qt-line-edit-create parent: w)))
        (qt-form-layout-add-row! fl "Name:" edit1)
        (check (qt-form-layout-row-count fl) => 1)
        (qt-form-layout-add-row! fl "Email:" edit2)
        (check (qt-form-layout-row-count fl) => 2)
        (qt-widget-destroy! w)))

    (test-case "form layout add row widget"
      (let* ((w (qt-widget-create))
             (fl (qt-form-layout-create parent: w))
             (label (qt-label-create "Custom Label" parent: w))
             (edit (qt-line-edit-create parent: w)))
        (qt-form-layout-add-row-widget! fl label edit)
        (check (qt-form-layout-row-count fl) => 1)
        (qt-widget-destroy! w)))

    (test-case "form layout add spanning widget"
      (let* ((w (qt-widget-create))
             (fl (qt-form-layout-create parent: w))
             (btn (qt-push-button-create "Submit" parent: w)))
        (qt-form-layout-add-spanning-widget! fl btn)
        (check (qt-form-layout-row-count fl) => 1)
        (qt-widget-destroy! w)))

    (test-case "form layout spacing and margins"
      (let* ((w (qt-widget-create))
             (fl (qt-form-layout-create parent: w)))
        ;; These use existing qt_layout_t functions
        (qt-layout-set-spacing! fl 10)
        (qt-layout-set-margins! fl 5 5 5 5)
        (qt-widget-destroy! w)))

    ;; --- Shortcut ---

    (test-case "shortcut create and destroy"
      (let* ((win (qt-widget-create))
             (sc (qt-shortcut-create "Ctrl+S" win)))
        (check (not (eq? sc #f)) => #t)
        (qt-shortcut-destroy! sc)
        (qt-widget-destroy! win)))

    (test-case "shortcut set key"
      (let* ((win (qt-widget-create))
             (sc (qt-shortcut-create "Ctrl+S" win)))
        (qt-shortcut-set-key! sc "Ctrl+Q")
        (qt-shortcut-destroy! sc)
        (qt-widget-destroy! win)))

    (test-case "shortcut enabled"
      (let* ((win (qt-widget-create))
             (sc (qt-shortcut-create "Ctrl+S" win)))
        (check (qt-shortcut-enabled? sc) => #t)
        (qt-shortcut-set-enabled! sc #f)
        (check (qt-shortcut-enabled? sc) => #f)
        (qt-shortcut-set-enabled! sc #t)
        (check (qt-shortcut-enabled? sc) => #t)
        (qt-shortcut-destroy! sc)
        (qt-widget-destroy! win)))

    (test-case "shortcut on activated"
      (let* ((win (qt-widget-create))
             (sc (qt-shortcut-create "Ctrl+T" win)))
        (qt-on-shortcut-activated! sc (lambda () #t))
        (qt-shortcut-destroy! sc)
        (qt-widget-destroy! win)))

    ;; --- Text Browser ---

    (test-case "text browser create"
      (let ((tb (qt-text-browser-create)))
        (check (not (eq? tb #f)) => #t)
        (qt-widget-destroy! tb)))

    (test-case "text browser set html and plain text"
      (let ((tb (qt-text-browser-create)))
        (qt-text-browser-set-html! tb "<b>Hello</b>")
        (check (string-contains (qt-text-browser-plain-text tb) "Hello") =>  0)
        (qt-text-browser-set-plain-text! tb "Plain text")
        (check (qt-text-browser-plain-text tb) => "Plain text")
        (qt-widget-destroy! tb)))

    (test-case "text browser open external links"
      (let ((tb (qt-text-browser-create)))
        (qt-text-browser-set-open-external-links! tb #t)
        (qt-widget-destroy! tb)))

    (test-case "text browser source"
      (let ((tb (qt-text-browser-create)))
        ;; Default source is empty
        (check (qt-text-browser-source tb) => "")
        (qt-widget-destroy! tb)))

    (test-case "text browser on anchor clicked"
      (let ((tb (qt-text-browser-create)))
        (qt-on-anchor-clicked! tb (lambda (url) #t))
        (qt-widget-destroy! tb)))

    ;; --- Dialog Button Box ---

    (test-case "button box create"
      (let ((bb (qt-button-box-create (bitwise-ior QT_BUTTON_OK QT_BUTTON_CANCEL))))
        (check (not (eq? bb #f)) => #t)
        (qt-widget-destroy! bb)))

    (test-case "button box get standard button"
      (let ((bb (qt-button-box-create (bitwise-ior QT_BUTTON_OK QT_BUTTON_CANCEL))))
        (let ((ok-btn (qt-button-box-button bb QT_BUTTON_OK)))
          (check (not (eq? ok-btn #f)) => #t))
        (qt-widget-destroy! bb)))

    (test-case "button box add custom button"
      (let* ((bb (qt-button-box-create QT_BUTTON_OK))
             (btn (qt-push-button-create "Custom")))
        (qt-button-box-add-button! bb btn QT_BUTTON_ROLE_ACTION)
        (qt-widget-destroy! bb)))

    (test-case "button box signals"
      (let ((bb (qt-button-box-create (bitwise-ior QT_BUTTON_OK QT_BUTTON_CANCEL))))
        (qt-on-accepted! bb (lambda () #t))
        (qt-on-rejected! bb (lambda () #t))
        (qt-on-button-clicked! bb (lambda () #t))
        (qt-widget-destroy! bb)))

    (test-case "button box constants"
      (check QT_BUTTON_OK      => #x00000400)
      (check QT_BUTTON_CANCEL  => #x00400000)
      (check QT_BUTTON_APPLY   => #x02000000)
      (check QT_BUTTON_CLOSE   => #x00200000)
      (check QT_BUTTON_YES     => #x00004000)
      (check QT_BUTTON_NO      => #x00010000)
      (check QT_BUTTON_RESET   => #x04000000)
      (check QT_BUTTON_HELP    => #x01000000)
      (check QT_BUTTON_SAVE    => #x00000800)
      (check QT_BUTTON_DISCARD => #x00800000)
      (check QT_BUTTON_ROLE_ACCEPT  => 0)
      (check QT_BUTTON_ROLE_REJECT  => 1)
      (check QT_BUTTON_ROLE_HELP    => 4))

    ;; --- Calendar Widget ---

    (test-case "calendar create"
      (let ((cal (qt-calendar-create)))
        (check (not (eq? cal #f)) => #t)
        (qt-widget-destroy! cal)))

    (test-case "calendar set and get date"
      (let ((cal (qt-calendar-create)))
        (qt-calendar-set-selected-date! cal 2025 6 15)
        (check (qt-calendar-selected-year cal) => 2025)
        (check (qt-calendar-selected-month cal) => 6)
        (check (qt-calendar-selected-day cal) => 15)
        (check (qt-calendar-selected-date-string cal) => "2025-06-15")
        (qt-widget-destroy! cal)))

    (test-case "calendar min/max date"
      (let ((cal (qt-calendar-create)))
        (qt-calendar-set-minimum-date! cal 2020 1 1)
        (qt-calendar-set-maximum-date! cal 2030 12 31)
        (qt-calendar-set-selected-date! cal 2025 6 15)
        (check (qt-calendar-selected-year cal) => 2025)
        (qt-widget-destroy! cal)))

    (test-case "calendar grid visible"
      (let ((cal (qt-calendar-create)))
        (qt-calendar-set-grid-visible! cal #t)
        (check (qt-calendar-grid-visible? cal) => #t)
        (qt-calendar-set-grid-visible! cal #f)
        (check (qt-calendar-grid-visible? cal) => #f)
        (qt-widget-destroy! cal)))

    (test-case "calendar first day of week"
      (let ((cal (qt-calendar-create)))
        (qt-calendar-set-first-day-of-week! cal QT_SUNDAY)
        (qt-calendar-set-first-day-of-week! cal QT_MONDAY)
        (qt-widget-destroy! cal)))

    (test-case "calendar navigation bar"
      (let ((cal (qt-calendar-create)))
        (qt-calendar-set-navigation-bar-visible! cal #f)
        (qt-calendar-set-navigation-bar-visible! cal #t)
        (qt-widget-destroy! cal)))

    (test-case "calendar signals"
      (let ((cal (qt-calendar-create)))
        (qt-on-selection-changed! cal (lambda () #t))
        (qt-on-calendar-clicked! cal (lambda (date-str) #t))
        (qt-widget-destroy! cal)))

    (test-case "day-of-week constants"
      (check QT_MONDAY    => 1)
      (check QT_TUESDAY   => 2)
      (check QT_WEDNESDAY => 3)
      (check QT_THURSDAY  => 4)
      (check QT_FRIDAY    => 5)
      (check QT_SATURDAY  => 6)
      (check QT_SUNDAY    => 7))

    ;; ==================== Phase 11: QSettings, QCompleter, QToolTip ====================

    ;; --- QSettings ---

    (test-case "settings create org/app"
      (ensure-app!)
      (let ((s (qt-settings-create "gerbil-qt-test" "test-app")))
        (check (not (eq? s #f)) => #t)
        (qt-settings-destroy! s)))

    (test-case "settings create file INI"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test.ini")))
        (check (not (eq? s #f)) => #t)
        ;; file-name should contain the path
        (check (number? (string-contains (qt-settings-file-name s) "gerbil-qt-test")) => #t)
        (qt-settings-destroy! s)))

    (test-case "settings string round-trip"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-str.ini")))
        (qt-settings-clear! s)
        (qt-settings-set-string! s "name" "Alice")
        (check (qt-settings-string s "name") => "Alice")
        ;; default for missing key
        (check (qt-settings-string s "missing" default: "fallback") => "fallback")
        (qt-settings-destroy! s)))

    (test-case "settings int round-trip"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-int.ini")))
        (qt-settings-clear! s)
        (qt-settings-set-int! s "count" 42)
        (check (qt-settings-int s "count") => 42)
        ;; default for missing key
        (check (qt-settings-int s "missing" default: -1) => -1)
        (qt-settings-destroy! s)))

    (test-case "settings double round-trip"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-dbl.ini")))
        (qt-settings-clear! s)
        (qt-settings-set-double! s "pi" 3.14)
        ;; INI stores as string, so precision may vary slightly
        (let ((val (qt-settings-double s "pi")))
          (check (> val 3.13) => #t)
          (check (< val 3.15) => #t))
        (qt-settings-destroy! s)))

    (test-case "settings bool round-trip"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-bool.ini")))
        (qt-settings-clear! s)
        (qt-settings-set-bool! s "enabled" #t)
        (check (qt-settings-bool s "enabled") => #t)
        (qt-settings-set-bool! s "enabled" #f)
        (check (qt-settings-bool s "enabled") => #f)
        ;; default for missing key
        (check (qt-settings-bool s "missing" default: #t) => #t)
        (qt-settings-destroy! s)))

    (test-case "settings generic set/value"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-gen.ini")))
        (qt-settings-clear! s)
        (qt-settings-set! s "text" "hello")
        (qt-settings-set! s "num" 99)
        (qt-settings-set! s "flag" #t)
        (check (qt-settings-value s "text") => "hello")
        ;; Generic value returns strings — int/bool stored as strings in INI
        (check (string? (qt-settings-value s "num")) => #t)
        (qt-settings-destroy! s)))

    (test-case "settings contains and remove"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-rm.ini")))
        (qt-settings-clear! s)
        (qt-settings-set-string! s "key1" "val1")
        (check (qt-settings-contains? s "key1") => #t)
        (check (qt-settings-contains? s "key2") => #f)
        (qt-settings-remove! s "key1")
        (check (qt-settings-contains? s "key1") => #f)
        (qt-settings-destroy! s)))

    (test-case "settings all-keys"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-keys.ini")))
        (qt-settings-clear! s)
        (qt-settings-set-string! s "a" "1")
        (qt-settings-set-string! s "b" "2")
        (qt-settings-set-string! s "c" "3")
        (let ((keys (qt-settings-all-keys s)))
          (check (length keys) => 3)
          (check (member "a" keys) => '("a" "b" "c")))
        (qt-settings-destroy! s)))

    (test-case "settings groups"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-grp.ini")))
        (qt-settings-clear! s)
        ;; Set keys in different groups
        (qt-settings-begin-group! s "window")
        (check (qt-settings-group s) => "window")
        (qt-settings-set-int! s "width" 800)
        (qt-settings-set-int! s "height" 600)
        (qt-settings-end-group! s)
        ;; Verify child groups
        (let ((groups (qt-settings-child-groups s)))
          (check (member "window" groups) => '("window")))
        ;; Verify child keys within group
        (qt-settings-begin-group! s "window")
        (let ((keys (qt-settings-child-keys s)))
          (check (length keys) => 2))
        (qt-settings-end-group! s)
        (qt-settings-destroy! s)))

    (test-case "settings file-name and writable"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-info.ini")))
        (check (string? (qt-settings-file-name s)) => #t)
        (check (qt-settings-writable? s) => #t)
        (qt-settings-destroy! s)))

    (test-case "settings clear"
      (ensure-app!)
      (let ((s (qt-settings-create-file "/tmp/gerbil-qt-test-clr.ini")))
        (qt-settings-set-string! s "x" "y")
        (qt-settings-clear! s)
        (check (qt-settings-all-keys s) => '())
        (qt-settings-destroy! s)))

    (test-case "settings format constants"
      (check QT_SETTINGS_NATIVE => 0)
      (check QT_SETTINGS_INI    => 1))

    ;; --- QCompleter ---

    (test-case "completer create"
      (ensure-app!)
      (let ((c (qt-completer-create '("apple" "banana" "cherry"))))
        (check (not (eq? c #f)) => #t)
        (qt-completer-destroy! c)))

    (test-case "completer completion count"
      (ensure-app!)
      (let ((c (qt-completer-create '("apple" "apricot" "banana"))))
        (qt-completer-set-completion-prefix! c "ap")
        (check (qt-completer-completion-count c) => 2)
        (qt-completer-destroy! c)))

    (test-case "completer current completion"
      (ensure-app!)
      (let ((c (qt-completer-create '("apple" "apricot" "banana"))))
        (qt-completer-set-completion-prefix! c "app")
        (check (qt-completer-completion-count c) => 1)
        (check (qt-completer-current-completion c) => "apple")
        (qt-completer-destroy! c)))

    (test-case "completer case sensitivity"
      (ensure-app!)
      (let ((c (qt-completer-create '("Apple" "apricot" "Banana"))))
        ;; Default is case sensitive
        (qt-completer-set-case-sensitivity! c #t)
        (qt-completer-set-completion-prefix! c "a")
        (check (qt-completer-completion-count c) => 1)  ;; only "apricot"
        ;; Case insensitive
        (qt-completer-set-case-sensitivity! c #f)
        (qt-completer-set-completion-prefix! c "a")
        (check (qt-completer-completion-count c) => 2)  ;; "Apple" and "apricot"
        (qt-completer-destroy! c)))

    (test-case "completer completion modes"
      (ensure-app!)
      (let ((c (qt-completer-create '("test"))))
        (qt-completer-set-completion-mode! c QT_COMPLETER_POPUP)
        (qt-completer-set-completion-mode! c QT_COMPLETER_INLINE)
        (qt-completer-set-completion-mode! c QT_COMPLETER_UNFILTERED_POPUP)
        ;; No crash
        (qt-completer-destroy! c)))

    (test-case "completer update model"
      (ensure-app!)
      (let ((c (qt-completer-create '("old1" "old2"))))
        (qt-completer-set-completion-prefix! c "old")
        (check (qt-completer-completion-count c) => 2)
        (qt-completer-set-model-strings! c '("new1" "new2" "new3"))
        (qt-completer-set-completion-prefix! c "new")
        (check (qt-completer-completion-count c) => 3)
        (qt-completer-destroy! c)))

    (test-case "completer attach to line edit"
      (ensure-app!)
      (let ((c (qt-completer-create '("alpha" "beta" "gamma")))
            (e (qt-line-edit-create)))
        (qt-line-edit-set-completer! e c)
        ;; Completer ownership transferred — don't destroy c
        (qt-widget-destroy! e)))

    (test-case "completer on activated signal"
      (ensure-app!)
      (let ((c (qt-completer-create '("test"))))
        (qt-on-completer-activated! c (lambda (text) #t))
        (qt-completer-destroy! c)))

    (test-case "completer constants"
      (check QT_COMPLETER_POPUP            => 0)
      (check QT_COMPLETER_INLINE           => 1)
      (check QT_COMPLETER_UNFILTERED_POPUP => 2)
      (check QT_CASE_INSENSITIVE           => 0)
      (check QT_CASE_SENSITIVE             => 1)
      (check QT_MATCH_STARTS_WITH          => 0)
      (check QT_MATCH_CONTAINS             => 1)
      (check QT_MATCH_ENDS_WITH            => 2))

    ;; --- QToolTip / QWhatsThis ---

    (test-case "widget tooltip round-trip"
      (ensure-app!)
      (let ((w (qt-widget-create)))
        (qt-widget-set-tooltip! w "Help text")
        (check (qt-widget-tooltip w) => "Help text")
        (qt-widget-destroy! w)))

    (test-case "widget tooltip empty default"
      (ensure-app!)
      (let ((w (qt-widget-create)))
        (check (qt-widget-tooltip w) => "")
        (qt-widget-destroy! w)))

    (test-case "tooltip show/hide no crash"
      (ensure-app!)
      ;; QToolTip::showText may not render in offscreen — test no crash only
      (qt-tooltip-show-text! 100 100 "Tip text")
      (qt-tooltip-hide-text!))

    (test-case "whatsthis round-trip"
      (ensure-app!)
      (let ((w (qt-widget-create)))
        (qt-widget-set-whats-this! w "Explanation here")
        (check (qt-widget-whats-this w) => "Explanation here")
        (qt-widget-destroy! w)))

    (test-case "whatsthis empty default"
      (ensure-app!)
      (let ((w (qt-widget-create)))
        (check (qt-widget-whats-this w) => "")
        (qt-widget-destroy! w)))

    ;; ==================== Phase 12: Model/View Framework ====================

    ;; --- QStandardItemModel ---

    (test-case "standard model create and dimensions"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 3 cols: 2)))
        (check (not (eq? m #f)) => #t)
        (check (qt-standard-model-row-count m) => 3)
        (check (qt-standard-model-column-count m) => 2)
        (qt-standard-model-destroy! m)))

    (test-case "standard model set/get item"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 2))
            (item (qt-standard-item-create text: "Hello")))
        (qt-standard-model-set-item! m 0 0 item)
        (let ((got (qt-standard-model-item m 0 0)))
          (check (qt-standard-item-text got) => "Hello"))
        (qt-standard-model-destroy! m)))

    (test-case "standard model headers"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 2)))
        (qt-standard-model-set-horizontal-header! m 0 "Name")
        (qt-standard-model-set-horizontal-header! m 1 "Value")
        (qt-standard-model-set-vertical-header! m 0 "Row 1")
        ;; No crash — headers are set
        (qt-standard-model-destroy! m)))

    (test-case "standard model set row/col count"
      (ensure-app!)
      (let ((m (qt-standard-model-create)))
        (qt-standard-model-set-row-count! m 5)
        (qt-standard-model-set-column-count! m 3)
        (check (qt-standard-model-row-count m) => 5)
        (check (qt-standard-model-column-count m) => 3)
        (qt-standard-model-destroy! m)))

    (test-case "standard model insert/remove row"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 1)))
        (check (qt-standard-model-insert-row! m 1) => #t)
        (check (qt-standard-model-row-count m) => 3)
        (check (qt-standard-model-remove-row! m 0) => #t)
        (check (qt-standard-model-row-count m) => 2)
        (qt-standard-model-destroy! m)))

    (test-case "standard model insert/remove column"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 1 cols: 2)))
        (check (qt-standard-model-insert-column! m 1) => #t)
        (check (qt-standard-model-column-count m) => 3)
        (check (qt-standard-model-remove-column! m 0) => #t)
        (check (qt-standard-model-column-count m) => 2)
        (qt-standard-model-destroy! m)))

    (test-case "standard model clear"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 3 cols: 3)))
        (qt-standard-model-set-item! m 0 0
          (qt-standard-item-create text: "X"))
        (qt-standard-model-clear! m)
        (check (qt-standard-model-row-count m) => 0)
        (check (qt-standard-model-column-count m) => 0)
        (qt-standard-model-destroy! m)))

    ;; --- QStandardItem ---

    (test-case "standard item create and text"
      (ensure-app!)
      (let ((item (qt-standard-item-create text: "Test Item")))
        (check (qt-standard-item-text item) => "Test Item")
        (qt-standard-item-set-text! item "Updated")
        (check (qt-standard-item-text item) => "Updated")))

    (test-case "standard item tooltip"
      (ensure-app!)
      (let ((item (qt-standard-item-create text: "X")))
        (qt-standard-item-set-tooltip! item "Tip")
        (check (qt-standard-item-tooltip item) => "Tip")))

    (test-case "standard item editable"
      (ensure-app!)
      (let ((item (qt-standard-item-create text: "X")))
        ;; Default is editable
        (check (qt-standard-item-editable? item) => #t)
        (qt-standard-item-set-editable! item #f)
        (check (qt-standard-item-editable? item) => #f)))

    (test-case "standard item enabled"
      (ensure-app!)
      (let ((item (qt-standard-item-create text: "X")))
        (check (qt-standard-item-enabled? item) => #t)
        (qt-standard-item-set-enabled! item #f)
        (check (qt-standard-item-enabled? item) => #f)))

    (test-case "standard item selectable"
      (ensure-app!)
      (let ((item (qt-standard-item-create text: "X")))
        (check (qt-standard-item-selectable? item) => #t)
        (qt-standard-item-set-selectable! item #f)
        (check (qt-standard-item-selectable? item) => #f)))

    (test-case "standard item checkable"
      (ensure-app!)
      (let ((item (qt-standard-item-create text: "X")))
        (check (qt-standard-item-checkable? item) => #f)
        (qt-standard-item-set-checkable! item #t)
        (check (qt-standard-item-checkable? item) => #t)
        (qt-standard-item-set-check-state! item QT_CHECKED)
        (check (qt-standard-item-check-state item) => QT_CHECKED)))

    (test-case "standard item tree hierarchy"
      (ensure-app!)
      (let ((m (qt-standard-model-create))
            (parent (qt-standard-item-create text: "Parent"))
            (child (qt-standard-item-create text: "Child")))
        (qt-standard-item-append-row! parent child)
        (check (qt-standard-item-row-count parent) => 1)
        (let ((got-child (qt-standard-item-child parent 0)))
          (check (qt-standard-item-text got-child) => "Child"))
        ;; Add parent to model so model owns everything
        (qt-standard-model-set-item! m 0 0 parent)
        (qt-standard-model-destroy! m)))

    ;; --- QStringListModel ---

    (test-case "string list model create and round-trip"
      (ensure-app!)
      (let ((m (qt-string-list-model-create items: '("Alpha" "Beta" "Gamma"))))
        (check (qt-string-list-model-row-count m) => 3)
        (check (qt-string-list-model-strings m) => '("Alpha" "Beta" "Gamma"))
        (qt-string-list-model-destroy! m)))

    (test-case "string list model set strings"
      (ensure-app!)
      (let ((m (qt-string-list-model-create)))
        (check (qt-string-list-model-row-count m) => 0)
        (qt-string-list-model-set-strings! m '("X" "Y"))
        (check (qt-string-list-model-row-count m) => 2)
        (check (qt-string-list-model-strings m) => '("X" "Y"))
        (qt-string-list-model-destroy! m)))

    (test-case "string list model empty"
      (ensure-app!)
      (let ((m (qt-string-list-model-create)))
        (check (qt-string-list-model-row-count m) => 0)
        (check (qt-string-list-model-strings m) => '())
        (qt-string-list-model-destroy! m)))

    ;; --- Views ---

    (test-case "list view create and set model"
      (ensure-app!)
      (let ((m (qt-string-list-model-create items: '("A" "B" "C")))
            (v (qt-list-view-create)))
        (qt-view-set-model! v m)
        ;; No crash
        (qt-widget-destroy! v)
        (qt-string-list-model-destroy! m)))

    (test-case "table view create and set model"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 3 cols: 2))
            (v (qt-table-view-create)))
        (qt-view-set-model! v m)
        (qt-table-view-set-column-width! v 0 100)
        (qt-table-view-set-row-height! v 0 30)
        (qt-table-view-resize-columns-to-contents! v)
        (qt-table-view-resize-rows-to-contents! v)
        (qt-widget-destroy! v)
        (qt-standard-model-destroy! m)))

    (test-case "tree view create and set model"
      (ensure-app!)
      (let ((m (qt-standard-model-create))
            (v (qt-tree-view-create)))
        (qt-view-set-model! v m)
        (qt-tree-view-expand-all! v)
        (qt-tree-view-collapse-all! v)
        (qt-widget-destroy! v)
        (qt-standard-model-destroy! m)))

    (test-case "view selection mode"
      (ensure-app!)
      (let ((v (qt-table-view-create)))
        (qt-view-set-selection-mode! v QT_SELECT_SINGLE)
        (qt-view-set-selection-behavior! v QT_SELECT_ROWS)
        ;; No crash
        (qt-widget-destroy! v)))

    (test-case "table view hide/show column"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 3))
            (v (qt-table-view-create)))
        (qt-view-set-model! v m)
        (qt-table-view-hide-column! v 1)
        (qt-table-view-show-column! v 1)
        (qt-table-view-hide-row! v 0)
        (qt-table-view-show-row! v 0)
        (qt-widget-destroy! v)
        (qt-standard-model-destroy! m)))

    (test-case "tree view indentation"
      (ensure-app!)
      (let ((v (qt-tree-view-create)))
        (qt-tree-view-set-indentation! v 30)
        (check (qt-tree-view-indentation v) => 30)
        (qt-widget-destroy! v)))

    (test-case "view sorting enabled"
      (ensure-app!)
      (let ((v (qt-table-view-create)))
        (qt-view-set-sorting-enabled! v #t)
        ;; No crash
        (qt-widget-destroy! v)))

    (test-case "view edit triggers"
      (ensure-app!)
      (let ((v (qt-table-view-create)))
        (qt-view-set-edit-triggers! v QT_EDIT_NONE)
        ;; No crash
        (qt-widget-destroy! v)))

    (test-case "view alternating row colors"
      (ensure-app!)
      (let ((v (qt-table-view-create)))
        (qt-view-set-alternating-row-colors! v #t)
        ;; No crash
        (qt-widget-destroy! v)))

    (test-case "tree view root decorated and header hidden"
      (ensure-app!)
      (let ((v (qt-tree-view-create)))
        (qt-tree-view-set-root-is-decorated! v #f)
        (qt-tree-view-set-header-hidden! v #t)
        ;; No crash
        (qt-widget-destroy! v)))

    ;; --- QHeaderView ---

    (test-case "view header stretch last section"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 3))
            (v (qt-table-view-create)))
        (qt-view-set-model! v m)
        (qt-view-header-set-stretch-last-section! v #t)
        (qt-view-header-set-section-resize-mode! v QT_HEADER_STRETCH)
        (qt-view-header-set-default-section-size! v 80)
        ;; No crash
        (qt-widget-destroy! v)
        (qt-standard-model-destroy! m)))

    (test-case "view header hide/show"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 2))
            (v (qt-table-view-create)))
        (qt-view-set-model! v m)
        (qt-view-header-hide! v)
        (qt-view-header-show! v)
        (qt-view-header-hide! v horizontal: #f)
        (qt-view-header-show! v horizontal: #f)
        (qt-widget-destroy! v)
        (qt-standard-model-destroy! m)))

    ;; --- QSortFilterProxyModel ---

    (test-case "sort filter proxy create"
      (ensure-app!)
      (let ((p (qt-sort-filter-proxy-create)))
        (check (not (eq? p #f)) => #t)
        (qt-sort-filter-proxy-destroy! p)))

    (test-case "sort filter proxy with source model"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 3 cols: 1))
            (p (qt-sort-filter-proxy-create)))
        (qt-standard-model-set-item! m 0 0
          (qt-standard-item-create text: "Apple"))
        (qt-standard-model-set-item! m 1 0
          (qt-standard-item-create text: "Banana"))
        (qt-standard-model-set-item! m 2 0
          (qt-standard-item-create text: "Cherry"))
        (qt-sort-filter-proxy-set-source-model! p m)
        (check (qt-sort-filter-proxy-row-count p) => 3)
        (qt-sort-filter-proxy-destroy! p)
        (qt-standard-model-destroy! m)))

    (test-case "sort filter proxy filter regex"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 3 cols: 1))
            (p (qt-sort-filter-proxy-create)))
        (qt-standard-model-set-item! m 0 0
          (qt-standard-item-create text: "Apple"))
        (qt-standard-model-set-item! m 1 0
          (qt-standard-item-create text: "Banana"))
        (qt-standard-model-set-item! m 2 0
          (qt-standard-item-create text: "Avocado"))
        (qt-sort-filter-proxy-set-source-model! p m)
        (qt-sort-filter-proxy-set-filter-regex! p "^A")
        (check (qt-sort-filter-proxy-row-count p) => 2)
        (qt-sort-filter-proxy-destroy! p)
        (qt-standard-model-destroy! m)))

    (test-case "sort filter proxy filter column"
      (ensure-app!)
      (let ((p (qt-sort-filter-proxy-create)))
        (qt-sort-filter-proxy-set-filter-column! p 1)
        (qt-sort-filter-proxy-set-filter-case-sensitivity! p QT_CASE_INSENSITIVE)
        ;; No crash
        (qt-sort-filter-proxy-destroy! p)))

    (test-case "sort filter proxy sort"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 1))
            (p (qt-sort-filter-proxy-create)))
        (qt-standard-model-set-item! m 0 0
          (qt-standard-item-create text: "Banana"))
        (qt-standard-model-set-item! m 1 0
          (qt-standard-item-create text: "Apple"))
        (qt-sort-filter-proxy-set-source-model! p m)
        (qt-sort-filter-proxy-sort! p 0 order: QT_SORT_ASCENDING)
        ;; No crash — sorting happened
        (qt-sort-filter-proxy-destroy! p)
        (qt-standard-model-destroy! m)))

    (test-case "sort filter proxy dynamic sort filter"
      (ensure-app!)
      (let ((p (qt-sort-filter-proxy-create)))
        (qt-sort-filter-proxy-set-dynamic-sort-filter! p #t)
        (qt-sort-filter-proxy-invalidate-filter! p)
        ;; No crash
        (qt-sort-filter-proxy-destroy! p)))

    ;; --- View Signals + Selection ---

    (test-case "view signal registration"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 2))
            (v (qt-table-view-create)))
        (qt-view-set-model! v m)
        (qt-on-view-clicked! v (lambda () #t))
        (qt-on-view-double-clicked! v (lambda () #t))
        (qt-on-view-activated! v (lambda () #t))
        (qt-on-view-selection-changed! v (lambda () #t))
        ;; No crash — signals registered
        (qt-widget-destroy! v)
        (qt-standard-model-destroy! m)))

    (test-case "view query functions initial state"
      (ensure-app!)
      (check (qt-view-last-clicked-row) => -1)
      (check (qt-view-last-clicked-col) => -1))

    (test-case "view selected rows empty"
      (ensure-app!)
      (let ((m (qt-standard-model-create rows: 2 cols: 2))
            (v (qt-table-view-create)))
        (qt-view-set-model! v m)
        (check (qt-view-selected-rows v) => '())
        (check (qt-view-current-row v) => -1)
        (qt-widget-destroy! v)
        (qt-standard-model-destroy! m)))

    ;; --- Phase 12 Constants ---

    (test-case "phase 12 constants"
      (check QT_DISPLAY_ROLE        => 0)
      (check QT_EDIT_ROLE           => 2)
      (check QT_TOOLTIP_ROLE        => 3)
      (check QT_CHECK_STATE_ROLE    => 10)
      (check QT_USER_ROLE           => 256)
      (check QT_SELECT_NONE         => 0)
      (check QT_SELECT_SINGLE       => 1)
      (check QT_SELECT_MULTI        => 2)
      (check QT_SELECT_EXTENDED     => 3)
      (check QT_SELECT_CONTIGUOUS   => 4)
      (check QT_SELECT_ITEMS        => 0)
      (check QT_SELECT_ROWS         => 1)
      (check QT_SELECT_COLUMNS      => 2)
      (check QT_SORT_ASCENDING      => 0)
      (check QT_SORT_DESCENDING     => 1)
      (check QT_UNCHECKED           => 0)
      (check QT_PARTIALLY_CHECKED   => 1)
      (check QT_CHECKED             => 2)
      (check QT_HEADER_INTERACTIVE  => 0)
      (check QT_HEADER_FIXED        => 1)
      (check QT_HEADER_STRETCH      => 2)
      (check QT_HEADER_RESIZE_TO_CONTENTS => 3)
      (check QT_EDIT_NONE           => 0)
      (check QT_EDIT_DOUBLE_CLICKED => 2)
      (check QT_EDIT_ALL_INPUT      => 31))

    ;; ==================== Phase 13: Practical Polish ====================

    ;; --- QValidator ---

    (test-case "int validator create/destroy"
      (ensure-app!)
      (let ((v (qt-int-validator-create 0 100)))
        (check (qt-validator-validate v "50")  => QT_VALIDATOR_ACCEPTABLE)
        (check (qt-validator-validate v "200") => QT_VALIDATOR_INTERMEDIATE)
        (check (qt-validator-validate v "abc") => QT_VALIDATOR_INVALID)
        (qt-validator-destroy! v)))

    (test-case "double validator create/destroy"
      (ensure-app!)
      (let ((v (qt-double-validator-create 0.0 100.0 decimals: 2)))
        (check (qt-validator-validate v "50.5")  => QT_VALIDATOR_ACCEPTABLE)
        (check (qt-validator-validate v "abc")   => QT_VALIDATOR_INVALID)
        (qt-validator-destroy! v)))

    (test-case "regex validator create/destroy"
      (ensure-app!)
      (let ((v (qt-regex-validator-create "^[A-Z]{3}$")))
        (check (qt-validator-validate v "ABC") => QT_VALIDATOR_ACCEPTABLE)
        (check (qt-validator-validate v "ab")  => QT_VALIDATOR_INVALID)
        (qt-validator-destroy! v)))

    (test-case "line edit set validator"
      (ensure-app!)
      (let ((e (qt-line-edit-create))
            (v (qt-int-validator-create 1 100)))
        (qt-line-edit-set-validator! e v)
        (qt-line-edit-set-text! e "50")
        (check (qt-line-edit-acceptable-input? e) => #t)
        (qt-line-edit-set-text! e "")
        ;; Empty string is intermediate for int validator
        (check (qt-line-edit-acceptable-input? e) => #f)
        (qt-widget-destroy! e)
        (qt-validator-destroy! v)))

    (test-case "validator state constants"
      (check QT_VALIDATOR_INVALID      => 0)
      (check QT_VALIDATOR_INTERMEDIATE => 1)
      (check QT_VALIDATOR_ACCEPTABLE   => 2))

    ;; --- QPlainTextEdit ---

    (test-case "plain text edit create"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (check (qt-plain-text-edit-text e) => "")
        (qt-widget-destroy! e)))

    (test-case "plain text edit text round-trip"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (qt-plain-text-edit-set-text! e "Hello World")
        (check (qt-plain-text-edit-text e) => "Hello World")
        (qt-widget-destroy! e)))

    (test-case "plain text edit append/clear"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (qt-plain-text-edit-set-text! e "Line1")
        (qt-plain-text-edit-append! e "Line2")
        (let ((text (qt-plain-text-edit-text e)))
          (check (number? (string-contains text "Line1")) => #t)
          (check (number? (string-contains text "Line2")) => #t))
        (qt-plain-text-edit-clear! e)
        (check (qt-plain-text-edit-text e) => "")
        (qt-widget-destroy! e)))

    (test-case "plain text edit read-only"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (check (qt-plain-text-edit-read-only? e) => #f)
        (qt-plain-text-edit-set-read-only! e #t)
        (check (qt-plain-text-edit-read-only? e) => #t)
        (qt-widget-destroy! e)))

    (test-case "plain text edit placeholder"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (qt-plain-text-edit-set-placeholder! e "Enter text...")
        ;; No crash — placeholder is set
        (qt-widget-destroy! e)))

    (test-case "plain text edit line count"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        ;; Empty editor has 1 block
        (check (>= (qt-plain-text-edit-line-count e) 1) => #t)
        (qt-plain-text-edit-set-text! e "Line1\nLine2\nLine3")
        (check (qt-plain-text-edit-line-count e) => 3)
        (qt-widget-destroy! e)))

    (test-case "plain text edit max block count"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (qt-plain-text-edit-set-max-block-count! e 5)
        ;; Add more than 5 lines
        (qt-plain-text-edit-set-text! e "1\n2\n3\n4\n5\n6\n7")
        (check (<= (qt-plain-text-edit-line-count e) 5) => #t)
        (qt-widget-destroy! e)))

    (test-case "plain text edit cursor position"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (qt-plain-text-edit-set-text! e "Hello")
        ;; Cursor at start after setText
        (check (>= (qt-plain-text-edit-cursor-line e) 0) => #t)
        (check (>= (qt-plain-text-edit-cursor-column e) 0) => #t)
        (qt-widget-destroy! e)))

    (test-case "plain text edit text changed signal"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (qt-on-plain-text-edit-text-changed! e (lambda () #t))
        ;; No crash — signal connected
        (qt-widget-destroy! e)))

    (test-case "plain text edit line wrap modes"
      (ensure-app!)
      (let ((e (qt-plain-text-edit-create)))
        (qt-plain-text-edit-set-line-wrap! e QT_PLAIN_NO_WRAP)
        (qt-plain-text-edit-set-line-wrap! e QT_PLAIN_WIDGET_WRAP)
        ;; No crash
        (qt-widget-destroy! e)))

    (test-case "plain text edit wrap mode constants"
      (check QT_PLAIN_NO_WRAP     => 0)
      (check QT_PLAIN_WIDGET_WRAP => 1))

    ;; --- QToolButton ---

    (test-case "tool button create"
      (ensure-app!)
      (let ((b (qt-tool-button-create)))
        (qt-tool-button-set-text! b "Tool")
        (check (qt-tool-button-text b) => "Tool")
        (qt-widget-destroy! b)))

    (test-case "tool button popup mode"
      (ensure-app!)
      (let ((b (qt-tool-button-create)))
        (qt-tool-button-set-popup-mode! b QT_MENU_BUTTON_POPUP)
        ;; No crash
        (qt-widget-destroy! b)))

    (test-case "tool button auto raise"
      (ensure-app!)
      (let ((b (qt-tool-button-create)))
        (qt-tool-button-set-auto-raise! b #t)
        ;; No crash
        (qt-widget-destroy! b)))

    (test-case "tool button arrow type"
      (ensure-app!)
      (let ((b (qt-tool-button-create)))
        (qt-tool-button-set-arrow-type! b QT_DOWN_ARROW)
        ;; No crash
        (qt-widget-destroy! b)))

    (test-case "tool button style"
      (ensure-app!)
      (let ((b (qt-tool-button-create)))
        (qt-tool-button-set-tool-button-style! b QT_TOOL_BUTTON_TEXT_ONLY)
        ;; No crash
        (qt-widget-destroy! b)))

    (test-case "tool button menu"
      (ensure-app!)
      (let* ((mw (qt-main-window-create))
             (bar (qt-main-window-menu-bar mw))
             (m (qt-menu-bar-add-menu bar "Actions"))
             (b (qt-tool-button-create)))
        (qt-tool-button-set-menu! b m)
        (qt-tool-button-set-popup-mode! b QT_INSTANT_POPUP)
        ;; No crash
        (qt-widget-destroy! b)
        (qt-widget-destroy! mw)))

    (test-case "tool button clicked signal"
      (ensure-app!)
      (let ((b (qt-tool-button-create)))
        (qt-on-tool-button-clicked! b (lambda () #t))
        ;; No crash — signal connected
        (qt-widget-destroy! b)))

    (test-case "tool button popup mode constants"
      (check QT_DELAYED_POPUP     => 0)
      (check QT_MENU_BUTTON_POPUP => 1)
      (check QT_INSTANT_POPUP     => 2))

    (test-case "tool button arrow type constants"
      (check QT_NO_ARROW    => 0)
      (check QT_UP_ARROW    => 1)
      (check QT_DOWN_ARROW  => 2)
      (check QT_LEFT_ARROW  => 3)
      (check QT_RIGHT_ARROW => 4))

    (test-case "tool button style constants"
      (check QT_TOOL_BUTTON_ICON_ONLY        => 0)
      (check QT_TOOL_BUTTON_TEXT_ONLY        => 1)
      (check QT_TOOL_BUTTON_TEXT_BESIDE_ICON => 2)
      (check QT_TOOL_BUTTON_TEXT_UNDER_ICON  => 3))

    ;; --- Layout spacers ---

    (test-case "layout add spacing"
      (ensure-app!)
      (let* ((w (qt-widget-create))
             (layout (qt-vbox-layout-create w)))
        (qt-layout-add-spacing! layout 10)
        (qt-layout-add-spacing! layout 20)
        ;; No crash — spacing items added
        (qt-widget-destroy! w)))

    (test-case "layout stretch and spacing combined"
      (ensure-app!)
      (let* ((w (qt-widget-create))
             (layout (qt-vbox-layout-create w))
             (btn (qt-push-button-create "Test" parent: w)))
        (qt-layout-add-stretch! layout)
        (qt-layout-add-spacing! layout 10)
        (qt-layout-add-widget! layout btn)
        ;; No crash
        (qt-widget-destroy! w)))

    ;; --- QSizePolicy ---

    (test-case "widget set size policy"
      (ensure-app!)
      (let ((w (qt-widget-create)))
        (qt-widget-set-size-policy! w QT_SIZE_EXPANDING QT_SIZE_PREFERRED)
        ;; No crash
        (qt-widget-destroy! w)))

    (test-case "layout set stretch factor"
      (ensure-app!)
      (let* ((w (qt-widget-create))
             (layout (qt-vbox-layout-create w))
             (btn1 (qt-push-button-create "A" parent: w))
             (btn2 (qt-push-button-create "B" parent: w)))
        (qt-layout-add-widget! layout btn1)
        (qt-layout-add-widget! layout btn2)
        (qt-layout-set-stretch-factor! layout btn1 1)
        (qt-layout-set-stretch-factor! layout btn2 2)
        ;; No crash
        (qt-widget-destroy! w)))

    (test-case "size policy constants"
      (check QT_SIZE_FIXED             => 0)
      (check QT_SIZE_MINIMUM           => 1)
      (check QT_SIZE_MINIMUM_EXPANDING => 3)
      (check QT_SIZE_MAXIMUM           => 4)
      (check QT_SIZE_PREFERRED         => 5)
      (check QT_SIZE_EXPANDING         => 7)
      (check QT_SIZE_IGNORED           => 13))

    ;; ========== Phase 14: Graphics Scene & Custom Painting ==========

    (test-case "graphics scene create and destroy"
      (ensure-app!)
      (let ((scene (qt-graphics-scene-create 0 0 800 600)))
        (check (not (eq? scene #f)) => #t)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics scene add shapes"
      (ensure-app!)
      (let ((scene (qt-graphics-scene-create 0 0 800 600)))
        (check (qt-graphics-scene-items-count scene) => 0)
        (let ((rect (qt-graphics-scene-add-rect! scene 10 20 100 50))
              (ellipse (qt-graphics-scene-add-ellipse! scene 200 100 80 60))
              (line (qt-graphics-scene-add-line! scene 0 0 100 100))
              (text (qt-graphics-scene-add-text! scene "Hello")))
          (check (qt-graphics-scene-items-count scene) => 4)
          (check (not (eq? rect #f)) => #t)
          (check (not (eq? ellipse #f)) => #t)
          (check (not (eq? line #f)) => #t)
          (check (not (eq? text #f)) => #t))
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics scene remove item"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (rect (qt-graphics-scene-add-rect! scene 0 0 50 50)))
        (check (qt-graphics-scene-items-count scene) => 1)
        (qt-graphics-scene-remove-item! scene rect)
        (check (qt-graphics-scene-items-count scene) => 0)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics scene clear"
      (ensure-app!)
      (let ((scene (qt-graphics-scene-create 0 0 400 300)))
        (qt-graphics-scene-add-rect! scene 0 0 50 50)
        (qt-graphics-scene-add-ellipse! scene 100 100 30 30)
        (check (qt-graphics-scene-items-count scene) => 2)
        (qt-graphics-scene-clear! scene)
        (check (qt-graphics-scene-items-count scene) => 0)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics scene set background"
      (ensure-app!)
      (let ((scene (qt-graphics-scene-create 0 0 400 300)))
        ;; No crash
        (qt-graphics-scene-set-background! scene 30 30 30)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics view create"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (view (qt-graphics-view-create scene)))
        (check (not (eq? view #f)) => #t)
        (qt-widget-destroy! view)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics view render hints and drag mode"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (view (qt-graphics-view-create scene)))
        ;; No crash
        (qt-graphics-view-set-render-hint! view QT_RENDER_ANTIALIASING)
        (qt-graphics-view-set-drag-mode! view QT_DRAG_RUBBER_BAND)
        (qt-widget-destroy! view)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics view scale and center"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (view (qt-graphics-view-create scene)))
        ;; No crash
        (qt-graphics-view-scale! view 2.0 2.0)
        (qt-graphics-view-center-on! view 200 150)
        (qt-graphics-view-fit-in-view! view)
        (qt-widget-destroy! view)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics item position"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (rect (qt-graphics-scene-add-rect! scene 0 0 50 50)))
        (qt-graphics-item-set-pos! rect 100 200)
        (check (qt-graphics-item-x rect) => 100.0)
        (check (qt-graphics-item-y rect) => 200.0)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics item pen and brush"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (rect (qt-graphics-scene-add-rect! scene 0 0 50 50)))
        ;; No crash
        (qt-graphics-item-set-pen! rect 255 0 0 width: 2)
        (qt-graphics-item-set-brush! rect 0 128 255)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics item flags"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (rect (qt-graphics-scene-add-rect! scene 0 0 50 50)))
        ;; No crash
        (qt-graphics-item-set-flags! rect
          (bitwise-ior QT_ITEM_MOVABLE QT_ITEM_SELECTABLE))
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics item zvalue"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (rect (qt-graphics-scene-add-rect! scene 0 0 50 50)))
        (qt-graphics-item-set-zvalue! rect 5.0)
        (check (qt-graphics-item-zvalue rect) => 5.0)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics item rotation and scale"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (rect (qt-graphics-scene-add-rect! scene 0 0 50 50)))
        ;; No crash
        (qt-graphics-item-set-rotation! rect 45.0)
        (qt-graphics-item-set-scale! rect 2.0)
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics item visibility and tooltip"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (rect (qt-graphics-scene-add-rect! scene 0 0 50 50)))
        ;; No crash
        (qt-graphics-item-set-visible! rect #f)
        (qt-graphics-item-set-tooltip! rect "A rectangle")
        (qt-graphics-scene-destroy! scene)))

    (test-case "graphics scene add pixmap"
      (ensure-app!)
      (let* ((scene (qt-graphics-scene-create 0 0 400 300))
             (pixmap (qt-pixmap-create-blank 32 32)))
        (qt-pixmap-fill! pixmap 255 0 0)
        (let ((item (qt-graphics-scene-add-pixmap! scene pixmap)))
          (check (not (eq? item #f)) => #t)
          (check (qt-graphics-scene-items-count scene) => 1))
        (qt-graphics-scene-destroy! scene)
        (qt-pixmap-destroy! pixmap)))

    (test-case "paint widget create"
      (ensure-app!)
      (let ((pw (qt-paint-widget-create)))
        (check (not (eq? pw #f)) => #t)
        (qt-widget-destroy! pw)))

    (test-case "paint widget callback and update"
      (ensure-app!)
      (let ((pw (qt-paint-widget-create))
            (painted? #f))
        (qt-paint-widget-on-paint! pw
          (lambda ()
            (set! painted? #t)))
        ;; Request a repaint — callback fires during repaint cycle
        (qt-paint-widget-update! pw)
        ;; In offscreen mode we can process events to trigger the paint
        (qt-app-process-events! test-app)
        ;; The paint callback may or may not fire in offscreen (depends on
        ;; visibility), so just verify no crash
        (qt-widget-destroy! pw)))

    (test-case "paint widget dimensions"
      (ensure-app!)
      (let ((pw (qt-paint-widget-create)))
        (qt-widget-set-minimum-size! pw 200 150)
        ;; Width/height may not match minimum size until shown,
        ;; but the call should not crash
        (check (>= (qt-paint-widget-width pw) 0) => #t)
        (check (>= (qt-paint-widget-height pw) 0) => #t)
        (qt-widget-destroy! pw)))

    (test-case "Phase 14 constants"
      (check QT_ITEM_MOVABLE     => #x1)
      (check QT_ITEM_SELECTABLE  => #x2)
      (check QT_ITEM_FOCUSABLE   => #x4)
      (check QT_DRAG_NONE        => 0)
      (check QT_DRAG_SCROLL      => 1)
      (check QT_DRAG_RUBBER_BAND => 2)
      (check QT_RENDER_ANTIALIASING     => #x01)
      (check QT_RENDER_SMOOTH_PIXMAP    => #x02)
      (check QT_RENDER_TEXT_ANTIALIASING => #x04))

    ;; ==================== Phase 15: Process, Wizard, MDI ====================

    (test-case "QProcess create and destroy"
      (ensure-app!)
      (let ((proc (qt-process-create)))
        (check (qt-process-state proc) => QT_PROCESS_NOT_RUNNING)
        (qt-process-destroy! proc)))

    (test-case "QProcess run echo"
      (ensure-app!)
      (let ((proc (qt-process-create)))
        (qt-process-start! proc "/bin/echo" args: ["hello world"])
        (check (qt-process-wait-for-finished proc msecs: 5000) => #t)
        (check (qt-process-exit-code proc) => 0)
        (let ((out (qt-process-read-stdout proc)))
          (check (string-prefix? "hello world" out) => #t))
        (qt-process-destroy! proc)))

    (test-case "QProcess run with exit code"
      (ensure-app!)
      (let ((proc (qt-process-create)))
        (qt-process-start! proc "/bin/sh" args: ["-c" "exit 42"])
        (check (qt-process-wait-for-finished proc msecs: 5000) => #t)
        (qt-app-process-events! test-app)
        (qt-app-process-events! test-app)
        (check (qt-process-exit-code proc) => 42)
        (qt-process-destroy! proc)))

    (test-case "QProcess read stderr"
      (ensure-app!)
      (let ((proc (qt-process-create)))
        (qt-process-start! proc "/bin/sh" args: ["-c" "echo err >&2"])
        (check (qt-process-wait-for-finished proc msecs: 5000) => #t)
        (let ((err (qt-process-read-stderr proc)))
          (check (string-prefix? "err" err) => #t))
        (qt-process-destroy! proc)))

    (test-case "QProcess write stdin"
      (ensure-app!)
      (let ((proc (qt-process-create)))
        (qt-process-start! proc "/bin/cat" args: [])
        (qt-process-write! proc "test input")
        (qt-process-close-write! proc)
        (check (qt-process-wait-for-finished proc msecs: 5000) => #t)
        (let ((out (qt-process-read-stdout proc)))
          (check (string-prefix? "test input" out) => #t))
        (qt-process-destroy! proc)))

    (test-case "QProcess state"
      (ensure-app!)
      (let ((proc (qt-process-create)))
        (check (qt-process-state proc) => QT_PROCESS_NOT_RUNNING)
        (qt-process-start! proc "/bin/sleep" args: ["10"])
        ;; Process should be starting or running
        (check (> (qt-process-state proc) QT_PROCESS_NOT_RUNNING) => #t)
        (qt-process-kill! proc)
        (qt-process-wait-for-finished proc msecs: 5000)
        (qt-process-destroy! proc)))

    (test-case "QProcess on-finished callback"
      (ensure-app!)
      (let ((proc (qt-process-create))
            (finished-code #f))
        (qt-process-on-finished! proc (lambda (code) (set! finished-code code)))
        (qt-process-start! proc "/bin/sh" args: ["-c" "exit 7"])
        (qt-process-wait-for-finished proc msecs: 5000)
        (qt-app-process-events! test-app)
        (qt-app-process-events! test-app)
        (check finished-code => 7)
        (qt-process-destroy! proc)))

    (test-case "QWizard create and pages"
      (ensure-app!)
      (let ((wiz (qt-wizard-create))
            (p1 (qt-wizard-page-create))
            (p2 (qt-wizard-page-create)))
        (qt-wizard-page-set-title! p1 "Page 1")
        (qt-wizard-page-set-subtitle! p1 "First page")
        (qt-wizard-page-set-title! p2 "Page 2")
        (let ((id1 (qt-wizard-add-page! wiz p1))
              (id2 (qt-wizard-add-page! wiz p2)))
          (check (integer? id1) => #t)
          (check (integer? id2) => #t)
          (check (not (= id1 id2)) => #t))
        (qt-wizard-set-title! wiz "Test Wizard")
        (qt-widget-destroy! wiz)))

    (test-case "QWizard set start id"
      (ensure-app!)
      (let ((wiz (qt-wizard-create))
            (p1 (qt-wizard-page-create))
            (p2 (qt-wizard-page-create)))
        (let ((id1 (qt-wizard-add-page! wiz p1))
              (id2 (qt-wizard-add-page! wiz p2)))
          (qt-wizard-set-start-id! wiz id2)
          ;; current-id is only valid after show/exec, just check no crash
          (check (integer? (qt-wizard-current-id wiz)) => #t))
        (qt-widget-destroy! wiz)))

    (test-case "QWizard page layout"
      (ensure-app!)
      (let* ((wiz (qt-wizard-create))
             (page (qt-wizard-page-create))
             (layout (qt-vbox-layout-create page)))
        (qt-wizard-page-set-title! page "Layout Test")
        (qt-wizard-add-page! wiz page)
        (qt-widget-destroy! wiz)))

    (test-case "QMdiArea create and sub-windows"
      (ensure-app!)
      (let ((mdi (qt-mdi-area-create))
            (w1 (qt-widget-create))
            (w2 (qt-widget-create)))
        (check (qt-mdi-area-sub-window-count mdi) => 0)
        (let ((sub1 (qt-mdi-area-add-sub-window! mdi w1))
              (sub2 (qt-mdi-area-add-sub-window! mdi w2)))
          (check (qt-mdi-area-sub-window-count mdi) => 2)
          (qt-mdi-sub-window-set-title! sub1 "Doc 1")
          (qt-mdi-sub-window-set-title! sub2 "Doc 2")
          ;; Cascade and tile should not crash
          (qt-mdi-area-cascade! mdi)
          (qt-mdi-area-tile! mdi))
        (qt-widget-destroy! mdi)))

    (test-case "QMdiArea view mode"
      (ensure-app!)
      (let ((mdi (qt-mdi-area-create)))
        (qt-mdi-area-set-view-mode! mdi QT_MDI_TABBED)
        (qt-mdi-area-set-view-mode! mdi QT_MDI_SUBWINDOW)
        (qt-widget-destroy! mdi)))

    (test-case "QMdiArea active sub-window"
      (ensure-app!)
      (let ((mdi (qt-mdi-area-create))
            (w1 (qt-widget-create)))
        (qt-mdi-area-add-sub-window! mdi w1)
        ;; Active sub-window is #f when not shown in offscreen mode
        (let ((active (qt-mdi-area-active-sub-window mdi)))
          (check (not active) => #t))
        (qt-widget-destroy! mdi)))

    (test-case "QMdiArea remove sub-window"
      (ensure-app!)
      (let ((mdi (qt-mdi-area-create))
            (w1 (qt-widget-create)))
        (let ((sub (qt-mdi-area-add-sub-window! mdi w1)))
          (check (qt-mdi-area-sub-window-count mdi) => 1)
          (qt-mdi-area-remove-sub-window! mdi sub)
          (check (qt-mdi-area-sub-window-count mdi) => 0))
        (qt-widget-destroy! mdi)))

    (test-case "Phase 15 constants"
      (check QT_PROCESS_NOT_RUNNING => 0)
      (check QT_PROCESS_STARTING    => 1)
      (check QT_PROCESS_RUNNING     => 2)
      (check QT_MDI_SUBWINDOW       => 0)
      (check QT_MDI_TABBED          => 1))

    ;; ==================== Phase 16: Niche Widgets ====================

    (test-case "QDial create and value"
      (ensure-app!)
      (let ((dial (qt-dial-create)))
        (qt-dial-set-value! dial 50)
        (check (qt-dial-value dial) => 50)
        (qt-dial-set-value! dial 75)
        (check (qt-dial-value dial) => 75)
        (qt-widget-destroy! dial)))

    (test-case "QDial range"
      (ensure-app!)
      (let ((dial (qt-dial-create)))
        (qt-dial-set-range! dial 0 200)
        (qt-dial-set-value! dial 150)
        (check (qt-dial-value dial) => 150)
        ;; Value should clamp to range
        (qt-dial-set-value! dial 250)
        (check (qt-dial-value dial) => 200)
        (qt-widget-destroy! dial)))

    (test-case "QDial notches and wrapping"
      (ensure-app!)
      (let ((dial (qt-dial-create)))
        (qt-dial-set-notches-visible! dial #t)
        (qt-dial-set-wrapping! dial #t)
        (qt-widget-destroy! dial)))

    (test-case "QDial signal registration"
      (ensure-app!)
      (let ((dial (qt-dial-create)))
        (qt-dial-on-value-changed! dial (lambda (val) #t))
        (qt-widget-destroy! dial)))

    (test-case "QLCDNumber create and display"
      (ensure-app!)
      (let ((lcd (qt-lcd-create digits: 6)))
        (qt-lcd-display-int! lcd 42)
        (qt-lcd-display-double! lcd 3.14)
        (qt-lcd-display-string! lcd "12.5")
        (qt-widget-destroy! lcd)))

    (test-case "QLCDNumber mode and segment style"
      (ensure-app!)
      (let ((lcd (qt-lcd-create)))
        (qt-lcd-set-mode! lcd QT_LCD_HEX)
        (qt-lcd-set-mode! lcd QT_LCD_DEC)
        (qt-lcd-set-segment-style! lcd QT_LCD_FLAT)
        (qt-lcd-set-segment-style! lcd QT_LCD_FILLED)
        (qt-widget-destroy! lcd)))

    (test-case "QToolBox create and add items"
      (ensure-app!)
      (let ((tb (qt-tool-box-create))
            (w1 (qt-widget-create))
            (w2 (qt-widget-create)))
        (check (qt-tool-box-count tb) => 0)
        (let ((idx1 (qt-tool-box-add-item! tb w1 "Page 1"))
              (idx2 (qt-tool-box-add-item! tb w2 "Page 2")))
          (check (qt-tool-box-count tb) => 2)
          (check (integer? idx1) => #t)
          (check (integer? idx2) => #t))
        (qt-widget-destroy! tb)))

    (test-case "QToolBox current index"
      (ensure-app!)
      (let ((tb (qt-tool-box-create))
            (w1 (qt-widget-create))
            (w2 (qt-widget-create)))
        (qt-tool-box-add-item! tb w1 "First")
        (qt-tool-box-add-item! tb w2 "Second")
        (qt-tool-box-set-current-index! tb 1)
        (check (qt-tool-box-current-index tb) => 1)
        (qt-tool-box-set-current-index! tb 0)
        (check (qt-tool-box-current-index tb) => 0)
        (qt-widget-destroy! tb)))

    (test-case "QToolBox set item text"
      (ensure-app!)
      (let ((tb (qt-tool-box-create))
            (w1 (qt-widget-create)))
        (qt-tool-box-add-item! tb w1 "Original")
        (qt-tool-box-set-item-text! tb 0 "Updated")
        (qt-widget-destroy! tb)))

    (test-case "QToolBox signal registration"
      (ensure-app!)
      (let ((tb (qt-tool-box-create)))
        (qt-tool-box-on-current-changed! tb (lambda (idx) #t))
        (qt-widget-destroy! tb)))

    (test-case "QUndoStack create and destroy"
      (ensure-app!)
      (let ((stack (qt-undo-stack-create)))
        (check (qt-undo-stack-can-undo? stack) => #f)
        (check (qt-undo-stack-can-redo? stack) => #f)
        (qt-undo-stack-destroy! stack)))

    (test-case "QUndoStack push and undo/redo"
      (ensure-app!)
      (let ((stack (qt-undo-stack-create))
            (value 0))
        ;; Push a command: redo sets to 10, undo restores to 0
        (qt-undo-stack-push! stack "Set to 10"
          (lambda () (set! value 0))    ;; undo
          (lambda () (set! value 10)))  ;; redo
        ;; After push, redo fires immediately
        (check value => 10)
        (check (qt-undo-stack-can-undo? stack) => #t)
        (check (qt-undo-stack-can-redo? stack) => #f)
        ;; Undo
        (qt-undo-stack-undo! stack)
        (check value => 0)
        (check (qt-undo-stack-can-redo? stack) => #t)
        ;; Redo
        (qt-undo-stack-redo! stack)
        (check value => 10)
        (qt-undo-stack-destroy! stack)))

    (test-case "QUndoStack undo/redo text"
      (ensure-app!)
      (let ((stack (qt-undo-stack-create)))
        (qt-undo-stack-push! stack "Do thing"
          (lambda () #t) (lambda () #t))
        (check (string-prefix? "Do thing" (qt-undo-stack-undo-text stack)) => #t)
        (qt-undo-stack-destroy! stack)))

    (test-case "QUndoStack clear"
      (ensure-app!)
      (let ((stack (qt-undo-stack-create)))
        (qt-undo-stack-push! stack "Action 1"
          (lambda () #t) (lambda () #t))
        (check (qt-undo-stack-can-undo? stack) => #t)
        (qt-undo-stack-clear! stack)
        (check (qt-undo-stack-can-undo? stack) => #f)
        (qt-undo-stack-destroy! stack)))

    (test-case "QUndoStack create actions"
      (ensure-app!)
      (let* ((win (qt-main-window-create))
             (stack (qt-undo-stack-create)))
        (let ((undo-action (qt-undo-stack-create-undo-action stack parent: win))
              (redo-action (qt-undo-stack-create-redo-action stack parent: win)))
          (check (not (eq? undo-action #f)) => #t)
          (check (not (eq? redo-action #f)) => #t))
        (qt-undo-stack-destroy! stack)
        (qt-widget-destroy! win)))

    (test-case "QFileSystemModel create and destroy"
      (ensure-app!)
      (let ((model (qt-file-system-model-create)))
        (qt-file-system-model-set-root-path! model "/tmp")
        (qt-file-system-model-destroy! model)))

    (test-case "QFileSystemModel with tree view"
      (ensure-app!)
      (let ((model (qt-file-system-model-create))
            (view (qt-tree-view-create)))
        (qt-file-system-model-set-root-path! model "/tmp")
        (qt-tree-view-set-file-system-root! view model "/tmp")
        (qt-file-system-model-destroy! model)
        (qt-widget-destroy! view)))

    (test-case "QFileSystemModel filter"
      (ensure-app!)
      (let ((model (qt-file-system-model-create)))
        (qt-file-system-model-set-root-path! model "/tmp")
        (qt-file-system-model-set-filter! model
          (bitwise-ior QT_DIR_DIRS QT_DIR_FILES QT_DIR_NO_DOT_AND_DOT_DOT))
        (qt-file-system-model-set-name-filters! model ["*.txt" "*.md"])
        (qt-file-system-model-destroy! model)))

    (test-case "Phase 16 constants"
      (check QT_LCD_DEC     => 0)
      (check QT_LCD_HEX     => 1)
      (check QT_LCD_OCT     => 2)
      (check QT_LCD_BIN     => 3)
      (check QT_LCD_OUTLINE => 0)
      (check QT_LCD_FILLED  => 1)
      (check QT_LCD_FLAT    => 2)
      (check QT_DIR_DIRS    => 1)
      (check QT_DIR_FILES   => 2))

    ;; ==================== Review Fixes ====================

    (test-case "unregister-qt-handler! removes callback"
      (ensure-app!)
      (let* ((counter 0)
             (id (register-qt-void-handler! (lambda () (set! counter (+ counter 1))))))
        ;; Handler is registered
        (let ((handler (hash-ref *qt-void-handlers* id #f)))
          (check (not (eq? handler #f)) => #t))
        ;; Unregister it
        (unregister-qt-handler! id)
        ;; Handler is gone from all tables
        (check (hash-ref *qt-void-handlers* id #f) => #f)
        (check (hash-ref *qt-string-handlers* id #f) => #f)
        (check (hash-ref *qt-int-handlers* id #f) => #f)
        (check (hash-ref *qt-bool-handlers* id #f) => #f)))

    (test-case "with-painter cleans up"
      (ensure-app!)
      (with-pixmap (pm 100 100)
        (with-painter (p pm)
          (qt-painter-set-pen-color! p 255 0 0)
          (qt-painter-draw-line! p 0 0 100 100))
        ;; After with-painter, painter is ended and destroyed.
        ;; Pixmap is still usable.
        (check (qt-pixmap-width pm) => 100)))

    (test-case "with-font cleans up"
      (ensure-app!)
      (with-font (f "Monospace" point-size: 12)
        (check (string? (qt-font-family f)) => #t)))

    (test-case "with-color cleans up"
      (ensure-app!)
      (with-color (c 255 0 128)
        (check (qt-color-red c) => 255)
        (check (qt-color-green c) => 0)
        (check (qt-color-blue c) => 128)))

    (test-case "with-pixmap cleans up"
      (ensure-app!)
      (with-pixmap (pm 200 150)
        (check (qt-pixmap-width pm) => 200)
        (check (qt-pixmap-height pm) => 150)))

  ))
