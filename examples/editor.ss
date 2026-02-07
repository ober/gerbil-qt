#!/usr/bin/env gxi
;;; Simple text editor demonstrating menus, toolbar, keyboard shortcuts,
;;; and status bar (Phase 3 features).

(import :gerbil-qt/qt)

(def (main)
  (let* ((app (qt-app-create))
         (win (qt-main-window-create))
         (editor (qt-text-edit-create))

         ;; ---- Actions ----
         ;; File actions
         (act-new   (qt-action-create "&New" parent: win))
         (act-open  (qt-action-create "&Open..." parent: win))
         (act-save  (qt-action-create "&Save..." parent: win))
         (act-quit  (qt-action-create "&Quit" parent: win))

         ;; Edit actions
         (act-cut   (qt-action-create "Cu&t" parent: win))
         (act-copy  (qt-action-create "&Copy" parent: win))
         (act-paste (qt-action-create "&Paste" parent: win))
         (act-clear (qt-action-create "Clear &All" parent: win))

         ;; View actions
         (act-word-wrap (qt-action-create "&Word Wrap" parent: win))

         ;; ---- Menu Bar ----
         (menu-bar   (qt-main-window-menu-bar win))
         (file-menu  (qt-menu-bar-add-menu menu-bar "&File"))
         (edit-menu  (qt-menu-bar-add-menu menu-bar "&Edit"))
         (view-menu  (qt-menu-bar-add-menu menu-bar "&View"))

         ;; ---- Toolbar ----
         (toolbar (qt-toolbar-create "Main" parent: win))

         ;; State
         (current-file #f))

    ;; ---- Keyboard shortcuts ----
    (qt-action-set-shortcut! act-new   "Ctrl+N")
    (qt-action-set-shortcut! act-open  "Ctrl+O")
    (qt-action-set-shortcut! act-save  "Ctrl+S")
    (qt-action-set-shortcut! act-quit  "Ctrl+Q")
    (qt-action-set-shortcut! act-cut   "Ctrl+X")
    (qt-action-set-shortcut! act-copy  "Ctrl+C")
    (qt-action-set-shortcut! act-paste "Ctrl+V")

    ;; ---- Status tips ----
    (qt-action-set-status-tip! act-new   "Create a new document")
    (qt-action-set-status-tip! act-open  "Open an existing file")
    (qt-action-set-status-tip! act-save  "Save the current document")
    (qt-action-set-status-tip! act-quit  "Quit the editor")

    ;; ---- Checkable action ----
    (qt-action-set-checkable! act-word-wrap #t)
    (qt-action-set-checked! act-word-wrap #t)

    ;; ---- Build File menu ----
    (qt-menu-add-action! file-menu act-new)
    (qt-menu-add-action! file-menu act-open)
    (qt-menu-add-action! file-menu act-save)
    (qt-menu-add-separator! file-menu)
    (qt-menu-add-action! file-menu act-quit)

    ;; ---- Build Edit menu ----
    (qt-menu-add-action! edit-menu act-cut)
    (qt-menu-add-action! edit-menu act-copy)
    (qt-menu-add-action! edit-menu act-paste)
    (qt-menu-add-separator! edit-menu)
    (qt-menu-add-action! edit-menu act-clear)

    ;; ---- Build View menu ----
    (qt-menu-add-action! view-menu act-word-wrap)

    ;; ---- Build Toolbar ----
    (qt-main-window-add-toolbar! win toolbar)
    (qt-toolbar-add-action! toolbar act-new)
    (qt-toolbar-add-action! toolbar act-open)
    (qt-toolbar-add-action! toolbar act-save)
    (qt-toolbar-add-separator! toolbar)
    (qt-toolbar-add-action! toolbar act-cut)
    (qt-toolbar-add-action! toolbar act-copy)
    (qt-toolbar-add-action! toolbar act-paste)

    ;; ---- Signal handlers ----

    ;; New
    (qt-on-triggered! act-new
      (lambda ()
        (qt-text-edit-set-text! editor "")
        (set! current-file #f)
        (qt-main-window-set-title! win "Untitled — Qt Editor")
        (qt-main-window-set-status-bar-text! win "New document")))

    ;; Open
    (qt-on-triggered! act-open
      (lambda ()
        (let ((path (qt-file-dialog-open-file win
                      caption: "Open File"
                      filter: "Text Files (*.txt);;All Files (*)")))
          (when (not (string=? path ""))
            (let ((content (call-with-input-file path
                             (lambda (p) (read-line p #f)))))
              (qt-text-edit-set-text! editor (or content ""))
              (set! current-file path)
              (qt-main-window-set-title! win
                (string-append path " — Qt Editor"))
              (qt-main-window-set-status-bar-text! win
                (string-append "Opened " path)))))))

    ;; Save
    (qt-on-triggered! act-save
      (lambda ()
        (let ((path (or current-file
                        (qt-file-dialog-save-file win
                          caption: "Save File"
                          filter: "Text Files (*.txt);;All Files (*)"))))
          (when (not (string=? path ""))
            (call-with-output-file path
              (lambda (p) (display (qt-text-edit-text editor) p)))
            (set! current-file path)
            (qt-main-window-set-title! win
              (string-append path " — Qt Editor"))
            (qt-main-window-set-status-bar-text! win
              (string-append "Saved " path))))))

    ;; Quit
    (qt-on-triggered! act-quit
      (lambda () (qt-widget-close! win)))

    ;; Clear
    (qt-on-triggered! act-clear
      (lambda ()
        (qt-text-edit-clear! editor)
        (qt-main-window-set-status-bar-text! win "Cleared")))

    ;; Word wrap toggle
    (qt-on-action-toggled! act-word-wrap
      (lambda (checked)
        (qt-main-window-set-status-bar-text! win
          (if checked "Word wrap ON" "Word wrap OFF"))))

    ;; Track edits in status bar
    (qt-on-text-edit-changed! editor
      (lambda ()
        (qt-main-window-set-status-bar-text! win "Modified")))

    ;; ---- Window setup ----
    (qt-main-window-set-title! win "Untitled — Qt Editor")
    (qt-main-window-set-central-widget! win editor)
    (qt-widget-resize! win 700 500)
    (qt-main-window-set-status-bar-text! win "Ready")
    (qt-widget-show! win)

    (qt-app-exec! app)))

(main)
