#!/usr/bin/env gxi
;;; mdi.ss — Multi-document interface with cascade/tile/tabbed modes.
;;;
;;; Demonstrates: QMdiArea, QMdiSubWindow, cascade/tile arrangement,
;;; view mode switching (subwindow vs tabbed), sub-window activation signal,
;;; add/remove sub-windows, QUndoStack integration with QAction.

(import :gerbil-qt/qt
        :std/format)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (mdi (qt-mdi-area-create))
           (doc-counter 0)
           (undo-stack (qt-undo-stack-create))
           (status-text ""))

      ;; --- Menu Bar ---
      (let* ((mbar (qt-main-window-menu-bar win))
             (file-menu (qt-menu-bar-add-menu mbar "File"))
             (window-menu (qt-menu-bar-add-menu mbar "Window"))
             (edit-menu (qt-menu-bar-add-menu mbar "Edit")))

        ;; File > New Document
        (let ((new-action (qt-action-create "New Document" parent: win)))
          (qt-action-set-shortcut! new-action "Ctrl+N")
          (qt-on-triggered! new-action
            (lambda ()
              (set! doc-counter (+ doc-counter 1))
              (let* ((editor (qt-text-edit-create))
                     (sub (qt-mdi-area-add-sub-window! mdi editor)))
                (qt-mdi-sub-window-set-title! sub
                  (format "Document ~a" doc-counter))
                (qt-text-edit-set-text! editor
                  (format "This is document ~a.\nStart typing here..." doc-counter))
                (qt-widget-show! sub)
                (qt-main-window-set-status-bar-text! win
                  (format "Created Document ~a (~a open)"
                    doc-counter (qt-mdi-area-sub-window-count mdi))))))
          (qt-menu-add-action! file-menu new-action))

        ;; File > Close Active
        (let ((close-action (qt-action-create "Close Active" parent: win)))
          (qt-action-set-shortcut! close-action "Ctrl+W")
          (qt-on-triggered! close-action
            (lambda ()
              (let ((active (qt-mdi-area-active-sub-window mdi)))
                (when active
                  (qt-mdi-area-remove-sub-window! mdi active)
                  (qt-main-window-set-status-bar-text! win
                    (format "~a documents open"
                      (qt-mdi-area-sub-window-count mdi)))))))
          (qt-menu-add-action! file-menu close-action))

        (qt-menu-add-separator! file-menu)

        ;; File > Quit
        (let ((quit-action (qt-action-create "Quit" parent: win)))
          (qt-action-set-shortcut! quit-action "Ctrl+Q")
          (qt-on-triggered! quit-action
            (lambda () (qt-app-quit! app)))
          (qt-menu-add-action! file-menu quit-action))

        ;; Window > Cascade
        (let ((cascade-action (qt-action-create "Cascade" parent: win)))
          (qt-on-triggered! cascade-action
            (lambda () (qt-mdi-area-cascade! mdi)))
          (qt-menu-add-action! window-menu cascade-action))

        ;; Window > Tile
        (let ((tile-action (qt-action-create "Tile" parent: win)))
          (qt-on-triggered! tile-action
            (lambda () (qt-mdi-area-tile! mdi)))
          (qt-menu-add-action! window-menu tile-action))

        (qt-menu-add-separator! window-menu)

        ;; Window > Subwindow Mode
        (let ((sub-mode (qt-action-create "Subwindow Mode" parent: win)))
          (qt-on-triggered! sub-mode
            (lambda ()
              (qt-mdi-area-set-view-mode! mdi QT_MDI_SUBWINDOW)
              (qt-main-window-set-status-bar-text! win "View: Subwindow mode")))
          (qt-menu-add-action! window-menu sub-mode))

        ;; Window > Tabbed Mode
        (let ((tab-mode (qt-action-create "Tabbed Mode" parent: win)))
          (qt-on-triggered! tab-mode
            (lambda ()
              (qt-mdi-area-set-view-mode! mdi QT_MDI_TABBED)
              (qt-main-window-set-status-bar-text! win "View: Tabbed mode")))
          (qt-menu-add-action! window-menu tab-mode))

        ;; Edit > Undo/Redo actions from stack
        (let ((undo-action (qt-undo-stack-create-undo-action undo-stack win))
              (redo-action (qt-undo-stack-create-redo-action undo-stack win)))
          (qt-action-set-shortcut! undo-action "Ctrl+Z")
          (qt-action-set-shortcut! redo-action "Ctrl+Shift+Z")
          (qt-menu-add-action! edit-menu undo-action)
          (qt-menu-add-action! edit-menu redo-action)))

      ;; --- Sub-window activated signal ---
      (qt-mdi-area-on-sub-window-activated! mdi
        (lambda ()
          (let ((count (qt-mdi-area-sub-window-count mdi)))
            (qt-main-window-set-status-bar-text! win
              (format "~a documents open" count)))))

      ;; --- Toolbar ---
      (let ((toolbar (qt-toolbar-create "Quick Actions")))
        (let ((new-btn (qt-action-create "New" parent: win))
              (cascade-btn (qt-action-create "Cascade" parent: win))
              (tile-btn (qt-action-create "Tile" parent: win))
              (tab-btn (qt-action-create "Tabbed" parent: win)))
          (qt-on-triggered! new-btn
            (lambda ()
              (set! doc-counter (+ doc-counter 1))
              (let* ((editor (qt-text-edit-create))
                     (sub (qt-mdi-area-add-sub-window! mdi editor)))
                (qt-mdi-sub-window-set-title! sub
                  (format "Document ~a" doc-counter))
                (qt-widget-show! sub))))
          (qt-on-triggered! cascade-btn
            (lambda () (qt-mdi-area-cascade! mdi)))
          (qt-on-triggered! tile-btn
            (lambda () (qt-mdi-area-tile! mdi)))
          (qt-on-triggered! tab-btn
            (lambda () (qt-mdi-area-set-view-mode! mdi QT_MDI_TABBED)))
          (qt-toolbar-add-action! toolbar new-btn)
          (qt-toolbar-add-separator! toolbar)
          (qt-toolbar-add-action! toolbar cascade-btn)
          (qt-toolbar-add-action! toolbar tile-btn)
          (qt-toolbar-add-action! toolbar tab-btn))
        (qt-main-window-add-toolbar! win toolbar))

      ;; --- Create initial documents ---
      (let loop ((i 1))
        (when (<= i 3)
          (set! doc-counter i)
          (let* ((editor (qt-text-edit-create))
                 (sub (qt-mdi-area-add-sub-window! mdi editor)))
            (qt-mdi-sub-window-set-title! sub (format "Document ~a" i))
            (qt-text-edit-set-text! editor (format "Content of document ~a." i))
            (qt-widget-show! sub))
          (loop (+ i 1))))

      ;; --- Show ---
      (qt-main-window-set-central-widget! win mdi)
      (qt-main-window-set-title! win "MDI Editor")
      (qt-widget-resize! win 800 600)
      (qt-widget-show! win)
      (qt-mdi-area-tile! mdi)
      (qt-main-window-set-status-bar-text! win "3 documents open")
      (qt-app-exec! app)

      ;; Cleanup
      (qt-undo-stack-destroy! undo-stack))))

(main)
