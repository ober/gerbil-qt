#!/usr/bin/env gxi
;;; dockable.ss — IDE-like layout with dock widgets, stacked widget, scroll area,
;;; window state management, and keyboard shortcuts.
;;;
;;; Demonstrates: QDockWidget, QStackedWidget, QScrollArea, QShortcut,
;;; window state (minimize/maximize/fullscreen/normal), widget geometry,
;;; keyboard events, layout spacing/stretch.

(import :gerbil-qt/qt
        :std/format)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           ;; --- Central area: stacked widget ---
           (stacked (qt-stacked-widget-create))

           ;; Page 1: A scroll area with many labels
           (scroll (qt-scroll-area-create))
           (scroll-content (qt-widget-create))
           (scroll-layout (qt-vbox-layout-create scroll-content))

           ;; Page 2: Status dashboard
           (dash-page (qt-widget-create))
           (dash-layout (qt-vbox-layout-create dash-page))
           (state-label (qt-label-create "Window state: Normal"))
           (geo-label (qt-label-create "Geometry: ..."))

           ;; --- Dock 1: Navigation panel (left) ---
           (nav-dock (qt-dock-widget-create "Navigation"))
           (nav-list (qt-list-widget-create))

           ;; --- Dock 2: Properties panel (right) ---
           (prop-dock (qt-dock-widget-create "Properties"))
           (prop-widget (qt-widget-create))
           (prop-layout (qt-vbox-layout-create prop-widget))
           (prop-title (qt-label-create "Select a page"))
           (prop-info (qt-plain-text-edit-create))

           ;; --- Dock 3: Output panel (bottom) ---
           (out-dock (qt-dock-widget-create "Output"))
           (out-text (qt-plain-text-edit-create))

           ;; Status bar
           (status-text "Ready"))

      ;; --- Build scroll area content ---
      (let loop ((i 0))
        (when (< i 30)
          (let ((lbl (qt-label-create (format "Item ~a — Scrollable content row" i))))
            (qt-widget-set-style-sheet! lbl "padding: 6px; border-bottom: 1px solid #ddd;")
            (qt-layout-add-widget! scroll-layout lbl))
          (loop (+ i 1))))
      (qt-layout-add-stretch! scroll-layout)
      (qt-scroll-area-set-widget! scroll scroll-content)
      (qt-scroll-area-set-widget-resizable! scroll #t)
      (qt-scroll-area-set-horizontal-scrollbar-policy! scroll QT_SCROLLBAR_ALWAYS_OFF)

      ;; --- Build dashboard page ---
      (qt-label-set-alignment! state-label QT_ALIGN_CENTER)
      (qt-label-set-alignment! geo-label QT_ALIGN_CENTER)
      (qt-widget-set-font-size! state-label 14)
      (qt-layout-add-stretch! dash-layout)
      (qt-layout-add-widget! dash-layout state-label)
      (qt-layout-add-spacing! dash-layout 10)
      (qt-layout-add-widget! dash-layout geo-label)
      (qt-layout-add-stretch! dash-layout)

      ;; --- Stacked widget pages ---
      (qt-stacked-widget-add-widget! stacked scroll)
      (qt-stacked-widget-add-widget! stacked dash-page)

      ;; --- Navigation list ---
      (qt-list-widget-add-item! nav-list "Scroll Area")
      (qt-list-widget-add-item! nav-list "Window State")
      (qt-list-widget-set-current-row! nav-list 0)
      (qt-on-current-row-changed! nav-list
        (lambda (row)
          (qt-stacked-widget-set-current-index! stacked row)
          (qt-plain-text-edit-append! out-text
            (format "Switched to page ~a" row))))

      ;; --- Properties panel ---
      (qt-plain-text-edit-set-read-only! prop-info #t)
      (qt-layout-add-widget! prop-layout prop-title)
      (qt-layout-add-widget! prop-layout prop-info)
      (qt-on-stacked-changed! stacked
        (lambda (idx)
          (cond
            ((= idx 0)
             (qt-label-set-text! prop-title "Scroll Area Page")
             (qt-plain-text-edit-set-text! prop-info
               "Demonstrates QScrollArea with scrollbar policies.\n30 items in a vertical layout."))
            ((= idx 1)
             (qt-label-set-text! prop-title "Window State Page")
             (qt-plain-text-edit-set-text! prop-info
               "Shows live window state and geometry.\nUse keyboard shortcuts to change state.")))))

      ;; --- Output panel ---
      (qt-plain-text-edit-set-read-only! out-text #t)
      (qt-plain-text-edit-set-max-block-count! out-text 200)
      (let ((mono (qt-font-create "Monospace" point-size: 9)))
        (qt-widget-set-font! out-text mono)
        (qt-font-destroy! mono))
      (qt-plain-text-edit-append! out-text "Output log initialized")

      ;; --- Dock widgets setup ---
      (qt-dock-widget-set-widget! nav-dock nav-list)
      (qt-dock-widget-set-widget! prop-dock prop-widget)
      (qt-dock-widget-set-widget! out-dock out-text)

      ;; Add docks to main window
      (qt-main-window-add-dock-widget! win QT_DOCK_LEFT nav-dock)
      (qt-main-window-add-dock-widget! win QT_DOCK_RIGHT prop-dock)
      (qt-main-window-add-dock-widget! win QT_DOCK_BOTTOM out-dock)

      ;; --- Central widget ---
      (qt-main-window-set-central-widget! win stacked)

      ;; --- Menu bar ---
      (let* ((mbar (qt-main-window-menu-bar win))
             (view-menu (qt-menu-bar-add-menu mbar "View"))
             (state-menu (qt-menu-bar-add-menu mbar "Window")))

        ;; View menu: toggle dock visibility
        (let ((a1 (qt-action-create "Toggle Navigation" parent: win)))
          (qt-action-set-shortcut! a1 "Ctrl+1")
          (qt-on-triggered! a1
            (lambda ()
              (if (qt-widget-enabled? nav-dock)
                (qt-widget-hide! nav-dock)
                (qt-widget-show! nav-dock))
              (qt-plain-text-edit-append! out-text "Toggled Navigation dock")))
          (qt-menu-add-action! view-menu a1))

        (let ((a2 (qt-action-create "Toggle Properties" parent: win)))
          (qt-action-set-shortcut! a2 "Ctrl+2")
          (qt-on-triggered! a2
            (lambda ()
              (if (qt-widget-enabled? prop-dock)
                (qt-widget-hide! prop-dock)
                (qt-widget-show! prop-dock))
              (qt-plain-text-edit-append! out-text "Toggled Properties dock")))
          (qt-menu-add-action! view-menu a2))

        (let ((a3 (qt-action-create "Toggle Output" parent: win)))
          (qt-action-set-shortcut! a3 "Ctrl+3")
          (qt-on-triggered! a3
            (lambda ()
              (if (qt-widget-enabled? out-dock)
                (qt-widget-hide! out-dock)
                (qt-widget-show! out-dock))
              (qt-plain-text-edit-append! out-text "Toggled Output dock")))
          (qt-menu-add-action! view-menu a3))

        ;; Window state menu
        (let ((norm (qt-action-create "Normal" parent: win))
              (mini (qt-action-create "Minimize" parent: win))
              (maxi (qt-action-create "Maximize" parent: win))
              (full (qt-action-create "Fullscreen" parent: win)))
          (qt-action-set-shortcut! norm "Ctrl+Shift+N")
          (qt-action-set-shortcut! maxi "Ctrl+Shift+M")
          (qt-action-set-shortcut! full "F11")
          (qt-on-triggered! norm (lambda () (qt-widget-show-normal! win)))
          (qt-on-triggered! mini (lambda () (qt-widget-show-minimized! win)))
          (qt-on-triggered! maxi (lambda () (qt-widget-show-maximized! win)))
          (qt-on-triggered! full (lambda () (qt-widget-show-fullscreen! win)))
          (qt-menu-add-action! state-menu norm)
          (qt-menu-add-action! state-menu mini)
          (qt-menu-add-action! state-menu maxi)
          (qt-menu-add-action! state-menu full)))

      ;; --- Keyboard event handler ---
      (qt-on-key-press! win
        (lambda ()
          (let ((code (qt-last-key-code))
                (mods (qt-last-key-modifiers))
                (text (qt-last-key-text)))
            (qt-plain-text-edit-append! out-text
              (format "Key: code=~a mods=~a text=~s" code mods text)))))

      ;; --- Timer to update geometry display ---
      (qt-timer-single-shot! 500
        (lambda ()
          (let ((update!
                 (lambda ()
                   (let ((ws (qt-widget-window-state win))
                         (x (qt-widget-x win))
                         (y (qt-widget-y win))
                         (w (qt-widget-width win))
                         (h (qt-widget-height win)))
                     (qt-label-set-text! state-label
                       (format "Window state: ~a"
                         (cond ((= ws QT_WINDOW_NO_STATE) "Normal")
                               ((= ws QT_WINDOW_MINIMIZED) "Minimized")
                               ((= ws QT_WINDOW_MAXIMIZED) "Maximized")
                               ((= ws QT_WINDOW_FULL_SCREEN) "Fullscreen")
                               (else (format "~a" ws)))))
                     (qt-label-set-text! geo-label
                       (format "Position: (~a, ~a)  Size: ~ax~a" x y w h))))))
            ;; Update periodically
            (let ((timer (qt-timer-create)))
              (qt-timer-set-interval! timer 500)
              (qt-on-timeout! timer update!)
              (qt-timer-start! timer 500)))))

      ;; --- Show ---
      (qt-main-window-set-title! win "Dockable IDE Layout")
      (qt-widget-resize! win 900 600)
      (qt-widget-show! win)
      (qt-main-window-set-status-bar-text! win "Ready — Use Ctrl+1/2/3 to toggle docks, F11 for fullscreen")
      (qt-app-exec! app))))

(main)
