#!/usr/bin/env gxi
;;; trayapp.ss — System tray application with context menu and balloon messages.
;;;
;;; Demonstrates: QSystemTrayIcon (create, set-tooltip, set-icon, show, hide,
;;; show-message, set-context-menu), qt-on-tray-activated!, qt-system-tray-available?,
;;; QMenu (as context menu), tray icon type constants, QPixmap/QIcon for tray icon.

(import :gerbil-qt/qt
        :std/format)

(def (main)
  (with-qt-app app
    ;; Check tray availability first
    (if (not (qt-system-tray-available?))
      (begin
        (displayln "System tray is not available on this platform.")
        (displayln "This demo requires a desktop environment with tray support."))
      (let* ((win (qt-main-window-create))
             (central (qt-widget-create))
             (layout (qt-vbox-layout-create central))

             ;; Create a pixmap icon for the tray
             (pix (qt-pixmap-create-blank 32 32))
             (_ (begin
                  (qt-pixmap-fill! pix 52 152 219)  ;; Blue
                  (let ((p (qt-painter-create pix)))
                    (qt-painter-set-pen-color! p 255 255 255)
                    (qt-painter-set-brush-color! p 255 255 255)
                    (qt-painter-draw-text! p 6 22 "Qt")
                    (qt-painter-end! p)
                    (qt-painter-destroy! p))))
             (icon (qt-icon-create-from-pixmap pix))
             (tray (qt-system-tray-icon-create icon))

             ;; Context menu for tray
             (tray-menu (qt-menu-bar-add-menu (qt-main-window-menu-bar win) "Tray"))

             ;; Create real context menu
             (ctx-menu (qt-menu-bar-add-menu (qt-main-window-menu-bar win) "hidden"))

             ;; UI
             (log-area (qt-plain-text-edit-create))
             (btn-row (qt-widget-create))
             (btn-layout (qt-hbox-layout-create btn-row))
             (btn-show-msg (qt-push-button-create "Show Balloon"))
             (btn-hide (qt-push-button-create "Hide Tray Icon"))
             (btn-show (qt-push-button-create "Show Tray Icon"))
             (btn-change-tip (qt-push-button-create "Change Tooltip"))
             (tip-counter 0))

        (qt-plain-text-edit-set-read-only! log-area #t)
        (qt-plain-text-edit-set-max-block-count! log-area 200)

        (def (log! msg)
          (qt-plain-text-edit-append! log-area msg))

        ;; --- Tray icon setup ---
        (qt-system-tray-icon-set-tooltip! tray "Gerbil-Qt Tray Demo")

        ;; --- Context menu actions ---
        (let ((show-action (qt-action-create "Show Window" win))
              (info-action (qt-action-create "Info Message" win))
              (warn-action (qt-action-create "Warning Message" win))
              (sep1 #f)
              (quit-action (qt-action-create "Quit" win)))
          (qt-on-triggered! show-action
            (lambda ()
              (qt-widget-show-normal! win)
              (log! "Window shown from tray")))
          (qt-on-triggered! info-action
            (lambda ()
              (qt-system-tray-icon-show-message! tray
                "Info" "This is an info balloon" icon-type: QT_TRAY_INFO timeout: 5000)
              (log! "Info balloon sent")))
          (qt-on-triggered! warn-action
            (lambda ()
              (qt-system-tray-icon-show-message! tray
                "Warning" "Something needs attention" icon-type: QT_TRAY_WARNING)
              (log! "Warning balloon sent")))
          (qt-on-triggered! quit-action
            (lambda ()
              (qt-system-tray-icon-hide! tray)
              (qt-app-quit! app)))

          (qt-menu-add-action! ctx-menu show-action)
          (qt-menu-add-action! ctx-menu info-action)
          (qt-menu-add-action! ctx-menu warn-action)
          (qt-menu-add-separator! ctx-menu)
          (qt-menu-add-action! ctx-menu quit-action)

          (qt-system-tray-icon-set-context-menu! tray ctx-menu))

        ;; --- Tray activation handler ---
        (qt-on-tray-activated! tray
          (lambda (reason)
            (cond
              ((= reason QT_TRAY_TRIGGER)
               (log! "Tray: single click")
               (qt-widget-show-normal! win))
              ((= reason QT_TRAY_DOUBLE_CLICK)
               (log! "Tray: double click"))
              ((= reason QT_TRAY_CONTEXT)
               (log! "Tray: context menu"))
              ((= reason QT_TRAY_MIDDLE_CLICK)
               (log! "Tray: middle click")))))

        ;; --- Buttons ---
        (qt-on-clicked! btn-show-msg
          (lambda ()
            (qt-system-tray-icon-show-message! tray
              "Hello from Gerbil"
              "This balloon message was sent from the application."
              icon-type: QT_TRAY_INFO
              timeout: 3000)
            (log! "Balloon message sent")))

        (qt-on-clicked! btn-hide
          (lambda ()
            (qt-system-tray-icon-hide! tray)
            (log! "Tray icon hidden")))

        (qt-on-clicked! btn-show
          (lambda ()
            (qt-system-tray-icon-show! tray)
            (log! "Tray icon shown")))

        (qt-on-clicked! btn-change-tip
          (lambda ()
            (set! tip-counter (+ tip-counter 1))
            (let ((tip (format "Gerbil-Qt (#~a)" tip-counter)))
              (qt-system-tray-icon-set-tooltip! tray tip)
              (log! (format "Tooltip changed to: ~a" tip)))))

        ;; --- Assemble ---
        (qt-layout-add-widget! btn-layout btn-show-msg)
        (qt-layout-add-widget! btn-layout btn-hide)
        (qt-layout-add-widget! btn-layout btn-show)
        (qt-layout-add-widget! btn-layout btn-change-tip)

        (qt-layout-add-widget! layout
          (qt-label-create "System Tray Demo — minimize to tray, right-click tray icon for menu"))
        (qt-layout-add-widget! layout btn-row)
        (qt-layout-add-widget! layout log-area)

        ;; Show tray icon and window
        (qt-system-tray-icon-show! tray)

        (qt-main-window-set-central-widget! win central)
        (qt-main-window-set-title! win "System Tray App")
        (qt-widget-set-window-icon! win icon)
        (qt-widget-resize! win 550 400)
        (qt-widget-show! win)
        (log! "System tray demo started")
        (log! (format "Tray available: ~a" (qt-system-tray-available?)))
        (qt-app-exec! app)

        ;; Cleanup
        (qt-system-tray-icon-destroy! tray)
        (qt-icon-destroy! icon)
        (qt-pixmap-destroy! pix)))))

(main)
