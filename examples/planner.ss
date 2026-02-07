;;; planner.ss — Phase 10 demo: QFormLayout, QCalendarWidget, QTextBrowser,
;;;               QDialogButtonBox, QShortcut
;;;
;;; A simple "event planner" combining all Phase 10 features:
;;; - QFormLayout for user info fields
;;; - QCalendarWidget for date selection
;;; - QTextBrowser showing event details as HTML
;;; - QDialogButtonBox for Save/Cancel
;;; - QShortcut for Ctrl+S (save) and Ctrl+Q (quit)

(import :gerbil-qt/qt)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (central (qt-widget-create))
           (layout (qt-vbox-layout-create central))

           ;; --- Form Layout for user info ---
           (form-container (qt-widget-create parent: central))
           (form (qt-form-layout-create parent: form-container))
           (name-edit (qt-line-edit-create parent: form-container))
           (location-edit (qt-line-edit-create parent: form-container))
           (notes-edit (qt-line-edit-create parent: form-container))

           ;; --- Calendar for date picking ---
           (calendar (qt-calendar-create parent: central))

           ;; --- Text Browser for event preview ---
           (preview (qt-text-browser-create parent: central))

           ;; --- Dialog Button Box ---
           (buttons (qt-button-box-create
                      (bitwise-ior QT_BUTTON_SAVE QT_BUTTON_CANCEL)
                      parent: central))

           ;; --- Shortcuts ---
           (sc-save (qt-shortcut-create "Ctrl+S" win))
           (sc-quit (qt-shortcut-create "Ctrl+Q" win)))

      ;; Set up form rows
      (qt-line-edit-set-placeholder! name-edit "Enter event name")
      (qt-line-edit-set-placeholder! location-edit "Enter location")
      (qt-line-edit-set-placeholder! notes-edit "Optional notes")
      (qt-form-layout-add-row! form "Event Name:" name-edit)
      (qt-form-layout-add-row! form "Location:" location-edit)
      (qt-form-layout-add-row! form "Notes:" notes-edit)

      ;; Configure calendar
      (qt-calendar-set-grid-visible! calendar #t)
      (qt-calendar-set-first-day-of-week! calendar QT_MONDAY)

      ;; Initial preview
      (qt-text-browser-set-html! preview
        "<h3>Event Preview</h3><p><i>Fill in the form above and select a date.</i></p>")

      ;; Build the main layout
      (qt-layout-add-widget! layout form-container)
      (qt-layout-add-widget! layout calendar)
      (qt-layout-add-widget! layout preview)
      (qt-layout-add-widget! layout buttons)
      (qt-layout-set-spacing! layout 8)
      (qt-layout-set-margins! layout 10 10 10 10)

      ;; --- Update preview when calendar date changes ---
      (qt-on-selection-changed! calendar
        (lambda ()
          (let ((date (qt-calendar-selected-date-string calendar))
                (name (qt-line-edit-text name-edit))
                (loc  (qt-line-edit-text location-edit))
                (note (qt-line-edit-text notes-edit)))
            (qt-text-browser-set-html! preview
              (string-append
                "<h3>Event Preview</h3>"
                "<table>"
                "<tr><td><b>Date:</b></td><td>" date "</td></tr>"
                "<tr><td><b>Name:</b></td><td>" (if (string=? name "") "<i>not set</i>" name) "</td></tr>"
                "<tr><td><b>Location:</b></td><td>" (if (string=? loc "") "<i>not set</i>" loc) "</td></tr>"
                (if (string=? note "") ""
                    (string-append "<tr><td><b>Notes:</b></td><td>" note "</td></tr>"))
                "</table>")))))

      ;; --- Save action ---
      (let ((do-save
              (lambda ()
                (let ((name (qt-line-edit-text name-edit))
                      (date (qt-calendar-selected-date-string calendar))
                      (loc  (qt-line-edit-text location-edit)))
                  (displayln "SAVED: " name " on " date " at " loc)
                  (qt-text-browser-set-html! preview
                    (string-append
                      "<h3 style='color: green;'>Event Saved!</h3>"
                      "<p><b>" name "</b> on " date " at " loc "</p>"))))))

        ;; Button box signals
        (qt-on-accepted! buttons do-save)
        (qt-on-rejected! buttons (lambda () (qt-app-quit! app)))

        ;; Keyboard shortcuts
        (qt-on-shortcut-activated! sc-save do-save)
        (qt-on-shortcut-activated! sc-quit (lambda () (qt-app-quit! app))))

      ;; --- Window setup ---
      (qt-main-window-set-central-widget! win central)
      (qt-main-window-set-title! win "Event Planner (Phase 10)")
      (qt-widget-resize! win 500 700)
      (qt-widget-show! win)
      (qt-app-exec! app))))

(main)
