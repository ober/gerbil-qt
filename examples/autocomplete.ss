;;; autocomplete.ss — Phase 11 demo: QSettings, QCompleter, QToolTip
;;;
;;; A "search with history" app combining all Phase 11 features:
;;; - QSettings to persist search history across runs
;;; - QCompleter for auto-completing from past searches
;;; - QToolTip/QWhatsThis for contextual help
;;;
;;; Search history is saved to ~/.config/GerbilQt/autocomplete.conf

(import :gerbil-qt/qt
        :std/srfi/13)   ;; string-join

(def *max-history* 50)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (central (qt-widget-create))
           (layout (qt-vbox-layout-create central))

           ;; --- Settings (load persistent history) ---
           (settings (qt-settings-create "GerbilQt" "autocomplete"))

           ;; --- Search bar with completer ---
           (search-frame (qt-frame-create parent: central))
           (search-layout (qt-hbox-layout-create search-frame))
           (search-edit (qt-line-edit-create parent: search-frame))
           (search-btn (qt-push-button-create "Search" parent: search-frame))
           (clear-btn (qt-push-button-create "Clear History" parent: search-frame))

           ;; --- Results area ---
           (results (qt-text-browser-create parent: central))

           ;; --- Load saved history ---
           (saved (qt-settings-string settings "history" default: ""))
           (history (if (string=? saved "")
                      []
                      (string-split saved #\newline)))

           ;; --- Completer from history ---
           (completer (qt-completer-create history)))

      ;; Configure search bar
      (qt-line-edit-set-placeholder! search-edit "Type to search...")
      (qt-widget-set-tooltip! search-edit "Enter a search term — previous searches auto-complete")
      (qt-widget-set-whats-this! search-edit "Type a search query. Previous searches are remembered and offered as auto-complete suggestions.")
      (qt-widget-set-tooltip! search-btn "Add this search to history")
      (qt-widget-set-tooltip! clear-btn "Remove all saved searches")
      (qt-frame-set-shape! search-frame QT_FRAME_STYLED_PANEL)
      (qt-frame-set-shadow! search-frame QT_FRAME_RAISED)

      ;; Attach completer to search field
      (qt-completer-set-case-sensitivity! completer #f)
      (qt-completer-set-filter-mode! completer QT_MATCH_CONTAINS)
      (qt-completer-set-max-visible-items! completer 10)
      (qt-line-edit-set-completer! search-edit completer)

      ;; Build search bar layout
      (qt-layout-add-widget! search-layout search-edit)
      (qt-layout-add-widget! search-layout search-btn)
      (qt-layout-add-widget! search-layout clear-btn)

      ;; Initial results
      (qt-text-browser-set-html! results
        (string-append
          "<h2>Search Demo</h2>"
          "<p>Type a search term and press <b>Search</b>.</p>"
          "<p>Your search history is saved and offered as auto-complete suggestions.</p>"
          "<p><i>" (number->string (length history)) " saved searches.</i></p>"))

      ;; Build main layout
      (qt-layout-add-widget! layout search-frame)
      (qt-layout-add-widget! layout results)
      (qt-layout-set-spacing! layout 8)
      (qt-layout-set-margins! layout 10 10 10 10)

      ;; Update status bar with history count
      (qt-main-window-set-status-bar-text! win
        (string-append (number->string (length history)) " searches in history"))

      ;; --- Search action ---
      (let ((do-search
              (lambda ()
                (let ((query (qt-line-edit-text search-edit)))
                  (when (not (string=? query ""))
                    ;; Add to history (deduplicate)
                    (unless (member query history)
                      (set! history (cons query history))
                      ;; Trim to max
                      (when (> (length history) *max-history*)
                        (set! history (take history *max-history*)))
                      ;; Update completer model
                      (qt-completer-set-model-strings! completer history)
                      ;; Persist to settings
                      (qt-settings-set-string! settings "history"
                        (string-join history "\n"))
                      (qt-settings-sync! settings))
                    ;; Show result
                    (qt-text-browser-set-html! results
                      (string-append
                        "<h2>Search Results</h2>"
                        "<p>You searched for: <b>" query "</b></p>"
                        "<hr>"
                        "<p><i>(This is a demo — no actual search is performed.)</i></p>"
                        "<h3>History (" (number->string (length history)) " entries)</h3>"
                        "<ul>"
                        (apply string-append
                          (map (lambda (h) (string-append "<li>" h "</li>")) history))
                        "</ul>"))
                    ;; Update status
                    (qt-main-window-set-status-bar-text! win
                      (string-append "Searched: \"" query "\" — "
                        (number->string (length history)) " in history"))
                    ;; Clear input
                    (qt-line-edit-set-text! search-edit ""))))))

        ;; Connect search button
        (qt-on-clicked! search-btn do-search)

        ;; Connect Enter key in search field
        (qt-on-return-pressed! search-edit do-search)

        ;; Connect completer selection
        (qt-on-completer-activated! completer
          (lambda (text)
            (qt-text-browser-set-html! results
              (string-append
                "<h2>Re-search</h2>"
                "<p>Auto-completed: <b>" text "</b></p>"
                "<hr>"
                "<p><i>Selected from history.</i></p>"))
            (qt-main-window-set-status-bar-text! win
              (string-append "Auto-completed: \"" text "\""))))

        ;; Clear history button
        (qt-on-clicked! clear-btn
          (lambda ()
            (set! history [])
            (qt-completer-set-model-strings! completer history)
            (qt-settings-clear! settings)
            (qt-settings-sync! settings)
            (qt-text-browser-set-html! results
              "<h2>History Cleared</h2><p>All saved searches have been removed.</p>")
            (qt-main-window-set-status-bar-text! win "History cleared"))))

      ;; --- Window setup ---
      (qt-main-window-set-central-widget! win central)
      (qt-main-window-set-title! win "Auto-Complete Search (Phase 11)")
      (qt-widget-resize! win 600 500)
      (qt-widget-show! win)

      ;; Show tooltip at startup
      (qt-tooltip-show-text! 300 200 "Start typing to see auto-complete suggestions!"
        widget: search-edit)

      (qt-app-exec! app)

      ;; Cleanup (completer ownership transferred to line edit, don't destroy)
      (qt-settings-destroy! settings))))

(main)
