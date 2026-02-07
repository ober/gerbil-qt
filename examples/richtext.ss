#!/usr/bin/env gxi
;;; richtext.ss — HTML viewer with font/color customization and scroll area.
;;;
;;; Demonstrates: QTextBrowser (HTML, anchors), QScrollArea, QFont objects
;;; (create, family, bold, italic), QColor objects (create, channels, name),
;;; QFontDialog, QColorDialog, QDialogButtonBox, QFrame.

(import :gerbil-qt/qt
        :std/format)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (central (qt-widget-create))
           (main-layout (qt-vbox-layout-create central))

           ;; HTML browser
           (browser (qt-text-browser-create))

           ;; Control panel in a frame
           (ctrl-frame (qt-frame-create))
           (ctrl-layout (qt-hbox-layout-create ctrl-frame))
           (font-btn (qt-push-button-create "Choose Font"))
           (color-btn (qt-push-button-create "Choose Color"))
           (bold-btn (qt-push-button-create "B"))
           (italic-btn (qt-push-button-create "I"))
           (font-label (qt-label-create "Font: Default"))
           (color-label (qt-label-create ""))

           ;; Status area with button box
           (status-frame (qt-frame-create))
           (status-layout (qt-hbox-layout-create status-frame))
           (anchor-label (qt-label-create "Click a link..."))
           (button-box (qt-button-box-create
                         (bitwise-ior QT_BUTTON_CLOSE QT_BUTTON_RESET)))

           ;; Track current style
           (current-font #f)
           (current-color #f))

      ;; --- Frame styling ---
      (qt-frame-set-shape! ctrl-frame QT_FRAME_STYLED_PANEL)
      (qt-frame-set-shadow! ctrl-frame QT_FRAME_RAISED)
      (qt-frame-set-line-width! ctrl-frame 1)

      (qt-frame-set-shape! status-frame QT_FRAME_STYLED_PANEL)
      (qt-frame-set-shadow! status-frame QT_FRAME_SUNKEN)

      ;; --- Set initial HTML ---
      (qt-text-browser-set-html! browser
        (string-append
          "<h1>Gerbil-Qt Rich Text Browser</h1>"
          "<p>This demonstrates the <b>QTextBrowser</b> widget with HTML content.</p>"
          "<h2>Features</h2>"
          "<ul>"
          "<li>Inline <b>bold</b>, <i>italic</i>, and <code>code</code></li>"
          "<li>Links: <a href=\"https://cons.io\">Gerbil Homepage</a></li>"
          "<li>Colors: <span style=\"color:red\">red</span>, "
          "<span style=\"color:blue\">blue</span>, "
          "<span style=\"color:green\">green</span></li>"
          "<li>Nested lists and HTML formatting</li>"
          "</ul>"
          "<h2>Color Reference</h2>"
          "<table border=\"1\" cellpadding=\"4\">"
          "<tr><th>Color</th><th>Hex</th><th>RGB</th></tr>"
          "<tr><td style=\"background:#ff6b6b\">&nbsp;</td><td>#ff6b6b</td><td>255,107,107</td></tr>"
          "<tr><td style=\"background:#4ecdc4\">&nbsp;</td><td>#4ecdc4</td><td>78,205,196</td></tr>"
          "<tr><td style=\"background:#45b7d1\">&nbsp;</td><td>#45b7d1</td><td>69,183,209</td></tr>"
          "</table>"
          "<p><a href=\"action:font\">Choose Font</a> | <a href=\"action:color\">Choose Color</a></p>"))

      ;; Don't open links externally (we handle them)
      (qt-text-browser-set-open-external-links! browser #f)

      ;; --- Anchor click handler ---
      (qt-on-anchor-clicked! browser
        (lambda (url)
          (qt-label-set-text! anchor-label (format "Clicked: ~a" url))
          (cond
            ((string=? url "action:font")
             ;; Trigger font dialog
             (let ((f (qt-font-dialog parent: win)))
               (when f
                 (qt-label-set-text! font-label
                   (format "Font: ~a ~apt~a~a"
                     (qt-font-family f)
                     (qt-font-point-size f)
                     (if (qt-font-bold? f) " Bold" "")
                     (if (qt-font-italic? f) " Italic" "")))
                 (when current-font (qt-font-destroy! current-font))
                 (set! current-font f))))
            ((string=? url "action:color")
             ;; Trigger color dialog
             (let ((c (qt-color-dialog parent: win)))
               (when c
                 (let ((name (qt-color-name c)))
                   (qt-label-set-text! color-label
                     (format "Color: ~a (R:~a G:~a B:~a A:~a)"
                       name
                       (qt-color-red c) (qt-color-green c)
                       (qt-color-blue c) (qt-color-alpha c)))
                   (qt-widget-set-style-sheet! color-label
                     (format "color: ~a; font-weight: bold;" name)))
                 (when current-color (qt-color-destroy! current-color))
                 (set! current-color c))))
            (else
             (qt-label-set-text! anchor-label (format "External link: ~a" url))))))

      ;; --- Font dialog button ---
      (qt-on-clicked! font-btn
        (lambda ()
          (let ((f (qt-font-dialog parent: win)))
            (when f
              (qt-label-set-text! font-label
                (format "Font: ~a ~apt~a~a"
                  (qt-font-family f)
                  (qt-font-point-size f)
                  (if (qt-font-bold? f) " Bold" "")
                  (if (qt-font-italic? f) " Italic" "")))
              (when current-font (qt-font-destroy! current-font))
              (set! current-font f)))))

      ;; --- Color dialog button ---
      (qt-on-clicked! color-btn
        (lambda ()
          (let ((c (qt-color-dialog initial: "#3498db" parent: win)))
            (when c
              (let ((name (qt-color-name c)))
                (qt-label-set-text! color-label
                  (format "Color: ~a (R:~a G:~a B:~a)"
                    name (qt-color-red c) (qt-color-green c) (qt-color-blue c)))
                (qt-widget-set-style-sheet! color-label
                  (format "color: ~a; font-weight: bold;" name)))
              (when current-color (qt-color-destroy! current-color))
              (set! current-color c)))))

      ;; --- Bold/Italic buttons create font ---
      (qt-on-clicked! bold-btn
        (lambda ()
          (let ((f (qt-font-create "Sans" point-size: 12)))
            (qt-font-set-bold! f #t)
            (qt-label-set-text! font-label
              (format "Font: ~a ~apt Bold" (qt-font-family f) (qt-font-point-size f)))
            (when current-font (qt-font-destroy! current-font))
            (set! current-font f))))

      (qt-on-clicked! italic-btn
        (lambda ()
          (let ((f (qt-font-create "Serif" point-size: 14)))
            (qt-font-set-italic! f #t)
            (qt-label-set-text! font-label
              (format "Font: ~a ~apt Italic" (qt-font-family f) (qt-font-point-size f)))
            (when current-font (qt-font-destroy! current-font))
            (set! current-font f))))

      ;; --- Control bar ---
      (qt-widget-set-style-sheet! bold-btn "font-weight: bold;")
      (qt-widget-set-style-sheet! italic-btn "font-style: italic;")
      (qt-layout-add-widget! ctrl-layout font-btn)
      (qt-layout-add-widget! ctrl-layout color-btn)
      (qt-layout-add-spacing! ctrl-layout 10)
      (qt-layout-add-widget! ctrl-layout bold-btn)
      (qt-layout-add-widget! ctrl-layout italic-btn)
      (qt-layout-add-spacing! ctrl-layout 10)
      (qt-layout-add-widget! ctrl-layout font-label)
      (qt-layout-add-widget! ctrl-layout color-label)
      (qt-layout-add-stretch! ctrl-layout)

      ;; --- Button box ---
      (qt-on-accepted! button-box
        (lambda () (qt-widget-close! win)))
      (qt-on-rejected! button-box
        (lambda ()
          (qt-text-browser-set-html! browser "<h2>Content Reset</h2><p>Click a button to load new content.</p>")
          (qt-label-set-text! anchor-label "Content was reset")))
      ;; Hook the "reset" button through button-clicked
      (qt-on-button-clicked! button-box
        (lambda ()
          (qt-label-set-text! anchor-label "Button box button clicked")))

      (qt-layout-add-widget! status-layout anchor-label)
      (qt-layout-add-stretch! status-layout)
      (qt-layout-add-widget! status-layout button-box)

      ;; --- Assemble ---
      (qt-layout-add-widget! main-layout ctrl-frame)
      (qt-layout-add-widget! main-layout browser)
      (qt-layout-add-widget! main-layout status-frame)

      ;; --- Show ---
      (qt-main-window-set-central-widget! win central)
      (qt-main-window-set-title! win "Rich Text Browser")
      (qt-widget-resize! win 700 550)
      (qt-widget-show! win)
      (qt-app-exec! app)

      ;; Cleanup
      (when current-font (qt-font-destroy! current-font))
      (when current-color (qt-color-destroy! current-color)))))

(main)
