#!/usr/bin/env gxi
;;; Simple terminal demonstrating QProcess with QPlainTextEdit output
;;; and QLineEdit command input (Phase 15 features).

(import :gerbil-qt/qt
        :std/format)

(def (main)
  (let* ((app (qt-app-create))
         (win (qt-main-window-create))
         (central (qt-widget-create))
         (layout (qt-vbox-layout-create central))
         ;; Output display
         (output (qt-plain-text-edit-create))
         ;; Input area
         (input-box (qt-widget-create))
         (input-layout (qt-hbox-layout-create input-box))
         (prompt-label (qt-label-create "$ "))
         (input (qt-line-edit-create))
         (run-btn (qt-push-button-create "Run"))
         ;; Active process
         (proc #f))

    ;; Configure output area
    (qt-plain-text-edit-set-read-only! output #t)
    (qt-widget-set-font! output "Monospace" 10)
    (qt-plain-text-edit-set-max-block-count! output 10000)

    ;; Configure input
    (qt-line-edit-set-placeholder! input "Enter command...")
    (qt-widget-set-font! input "Monospace" 10)
    (qt-widget-set-font! prompt-label "Monospace" 10)

    ;; Build input row
    (qt-layout-add-widget! input-layout prompt-label)
    (qt-layout-add-widget! input-layout input)
    (qt-layout-add-widget! input-layout run-btn)

    ;; Build main layout
    (qt-layout-add-widget! layout output)
    (qt-layout-add-widget! layout input-box)

    ;; Append text to output
    (def (append-output text)
      (qt-plain-text-edit-append! output text))

    ;; Run a command
    (def (run-command)
      (let ((cmd (qt-line-edit-text input)))
        (when (> (string-length cmd) 0)
          ;; Clean up previous process
          (when proc
            (qt-process-destroy! proc)
            (set! proc #f))

          (append-output (format "$ ~a" cmd))
          (qt-line-edit-set-text! input "")

          ;; Create and start process
          (set! proc (qt-process-create))

          ;; Set up ready-read callback for streaming output
          (qt-process-on-ready-read! proc
            (lambda ()
              (let ((out (qt-process-read-stdout proc)))
                (when (> (string-length out) 0)
                  (append-output out)))))

          ;; Set up finished callback
          (qt-process-on-finished! proc
            (lambda (code)
              (let ((err (qt-process-read-stderr proc)))
                (when (> (string-length err) 0)
                  (append-output err)))
              (append-output (format "[exit ~a]" code))))

          ;; Start the process via sh -c
          (qt-process-start! proc "/bin/sh" ["-c" cmd]))))

    ;; Connect signals
    (qt-on-clicked! run-btn (lambda () (run-command)))
    (qt-on-line-edit-return! input (lambda (_text) (run-command)))

    ;; Window setup
    (qt-main-window-set-central-widget! win central)
    (qt-main-window-set-title! win "Gerbil Terminal")
    (qt-widget-resize! win 700 500)
    (qt-widget-show! win)

    ;; Welcome message
    (append-output "Gerbil Terminal — Type a command and press Enter or Run")
    (append-output "")

    (qt-app-exec! app)))

(main)
