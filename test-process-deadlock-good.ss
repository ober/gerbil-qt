#!/usr/bin/env gxi
;; DEMO: Correct way to handle subprocesses in Qt executables
;; Avoids process-status to prevent SIGCHLD race conditions

(import :gerbil-qt/qt
        :std/misc/process)

(export main)

(def (run-subprocess-readln-only)
  (displayln "Opening process: echo hello")
  (let (proc (open-process [path: "echo" arguments: ["hello"]]))
    (displayln "Reading output...")
    (let (output (read-line proc #f))
      (displayln "Output: " output)
      ;; CORRECT: read-line with eof waits for process completion
      ;; No explicit process-status needed
      (close-port proc)
      (displayln "Process completed (no process-status call)")
      0))) ;; Assume success if no error

(def (run-subprocess-qprocess)
  (displayln "\nUsing QProcess API:")
  (let (proc (qt-process-create))
    (qt-process-start! proc "echo" args: ["goodbye"])
    (displayln "Waiting for process...")
    (qt-process-wait-for-finished proc) ;; Safe - uses Qt's process management
    (let* ((output (qt-process-read-stdout proc))
           (status (qt-process-exit-code proc)))
      (displayln "Output: " output)
      (displayln "Exit status: " status)
      (qt-process-destroy! proc)
      status)))

(def (main . args)
  (displayln "=== Qt Subprocess Workarounds (SAFE) ===")
  (with-qt-app app
    (displayln "Method 1: read-line without process-status")
    (run-subprocess-readln-only)

    (displayln "\nMethod 2: QProcess API")
    (run-subprocess-qprocess))
  (displayln "\nDone - no hangs!"))
