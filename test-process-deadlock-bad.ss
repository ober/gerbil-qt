#!/usr/bin/env gxi
;; DEMO: This will HANG when run as a Qt executable with QT_QPA_PLATFORM=offscreen
;; Due to Qt's SIGCHLD handler racing with Gambit's process-status

(import :gerbil-qt/qt
        :std/misc/process)

(export main)

(def (run-subprocess-bad)
  (displayln "Opening process: sleep 0.1")
  (let (proc (open-process [path: "sleep" arguments: ["0.1"]]))
    (displayln "Reading to EOF...")
    (let (output (read-line proc #f))
      (displayln "Output: " output)
      (displayln "Calling process-status (THIS WILL HANG)...")
      ;; BUG: This will hang indefinitely if Qt's SIGCHLD handler reaps the child first
      (let (status (process-status proc))
        (displayln "Exit status: " status)
        status))))

(def (main . args)
  (displayln "=== Qt SIGCHLD Bug Demo (WILL HANG) ===")
  (with-qt-app app
    (displayln "Running subprocess with process-status...")
    (run-subprocess-bad)
    (displayln "If you see this, the bug did not manifest (rare)"))
  (displayln "Done"))
