#!/usr/bin/env gxi
;;; File Browser — Phase 5 demo
;;; Demonstrates QTreeWidget, QGridLayout, QTimer, and clipboard integration.

(import :gerbil-qt/qt
        :std/os/fs
        :std/srfi/19
        :std/format)

(def (populate-tree! tree path)
  "Recursively populate a tree widget with directory contents."
  (qt-tree-widget-clear! tree)
  (qt-tree-widget-set-header-labels! tree '("Name" "Size" "Type"))
  (add-dir-entries! tree #f path))

(def (add-dir-entries! tree parent-item dir)
  "Add directory entries as children (or top-level if parent-item is #f)."
  (let ((entries (with-catch (lambda (_) '()) (lambda () (directory-files dir)))))
    (for-each
      (lambda (name)
        (let* ((full-path (string-append dir "/" name))
               (is-dir (with-catch (lambda (_) #f)
                         (lambda () (file-directory? full-path))))
               (size (if is-dir
                       ""
                       (with-catch (lambda (_) "?")
                         (lambda () (number->string (file-size full-path))))))
               (type (if is-dir "Directory" "File"))
               (item (qt-tree-item-create name)))
          (qt-tree-item-set-text! item size column: 1)
          (qt-tree-item-set-text! item type column: 2)
          (if parent-item
            (qt-tree-item-add-child! parent-item item)
            (qt-tree-widget-add-top-level-item! tree item))
          ;; Recursively add subdirectories (1 level deep for speed)
          (when (and is-dir (not parent-item))
            (add-dir-entries! tree item full-path))))
      entries)))

(def (current-time-string)
  (let ((now (current-date)))
    (format "~a:~a:~a"
            (with-output-to-string (lambda () (display (date-hour now))))
            (let ((m (date-minute now)))
              (if (< m 10) (string-append "0" (number->string m))
                  (number->string m)))
            (let ((s (date-second now)))
              (if (< s 10) (string-append "0" (number->string s))
                  (number->string s))))))

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (central (qt-widget-create))
           (grid (qt-grid-layout-create central))

           ;; Path entry
           (path-label (qt-label-create "Path:"))
           (path-edit (qt-line-edit-create))
           (browse-btn (qt-push-button-create "Browse"))

           ;; Tree widget
           (tree (qt-tree-widget-create))

           ;; Status area
           (status-label (qt-label-create "Ready"))
           (clock-label (qt-label-create (current-time-string)))
           (copy-btn (qt-push-button-create "Copy Path")))

      ;; Grid layout: path bar on row 0, tree on row 1, status on row 2
      (qt-grid-layout-add-widget! grid path-label 0 0)
      (qt-grid-layout-add-widget! grid path-edit 0 1)
      (qt-grid-layout-add-widget! grid browse-btn 0 2)
      (qt-grid-layout-add-widget! grid tree 1 0 col-span: 3)
      (qt-grid-layout-add-widget! grid status-label 2 0)
      (qt-grid-layout-add-widget! grid clock-label 2 1)
      (qt-grid-layout-add-widget! grid copy-btn 2 2)

      ;; Give tree row most of the stretch
      (qt-grid-layout-set-row-stretch! grid 1 1)
      (qt-layout-set-spacing! grid 6)
      (qt-layout-set-margins! grid 8 8 8 8)

      ;; Initial state
      (let ((start-dir (path-expand "~")))
        (qt-line-edit-set-text! path-edit start-dir)
        (populate-tree! tree start-dir))

      ;; Browse button — refresh tree from path
      (qt-on-clicked! browse-btn
        (lambda ()
          (let ((dir (qt-line-edit-text path-edit)))
            (populate-tree! tree dir)
            (qt-label-set-text! status-label
              (string-append "Browsing: " dir)))))

      ;; Return pressed in path edit — same as browse
      (qt-on-return-pressed! path-edit
        (lambda ()
          (let ((dir (qt-line-edit-text path-edit)))
            (populate-tree! tree dir)
            (qt-label-set-text! status-label
              (string-append "Browsing: " dir)))))

      ;; Copy path button — copy current path to clipboard
      (qt-on-clicked! copy-btn
        (lambda ()
          (qt-clipboard-set-text! app (qt-line-edit-text path-edit))
          (qt-label-set-text! status-label "Path copied to clipboard")))

      ;; Timer updates the clock every second
      (let ((timer (qt-start-timer! 1000
                     (lambda ()
                       (qt-label-set-text! clock-label (current-time-string))))))

        (qt-main-window-set-central-widget! win central)
        (qt-main-window-set-title! win "File Browser")
        (qt-widget-resize! win 700 500)
        (qt-widget-show! win)

        (qt-app-exec! app)

        ;; Cleanup timer
        (qt-timer-stop! timer)
        (qt-timer-destroy! timer)))))

(main)
