#!/usr/bin/env gxi
;;; Minimal Gerbil-Qt example: window with a label

(import :gerbil-qt/qt)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (label (qt-label-create "Hello from Gerbil-Qt!")))
      (qt-label-set-alignment! label QT_ALIGN_CENTER)
      (qt-widget-set-font-size! label 18)
      (qt-main-window-set-title! win "Hello World")
      (qt-main-window-set-central-widget! win label)
      (qt-widget-resize! win 400 200)
      (qt-widget-show! win)
      (qt-app-exec! app))))

(main)
