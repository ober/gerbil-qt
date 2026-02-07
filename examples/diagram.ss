#!/usr/bin/env gxi
;; diagram.ss — Interactive diagram with QGraphicsScene/View and custom PaintWidget

(import :gerbil-qt/qt)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (central (qt-widget-create))
           (layout (qt-vbox-layout-create central))

           ;; --- Scene graph demo (top half) ---
           (scene (qt-graphics-scene-create 0 0 500 300))
           (view (qt-graphics-view-create scene))

           ;; --- Custom paint widget (bottom half) ---
           (canvas (qt-paint-widget-create))

           ;; Status label
           (status (qt-label-create "Drag shapes in the scene, canvas paints live")))

      ;; Configure the graphics view
      (qt-graphics-view-set-render-hint! view QT_RENDER_ANTIALIASING)
      (qt-graphics-view-set-drag-mode! view QT_DRAG_RUBBER_BAND)
      (qt-graphics-scene-set-background! scene 245 245 245)

      ;; Add some shapes to the scene
      (let ((box1 (qt-graphics-scene-add-rect! scene 20 30 120 80)))
        (qt-graphics-item-set-brush! box1 100 150 255)
        (qt-graphics-item-set-pen! box1 40 80 180 2)
        (qt-graphics-item-set-flags! box1
          (bitwise-ior QT_ITEM_MOVABLE QT_ITEM_SELECTABLE))
        (qt-graphics-item-set-tooltip! box1 "Blue box — drag me!"))

      (let ((box2 (qt-graphics-scene-add-rect! scene 200 50 100 100)))
        (qt-graphics-item-set-brush! box2 255 140 60)
        (qt-graphics-item-set-pen! box2 200 90 20 2)
        (qt-graphics-item-set-flags! box2
          (bitwise-ior QT_ITEM_MOVABLE QT_ITEM_SELECTABLE))
        (qt-graphics-item-set-tooltip! box2 "Orange box — drag me!"))

      (let ((circle (qt-graphics-scene-add-ellipse! scene 360 80 100 100)))
        (qt-graphics-item-set-brush! circle 100 220 100)
        (qt-graphics-item-set-pen! circle 40 160 40 2)
        (qt-graphics-item-set-flags! circle
          (bitwise-ior QT_ITEM_MOVABLE QT_ITEM_SELECTABLE))
        (qt-graphics-item-set-tooltip! circle "Green circle — drag me!"))

      ;; Connecting line
      (let ((line (qt-graphics-scene-add-line! scene 140 70 200 100)))
        (qt-graphics-item-set-pen! line 80 80 80 2)
        (qt-graphics-item-set-zvalue! line -1.0))

      ;; Text label in the scene
      (let ((txt (qt-graphics-scene-add-text! scene "Diagram")))
        (qt-graphics-item-set-pos! txt 180 10))

      ;; Configure the custom paint widget
      (qt-widget-set-minimum-size! canvas 500 200)
      (qt-paint-widget-on-paint! canvas
        (lambda ()
          (let ((painter (qt-paint-widget-painter canvas))
                (w (qt-paint-widget-width canvas))
                (h (qt-paint-widget-height canvas)))
            (when painter
              ;; Background
              (qt-painter-set-brush-color! painter 30 30 50)
              (qt-painter-fill-rect! painter 0 0 w h)

              ;; Draw a gradient of circles
              (qt-painter-set-antialiasing! painter #t)
              (let loop ((i 0))
                (when (< i 8)
                  (let* ((x (+ 30 (* i 60)))
                         (r (+ 80 (* i 20)))
                         (g (- 220 (* i 25)))
                         (b (+ 120 (* i 15)))
                         (size (- 50 (* i 3))))
                    (qt-painter-set-brush-color! painter r g b)
                    (qt-painter-set-pen-color! painter 255 255 255)
                    (qt-painter-set-pen-width! painter 1)
                    (qt-painter-draw-ellipse! painter x 20 size size))
                  (loop (+ i 1))))

              ;; Label
              (qt-painter-set-pen-color! painter 200 200 200)
              (qt-painter-draw-text! painter 30 120 "Custom PaintWidget — repaints on resize")

              ;; Decorative lines
              (qt-painter-set-pen-color! painter 80 120 180)
              (qt-painter-set-pen-width! painter 2)
              (qt-painter-draw-line! painter 30 140 (- w 30) 140)
              (qt-painter-draw-line! painter 30 145 (- w 30) 145)))))

      ;; Assemble layout
      (qt-layout-add-widget! layout view)
      (qt-layout-add-widget! layout canvas)
      (qt-layout-add-widget! layout status)

      ;; Show
      (qt-main-window-set-title! win "Diagram — Graphics Scene & PaintWidget")
      (qt-main-window-set-central-widget! win central)
      (qt-widget-resize! win 540 560)
      (qt-widget-show! win)
      (qt-app-exec! app))))

(main)
