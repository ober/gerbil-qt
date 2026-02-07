#!/usr/bin/env gxi
;;; dragdrop.ss — Drag and drop between list widgets.
;;;
;;; Demonstrates: qt-widget-set-accept-drops!, qt-on-drop!, qt-drag-text!,
;;; qt-drop-filter-last-text, qt-drop-filter-destroy!, QFrame styling,
;;; QLabel alignment.

(import :gerbil-qt/qt
        :std/format)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (central (qt-widget-create))
           (main-layout (qt-vbox-layout-create central))

           ;; Instructions
           (instructions (qt-label-create
             "Double-click an item in the left list to drag it to the right list"))
           ;; Lists in a horizontal layout
           (lists-row (qt-widget-create))
           (lists-layout (qt-hbox-layout-create lists-row))

           ;; Source list (left)
           (src-frame (qt-frame-create))
           (src-layout (qt-vbox-layout-create src-frame))
           (src-label (qt-label-create "Source Items"))
           (src-list (qt-list-widget-create))

           ;; Drop target (right)
           (dst-frame (qt-frame-create))
           (dst-layout (qt-vbox-layout-create dst-frame))
           (dst-label (qt-label-create "Drop Target"))
           (dst-list (qt-list-widget-create))

           ;; Drop zone (a label that accepts drops)
           (drop-zone-frame (qt-frame-create))
           (drop-zone-layout (qt-vbox-layout-create drop-zone-frame))
           (drop-zone (qt-label-create "Drop items here too!"))
           (drop-log (qt-plain-text-edit-create))

           ;; Track drop filters for cleanup
           (drop-filters []))

      ;; --- Style frames ---
      (qt-frame-set-shape! src-frame QT_FRAME_BOX)
      (qt-frame-set-shadow! src-frame QT_FRAME_SUNKEN)
      (qt-frame-set-line-width! src-frame 2)

      (qt-frame-set-shape! dst-frame QT_FRAME_BOX)
      (qt-frame-set-shadow! dst-frame QT_FRAME_RAISED)
      (qt-frame-set-line-width! dst-frame 2)

      (qt-frame-set-shape! drop-zone-frame QT_FRAME_STYLED_PANEL)
      (qt-frame-set-shadow! drop-zone-frame QT_FRAME_SUNKEN)
      (qt-frame-set-mid-line-width! drop-zone-frame 2)

      ;; --- Source list items ---
      (for-each (lambda (item) (qt-list-widget-add-item! src-list item))
        ["Apple" "Banana" "Cherry" "Date" "Elderberry"
         "Fig" "Grape" "Honeydew" "Kiwi" "Lemon"])

      ;; --- Labels ---
      (qt-label-set-alignment! src-label QT_ALIGN_CENTER)
      (qt-label-set-alignment! dst-label QT_ALIGN_CENTER)
      (qt-label-set-alignment! drop-zone QT_ALIGN_CENTER)
      (qt-widget-set-style-sheet! src-label "font-weight: bold; padding: 4px;")
      (qt-widget-set-style-sheet! dst-label "font-weight: bold; padding: 4px;")
      (qt-widget-set-style-sheet! drop-zone
        "font-size: 14px; padding: 20px; background: #eef; border: 2px dashed #99c;")
      (qt-widget-set-minimum-size! drop-zone 200 80)

      ;; --- Enable drops on destination list ---
      (qt-widget-set-accept-drops! dst-list #t)
      (let ((filter (qt-on-drop! dst-list
                      (lambda (text)
                        (qt-list-widget-add-item! dst-list text)
                        (qt-plain-text-edit-append! drop-log
                          (format "Dropped to list: ~a" text))))))
        (set! drop-filters (cons filter drop-filters)))

      ;; --- Enable drops on the drop zone label ---
      (qt-widget-set-accept-drops! drop-zone #t)
      (let ((filter (qt-on-drop! drop-zone
                      (lambda (text)
                        (qt-label-set-text! drop-zone
                          (format "Received: ~a" text))
                        (qt-plain-text-edit-append! drop-log
                          (format "Dropped to zone: ~a" text))))))
        (set! drop-filters (cons filter drop-filters)))

      ;; --- Double-click to initiate drag ---
      (qt-on-item-double-clicked! src-list
        (lambda (row)
          (let ((text (qt-list-widget-item-text src-list row)))
            (qt-plain-text-edit-append! drop-log
              (format "Dragging: ~a" text))
            (qt-drag-text! src-list text))))

      ;; --- Log area ---
      (qt-plain-text-edit-set-read-only! drop-log #t)
      (qt-plain-text-edit-set-max-block-count! drop-log 100)

      ;; --- Assemble source frame ---
      (qt-layout-add-widget! src-layout src-label)
      (qt-layout-add-widget! src-layout src-list)

      ;; --- Assemble dest frame ---
      (qt-layout-add-widget! dst-layout dst-label)
      (qt-layout-add-widget! dst-layout dst-list)

      ;; --- Assemble drop zone ---
      (qt-layout-add-widget! drop-zone-layout drop-zone)
      (qt-layout-add-widget! drop-zone-layout drop-log)

      ;; --- Lists row ---
      (qt-layout-add-widget! lists-layout src-frame)
      (qt-layout-add-widget! lists-layout dst-frame)

      ;; --- Main layout ---
      (qt-layout-add-widget! main-layout instructions)
      (qt-layout-add-widget! main-layout lists-row)
      (qt-layout-add-widget! main-layout drop-zone-frame)

      ;; --- Show ---
      (qt-main-window-set-central-widget! win central)
      (qt-main-window-set-title! win "Drag & Drop Demo")
      (qt-widget-resize! win 700 550)
      (qt-widget-show! win)
      (qt-app-exec! app)

      ;; Cleanup drop filters
      (for-each qt-drop-filter-destroy! drop-filters))))

(main)
