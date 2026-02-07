#!/usr/bin/env gxi
;;; wizard.ss — Multi-step setup wizard with radio buttons, button groups, and group boxes.
;;;
;;; Demonstrates: QWizard, QWizardPage, QRadioButton, QButtonGroup,
;;; QGroupBox, QDialogButtonBox, qt-on-radio-toggled!, qt-on-button-group-clicked!,
;;; qt-on-group-box-toggled!, QFormLayout.

(import :gerbil-qt/qt
        :std/format)

(def (main)
  (with-qt-app app
    (let* ((win (qt-main-window-create))
           (central (qt-widget-create))
           (vlayout (qt-vbox-layout-create central))
           (log-area (qt-plain-text-edit-create))
           (start-btn (qt-push-button-create "Launch Setup Wizard"))
           (result-label (qt-label-create "No wizard run yet")))

      (qt-plain-text-edit-set-read-only! log-area #t)
      (qt-widget-set-font! log-area "Monospace" 9)

      (qt-layout-add-widget! vlayout start-btn)
      (qt-layout-add-widget! vlayout result-label)
      (qt-layout-add-widget! vlayout log-area)

      ;; Launch wizard on click
      (qt-on-clicked! start-btn
        (lambda ()
          (qt-plain-text-edit-append! log-area "--- Starting wizard ---")

          (let* ((wizard (qt-wizard-create win))

                 ;; --- Page 1: Theme selection with radio buttons + button group ---
                 (page1 (qt-wizard-page-create))
                 (p1-layout (qt-vbox-layout-create))
                 (theme-group-box (qt-group-box-create "Choose Theme"))
                 (theme-layout (qt-vbox-layout-create theme-group-box))
                 (rb-light (qt-radio-button-create "Light Theme"))
                 (rb-dark (qt-radio-button-create "Dark Theme"))
                 (rb-auto (qt-radio-button-create "Auto (System)"))
                 (btn-group (qt-button-group-create))

                 ;; --- Page 2: Feature toggles with checkable group boxes ---
                 (page2 (qt-wizard-page-create))
                 (p2-layout (qt-vbox-layout-create))
                 (feat-notify (qt-group-box-create "Enable Notifications"))
                 (notify-layout (qt-vbox-layout-create feat-notify))
                 (notify-info (qt-label-create "Receive desktop notifications for events"))
                 (feat-auto-save (qt-group-box-create "Enable Auto-Save"))
                 (auto-layout (qt-vbox-layout-create feat-auto-save))
                 (auto-info (qt-label-create "Save documents every 5 minutes"))

                 ;; --- Page 3: Summary ---
                 (page3 (qt-wizard-page-create))
                 (p3-layout (qt-vbox-layout-create))
                 (summary-label (qt-label-create "Review your choices:"))
                 (summary-text (qt-plain-text-edit-create)))

            ;; === Page 1: Theme ===
            (qt-wizard-page-set-title! page1 "Theme Selection")
            (qt-wizard-page-set-subtitle! page1 "Choose your preferred appearance")

            ;; Radio buttons in group
            (qt-button-group-add-button! btn-group rb-light 1)
            (qt-button-group-add-button! btn-group rb-dark 2)
            (qt-button-group-add-button! btn-group rb-auto 3)
            (qt-radio-button-set-checked! rb-auto #t)  ;; default

            ;; Log radio toggle events
            (qt-on-radio-toggled! rb-light
              (lambda (checked)
                (when checked
                  (qt-plain-text-edit-append! log-area "Theme: Light selected"))))
            (qt-on-radio-toggled! rb-dark
              (lambda (checked)
                (when checked
                  (qt-plain-text-edit-append! log-area "Theme: Dark selected"))))
            (qt-on-radio-toggled! rb-auto
              (lambda (checked)
                (when checked
                  (qt-plain-text-edit-append! log-area "Theme: Auto selected"))))

            ;; Button group clicked signal
            (qt-on-button-group-clicked! btn-group
              (lambda (id)
                (qt-plain-text-edit-append! log-area
                  (format "Button group clicked: id=~a" id))))

            ;; Assemble page 1
            (qt-layout-add-widget! theme-layout rb-light)
            (qt-layout-add-widget! theme-layout rb-dark)
            (qt-layout-add-widget! theme-layout rb-auto)
            (qt-layout-add-widget! p1-layout theme-group-box)
            (qt-layout-add-stretch! p1-layout)
            (qt-wizard-page-set-layout! page1 p1-layout)

            ;; === Page 2: Features ===
            (qt-wizard-page-set-title! page2 "Feature Configuration")
            (qt-wizard-page-set-subtitle! page2 "Enable or disable optional features")

            ;; Checkable group boxes
            (qt-group-box-set-checkable! feat-notify #t)
            (qt-group-box-set-checked! feat-notify #t)
            (qt-layout-add-widget! notify-layout notify-info)

            (qt-group-box-set-checkable! feat-auto-save #t)
            (qt-group-box-set-checked! feat-auto-save #f)
            (qt-layout-add-widget! auto-layout auto-info)

            ;; Group box toggle signals
            (qt-on-group-box-toggled! feat-notify
              (lambda (checked)
                (qt-plain-text-edit-append! log-area
                  (format "Notifications: ~a" (if checked "enabled" "disabled")))))
            (qt-on-group-box-toggled! feat-auto-save
              (lambda (checked)
                (qt-plain-text-edit-append! log-area
                  (format "Auto-save: ~a" (if checked "enabled" "disabled")))))

            ;; Assemble page 2
            (qt-layout-add-widget! p2-layout feat-notify)
            (qt-layout-add-spacing! p2-layout 10)
            (qt-layout-add-widget! p2-layout feat-auto-save)
            (qt-layout-add-stretch! p2-layout)
            (qt-wizard-page-set-layout! page2 p2-layout)

            ;; === Page 3: Summary ===
            (qt-wizard-page-set-title! page3 "Summary")
            (qt-wizard-page-set-subtitle! page3 "Confirm your settings")
            (qt-plain-text-edit-set-read-only! summary-text #t)
            (qt-layout-add-widget! p3-layout summary-label)
            (qt-layout-add-widget! p3-layout summary-text)
            (qt-wizard-page-set-layout! page3 p3-layout)

            ;; Track page changes to update summary
            (qt-wizard-on-current-changed! wizard
              (lambda (page-id)
                (qt-plain-text-edit-append! log-area
                  (format "Wizard page changed to: ~a" page-id))
                ;; Update summary on page 3 (id=2, 0-indexed)
                (when (= page-id 2)
                  (let* ((theme-id (qt-button-group-checked-id btn-group))
                         (theme-name
                           (cond ((= theme-id 1) "Light")
                                 ((= theme-id 2) "Dark")
                                 ((= theme-id 3) "Auto")
                                 (else "Unknown")))
                         (notify? (qt-group-box-checked? feat-notify))
                         (autosave? (qt-group-box-checked? feat-auto-save)))
                    (qt-plain-text-edit-set-text! summary-text
                      (format "Theme: ~a\nNotifications: ~a\nAuto-save: ~a"
                        theme-name
                        (if notify? "Enabled" "Disabled")
                        (if autosave? "Enabled" "Disabled")))))))

            ;; Add pages and configure wizard
            (qt-wizard-add-page! wizard page1)
            (qt-wizard-add-page! wizard page2)
            (qt-wizard-add-page! wizard page3)
            (qt-wizard-set-title! wizard "Application Setup")

            ;; Run the wizard (modal)
            (let ((result (qt-wizard-exec! wizard)))
              (if (= result 1)
                (begin
                  (qt-label-set-text! result-label "Wizard: Accepted")
                  (qt-plain-text-edit-append! log-area "Wizard completed (accepted)"))
                (begin
                  (qt-label-set-text! result-label "Wizard: Cancelled")
                  (qt-plain-text-edit-append! log-area "Wizard cancelled"))))

            ;; Cleanup button group
            (qt-button-group-destroy! btn-group))))

      ;; Show main window
      (qt-main-window-set-title! win "Wizard Demo")
      (qt-main-window-set-central-widget! win central)
      (qt-widget-resize! win 500 400)
      (qt-widget-show! win)
      (qt-app-exec! app))))

(main)
