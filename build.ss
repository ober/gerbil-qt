#!/usr/bin/env gxi
(import :std/build-script
        :std/make
        :std/misc/process)

;; Ensure pkg-config finds system Qt6 packages (portable across architectures)
(let* ((current (getenv "PKG_CONFIG_PATH" ""))
       (arch (with-catch (lambda (_) "x86_64-linux-gnu")
               (lambda () (run-process ["gcc" "-dumpmachine"] coprocess: read-line))))
       (sysdir (string-append "/usr/lib/" arch "/pkgconfig")))
  (when (and (file-exists? sysdir)
             (not (string-contains current sysdir)))
    (setenv "PKG_CONFIG_PATH"
            (if (string-empty? current) sysdir
                (string-append sysdir ":" current)))))

;; Filter broad -I/opt/homebrew/include from pkg-config output to prevent
;; Gambit header conflicts when homebrew's gerbil-scheme package is installed
(def (filter-broad-includes flags)
  (string-join
    (filter (lambda (p) (not (equal? p "-I/opt/homebrew/include")))
            (string-split flags #\space))
    " "))

(def here (path-directory (this-source-file)))
(def vendor-dir (path-expand "vendor" here))
(def shim-so (path-expand "libqt_shim.so" vendor-dir))
(def shim-cpp (path-expand "qt_shim.cpp" vendor-dir))
(def shim-h (path-expand "qt_shim.h" vendor-dir))


;; Build vendor/libqt_shim.so if it doesn't exist or is older than sources
(unless (and (file-exists? shim-so)
             (>= (time->seconds (file-info-last-modification-time (file-info shim-so)))
                 (max (time->seconds (file-info-last-modification-time (file-info shim-cpp)))
                      (time->seconds (file-info-last-modification-time (file-info shim-h))))))
  (displayln "... compile vendor/libqt_shim.so")
  (run-process ["g++" "-shared" "-fPIC" "-std=c++17" "-o" shim-so
                shim-cpp
                (string-split (cppflags "Qt6Widgets" "") #\space) ...
                (string-split (ldflags "Qt6Widgets" "-lQt6Widgets") #\space) ...]
               coprocess: void))

(defbuild-script
  `((gxc: "libqt"
          "-cc-options" ,(string-append
                           "-I" vendor-dir " "
                           (filter-broad-includes (cppflags "Qt6Widgets" "")))
          "-ld-options" ,(string-append
                           "-L" vendor-dir " -lqt_shim "
                           "-Wl,-rpath," vendor-dir " "
                           (ldflags "Qt6Widgets" "-lQt6Widgets")))
    "qt"))
