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

;; Detect QScintilla availability: try pkg-config first, then check for headers
(def have-qscintilla?
  (or (with-catch (lambda (_) #f)
        (lambda ()
          (run-process ["pkg-config" "--exists" "QScintilla"] coprocess: void)
          #t))
      (let ((arch (with-catch (lambda (_) "x86_64-linux-gnu")
                    (lambda () (run-process ["gcc" "-dumpmachine"] coprocess: read-line)))))
        (or (file-exists? (string-append "/usr/include/" arch "/qt6/Qsci/qsciscintilla.h"))
            (file-exists? "/usr/include/qt6/Qsci/qsciscintilla.h")
            (file-exists? "/usr/include/Qsci/qsciscintilla.h")
            (file-exists? (string-append "/usr/include/" arch "/Qsci/qsciscintilla.h"))))))

(when have-qscintilla?
  (displayln "... QScintilla detected — enabling QScintilla bindings"))

;; QScintilla compiler/linker flags
(def qsci-cppflags
  (if have-qscintilla?
    (with-catch (lambda (_) "-DQT_SCINTILLA_AVAILABLE")
      (lambda () (string-append "-DQT_SCINTILLA_AVAILABLE "
                                (run-process ["pkg-config" "--cflags" "QScintilla"]
                                             coprocess: read-line))))
    ""))

(def qsci-ldflags
  (if have-qscintilla?
    (with-catch (lambda (_) "-lqscintilla2_qt6")
      (lambda () (run-process ["pkg-config" "--libs" "QScintilla"]
                              coprocess: read-line)))
    ""))


;; Build vendor/libqt_shim.so if it doesn't exist or is older than sources
(unless (and (file-exists? shim-so)
             (>= (time->seconds (file-info-last-modification-time (file-info shim-so)))
                 (max (time->seconds (file-info-last-modification-time (file-info shim-cpp)))
                      (time->seconds (file-info-last-modification-time (file-info shim-h))))))
  (displayln "... compile vendor/libqt_shim.so")
  (run-process ["g++" "-shared" "-fPIC" "-std=c++17" "-o" shim-so
                shim-cpp
                (string-split (string-append (cppflags "Qt6Widgets" "")
                                             " " qsci-cppflags) #\space) ...
                (string-split (string-append (ldflags "Qt6Widgets" "-lQt6Widgets")
                                             " " qsci-ldflags) #\space) ...]
               coprocess: void))

(defbuild-script
  `((gxc: "libqt"
          "-cc-options" ,(string-append
                           "-I" vendor-dir " "
                           (filter-broad-includes (cppflags "Qt6Widgets" ""))
                           " " qsci-cppflags)
          "-ld-options" ,(string-append
                           "-L" vendor-dir " -lqt_shim "
                           "-Wl,-rpath," vendor-dir " "
                           (ldflags "Qt6Widgets" "-lQt6Widgets")
                           " " qsci-ldflags))
    "qt"))
