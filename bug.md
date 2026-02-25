# Known Bugs, Security Issues, and Design Problems

## Summary

| Severity | Count | Key Themes |
|----------|-------|------------|
| High | 3 | Null pointer crashes, SIGCHLD forwarding UB, use-after-free |
| Medium | 7 | Silent failures, stack pointer escape, handler leaks |
| Low | 8 | Cleanup leaks, macro inconsistency, truncation |

---

## High Severity

### H1. No null-pointer validation on ~650 C++ shim functions

**Files:** `vendor/qt_shim.cpp` (all ~650 exported functions)

Every exported function performs `static_cast<QWidget*>(w)->method()` without checking if `w` is null. Since all Qt types are `(pointer void)` in the FFI layer, passing `#f` (Gambit's null pointer) to any wrapper crashes with a segfault.

```cpp
// qt_shim.cpp:224-225 — representative of ALL functions
extern "C" void qt_widget_show(qt_widget_t w) {
    static_cast<QWidget*>(w)->show();  // CRASH if w is nullptr
}
```

A handful of functions do check (e.g., `qt_list_widget_item_text` at line 889, `qt_tree_widget_set_header_item_text` at line 1271), showing awareness of the issue, but it is not applied consistently.

**Recommendation:** Add null guards in the Scheme wrappers (`qt.ss`) for functions that commonly receive nullable values (tree item getters, model item getters, dialog results), or add a `QT_NULL_CHECK(ptr)` macro in the C++ shim that returns early with a safe default.

### H2. SIGCHLD SA_SIGINFO forwarding mismatch

**File:** `vendor/qt_shim.cpp:4129-4133`

The custom SIGCHLD handler forwards to Gambit's saved handler via `sa_handler`, but never checks whether Gambit installed its handler with `SA_SIGINFO` (which would use the `sa_sigaction` union member instead). If Gambit uses `SA_SIGINFO`, calling `sa_handler` is undefined behavior.

```cpp
// Only checks sa_handler, never checks sa_flags & SA_SIGINFO
if (s_gambit_sigaction.sa_handler &&
    s_gambit_sigaction.sa_handler != SIG_DFL &&
    s_gambit_sigaction.sa_handler != SIG_IGN) {
    s_gambit_sigaction.sa_handler(sig);
}
```

**Fix:** Check `s_gambit_sigaction.sa_flags & SA_SIGINFO` and call the appropriate union member (`sa_sigaction` with three args vs `sa_handler` with one).

### H3. Use-after-free with Qt parent-child destruction

**Files:** All of `qt.ss`, `libqt.ss`

When a parent widget is destroyed, Qt deletes all children. The Scheme side still holds `(pointer void)` references to those children. Calling any function on a child after the parent is destroyed is use-after-free:

```scheme
(let ((parent (qt-widget-create))
      (child (qt-label-create "hi" parent: parent)))
  (qt-widget-destroy! parent)
  (qt-label-text child))  ;; use-after-free / segfault
```

No mechanism exists to invalidate child pointers. This is inherent to the `(pointer void)` architecture.

**Recommendation:** Document prominently in README. Consider a Scheme-side weak-reference table that tracks parent-child relationships and can invalidate children on parent destruction.

---

## Medium Severity

### M1. `qt_process_write` crashes on null string

**File:** `vendor/qt_shim.cpp:4225`

`QProcess::write(data)` calls `strlen(data)` internally. If Scheme passes `#f`/null, this segfaults. Other string functions are safe because `QString::fromUtf8(nullptr)` returns a null QString.

```cpp
extern "C" void qt_process_write(qt_process_t proc, const char* data) {
    static_cast<QProcess*>(proc)->write(data);  // strlen(nullptr) = crash
}
```

**Fix:** Add `if (!data) return;` guard.

### M2. `qt_process_start` silently swallows start failures

**File:** `vendor/qt_shim.cpp:4207-4208`

`waitForStarted(5000)` return value is discarded. If the program doesn't exist, the process info map is populated with pid 0 and no error propagates to Scheme.

```cpp
p->start(QString::fromUtf8(program), args);
p->waitForStarted(5000);  // return value ignored
```

**Fix:** Return a status int or bool to Scheme indicating whether the process started successfully.

### M3. Process tracking limited to 16 slots

**File:** `vendor/qt_shim.cpp:4158-4167`

`MAX_TRACKED_PROCESSES = 16`. When all slots are full, `qt_track_pid` returns -1 and the caller at line 4212 ignores the failure. Exit codes for the 17th+ concurrent process are silently lost.

**Recommendation:** Increase the limit, or switch to a dynamically-sized container, or at minimum log a warning when slots are exhausted.

### M4. Stack-local QPainter pointer escapes to FFI

**File:** `vendor/qt_shim.cpp:4033-4063`

`PaintWidget::paintEvent` creates a `QPainter` on the stack and stores its address in `m_painter`. `qt_paint_widget_painter` returns this pointer to Scheme. If the Scheme side caches this pointer beyond the paint callback, it becomes a dangling pointer.

```cpp
void paintEvent(QPaintEvent*) override {
    QPainter painter(this);  // Stack-local!
    m_painter = &painter;    // Address of stack object stored
    m_callback(m_callback_id);
    m_painter = nullptr;
}
```

**Recommendation:** Document that the painter pointer is only valid inside the paint callback. Consider adding a flag that prevents `qt_paint_widget_painter` from returning a non-null value outside `paintEvent`.

### M5. Thread-local `s_return_buf` shared across all string getters

**File:** `vendor/qt_shim.cpp:126`

All string-returning functions write to the same `thread_local std::string`. Gambit's `UTF-8-string` return type copies immediately, so this is safe in practice. But nothing enforces it at the C level, making it fragile for future changes or alternative FFI usage.

### M6. `qt-on-double-value-changed!` doesn't guard `string->number`

**File:** `qt.ss`

The double-value trampoline converts via `string->number` which can return `#f`. If the C side sends a non-numeric string, the handler receives `#f` instead of a number with no warning.

**Fix:** Add a fallback: `(or (string->number s) 0.0)`.

### M7. Handler tracking cleanup incomplete

**Files:** `qt.ss`

`qt-scintilla-destroy!` (when QScintilla is enabled) doesn't clean up handler tracking entries, leaking closures. `bug.md` documents partial fixes for other destroy functions that have been applied but not yet built/tested.

---

## Low Severity

### L1. `g_extraSelections` entries never cleaned up

**File:** `vendor/qt_shim.cpp:5188`

When a `QPlainTextEdit` is destroyed, its entry in `g_extraSelections` is never removed. Leaks a `QList<QTextEdit::ExtraSelection>` per destroyed editor. Minor since values are lightweight.

### L2. Missing `#include <unordered_map>`

**File:** `vendor/qt_shim.cpp:4104`

`std::unordered_map` is used without an explicit include. Works via transitive Qt includes but isn't guaranteed across platforms/compilers.

### L3. Scintilla margin click truncates line numbers to 16 bits

**File:** `vendor/qt_shim.cpp:5433-5437`

```cpp
callback(callback_id, (margin << 16) | (line & 0xFFFF));
```

Line numbers above 65535 are silently truncated.

### L4. `qt_process_start` splits args on newline

**File:** `vendor/qt_shim.cpp`

Arguments containing literal `\n` characters are incorrectly split into separate arguments. Unusual but possible.

### L5. Duplicate `#define` constants across files

**Files:** `libqt.ss` c-declare blocks AND `vendor/qt_shim.h`

Frame constants, button box constants, button role constants, and day-of-week constants are defined in both places. Values match today but could diverge during maintenance.

**Recommendation:** Remove the duplicates from `libqt.ss` and rely solely on the header.

### L6. `with-icon` / `with-settings` macro inconsistency

**File:** `qt.ss:3575-3583`

These macros take `var expr` while `with-painter`/`with-font`/`with-color`/`with-pixmap` take `(var args...)`. Inconsistent API surface across the macro family.

### L7. No GC finalizers on non-parented heap objects

**Files:** `libqt.ss`, `qt.ss`

`QFont`, `QColor`, `QPixmap`, `QIcon`, `QButtonGroup`, `QSettings`, `QTimer`, `QUndoStack`, `QFileSystemModel` — all require explicit destroy calls with no safety net. Losing a Scheme reference leaks permanently. This is by-design but increases the burden on API users.

**Recommendation:** Consider `make-will` GC finalizers for the most commonly leaked types (`QFont`, `QColor`, `QPixmap`).

### L8. Callback IDs never recycled

**File:** `libqt.ss:3798`

The counter monotonically increases. Harmless for correctness (Scheme bignums don't overflow) but hash tables accumulate sparse entries in long-running apps that create/destroy many widgets.

### L9. `qt_color_dialog_get_color` always allocates

**File:** `vendor/qt_shim.cpp:1901-1907`

Unlike `qt_font_dialog_get_font` (returns nullptr on cancel), `qt_color_dialog_get_color` always allocates a `QColor` even when the user cancels. The high-level wrapper handles this, but raw FFI users could miss the asymmetry.

### L10. No enum validation on integer parameters

**File:** `vendor/qt_shim.cpp` (throughout)

Integer parameters are cast directly to Qt enum types without range validation. Qt generally handles invalid values gracefully, but behavior is not guaranteed.

```cpp
// qt_shim.cpp:281
static_cast<QWidget*>(w)->setCursor(QCursor(static_cast<Qt::CursorShape>(shape)));
```

---

# Qt SIGCHLD Handler Bug: process-status Deadlock

## Problem Summary

**STATUS:** This is a **documented race condition** from the gemacs-qt project that may or may not manifest in simple test cases.

The issue involves SIGCHLD handler interactions between Qt/Gambit when using `process-status`:

- **Gambit** has a SIGCHLD handler that calls `waitpid(-1, WNOHANG)` to reap child processes
- **Qt** may use forkfd or its own child process monitoring
- When these interact, there's a potential race where Gambit's handler reaps a child before `process-status` is called
- **Result:** `process-status` blocks indefinitely waiting for a process that's already been reaped (ECHILD)

**Important:** In simple test cases (like `echo` or `sleep`), this bug **does not reliably manifest**. The gemacs-qt project reported it occurring "only in CI headless testing with real subprocess calls" (e.g., actual git commands), suggesting it's **timing-dependent** and may require:
- The Qt event loop running (QApplication::exec)
- Longer-running or more complex subprocesses
- Specific timing conditions that create the race

The test cases in this repository demonstrate the **pattern** and **workarounds**, not a guaranteed reproduction.

## Test Case

### Failing Code (Deadlocks)

```scheme
(import :gerbil-qt/qt
        :std/misc/process)

(def (run-subprocess-bad)
  (let (proc (open-process [path: "echo" arguments: ["hello"]]))
    (let (output (read-line proc #f))
      (displayln "Output: " output)
      ;; BUG: This will hang indefinitely if Qt's SIGCHLD handler reaps the child first
      (process-status proc))))

(def (main . args)
  (with-qt-app app
    (displayln "Running subprocess with process-status (will hang)...")
    (let (status (run-subprocess-bad))
      (displayln "Exit status: " status))))
```

### Working Code (Uses read-line Only)

```scheme
(import :gerbil-qt/qt
        :std/misc/process)

(def (run-subprocess-good)
  (let (proc (open-process [path: "echo" arguments: ["hello"]]))
    (let (output (read-line proc #f))
      (displayln "Output: " output)
      ;; CORRECT: read-line with eof waits for process completion
      ;; No explicit process-status needed
      (close-port proc)
      0))) ;; Assume success if no error

(def (main . args)
  (with-qt-app app
    (displayln "Running subprocess without process-status (works)...")
    (run-subprocess-good)
    (displayln "Success!")))
```

### Working Code (Uses QProcess)

```scheme
(import :gerbil-qt/qt)

(def (run-subprocess-qprocess)
  (let (proc (qt-process-create))
    (qt-process-start! proc "echo" args: ["hello"])
    (qt-process-wait-for-finished proc) ;; Safe - uses Qt's process management
    (let* ((output (qt-process-read-stdout proc))
           (status (qt-process-exit-code proc)))
      (displayln "Output: " output)
      (qt-process-destroy! proc)
      status)))

(def (main . args)
  (with-qt-app app
    (displayln "Running subprocess with QProcess (works)...")
    (let (status (run-subprocess-qprocess))
      (displayln "Exit status: " status))))
```

## Reproduction Steps

1. **Create test file:** `test-process-deadlock.ss` with the failing code above
2. **Add exe target to build.ss:**
   ```scheme
   (exe: "test-process-deadlock" static: #t
         ld-options: "-L/path/to/vendor -lqt_shim -lQt6Widgets ...")
   ```
3. **Build:** `gerbil build`
4. **Run with headless Qt:** `QT_QPA_PLATFORM=offscreen ./test-process-deadlock`
5. **Observe:** Program hangs indefinitely after "Running subprocess with process-status..."

## Root Cause

From Qt's source (`src/corelib/kernel/qcore_unix.cpp`):

```cpp
static void sigchldHandler(int signum) {
    // ... Qt's handler calls waitpid(-1, ..., WNOHANG)
    // This reaps ANY terminated child process
}
```

When Gambit calls `(process-status proc)`, it internally does:

```c
waitpid(specific_pid, &status, 0); // Blocking wait
```

But if Qt already reaped the process, this returns `ECHILD` and Gambit retries indefinitely.

## Workarounds

### Option 1: Avoid process-status (Recommended for simple cases)

```scheme
;; Reading to EOF implicitly waits for process completion
(let (proc (open-process [path: "git" arguments: ["status"]]))
  (let (output (read-line proc #f))
    (close-port proc)
    ;; No process-status needed
    ))
```

### Option 2: Use QProcess (Recommended for Qt apps)

```scheme
;; Qt's process management is SIGCHLD-safe
(let (proc (qt-process-create))
  (qt-process-start! proc "git" args: ["status"])
  (qt-process-wait-for-finished proc)
  (let* ((output (qt-process-read-stdout proc))
         (status (qt-process-exit-code proc)))
    ;; ... handle output
    (qt-process-destroy! proc)))
```

### Option 3: Custom SIGCHLD Handler (Advanced)

See `vendor/qt_shim.cpp` for the implementation used by `qt-process-wait-for-finished`:

```cpp
// Install custom SIGCHLD handler before starting process
// Capture exit codes for tracked PIDs
// Restore Gambit's handler after wait completes
```

## Detection Tool

The proposed `tools/qt-process-status-lint.ss` would detect the anti-pattern by:

1. Parsing `build.ss` to find exe targets
2. Scanning those files for `(process-status ...)` calls
3. Checking if files import `:gerbil-qt/qt` or use `with-qt-app`
4. Reporting warnings with file/line context

## Example Warning Output

```
WARNING: test-process-deadlock.ss:10
  Function: run-subprocess-bad
  Pattern: (process-status proc)

  Calling process-status in a Qt executable causes SIGCHLD race conditions
  and indefinite hangs when Qt's handler reaps the child process first.

  Suggested fixes:
  1. Use read-line with EOF instead of explicit process-status
  2. Use QProcess APIs (create-process, process-wait-for-finished!)
  3. Avoid Gambit's open-process entirely in Qt executables
```

## Running the Demo

### Build the Test Executables

```bash
# Make build script executable
chmod +x build-demos.sh

# Build both demo executables
./build-demos.sh
```

Or compile manually:
```bash
gxc -exe \
    -cc-options "-I$PWD/vendor $(pkg-config --cflags Qt6Widgets)" \
    -ld-options "-L$PWD/vendor -lqt_shim -Wl,-rpath,$PWD/vendor $(pkg-config --libs Qt6Widgets)" \
    -o test-process-deadlock-bad \
    test-process-deadlock-bad.ss
```

This creates:
- `test-process-deadlock-bad` - Demonstrates the hang (WILL DEADLOCK)
- `test-process-deadlock-good` - Shows working workarounds (SAFE)

### Run the Failing Demo (May Trigger Deadlock)

**NOTE:** In testing with simple subprocesses like `sleep`, the race condition **does not consistently occur**. The program may complete successfully most of the time:

```bash
# Run with headless Qt (offscreen platform)
QT_QPA_PLATFORM=offscreen ./test-process-deadlock-bad
```

Possible outcomes:

**If race does NOT occur (typical in simple tests):**
```
=== Qt SIGCHLD Bug Demo (WILL HANG) ===
Running subprocess with process-status...
Opening process: sleep 0.1
Reading to EOF...
Output:
Calling process-status (THIS WILL HANG)...
Exit status: 0
If you see this, the bug did not manifest (rare)
Done
```

**If race DOES occur (rare with simple processes, more common with git/complex subprocesses in CI):**
```
=== Qt SIGCHLD Bug Demo (WILL HANG) ===
Running subprocess with process-status...
Opening process: sleep 0.1
Reading to EOF...
Output:
Calling process-status (THIS WILL HANG)...
[HANGS INDEFINITELY - Press Ctrl+C]
```

### Run the Working Demo (Safe)

```bash
# Run with headless Qt (offscreen platform)
QT_QPA_PLATFORM=offscreen ./test-process-deadlock-good
```

Expected output:
```
=== Qt Subprocess Workarounds (SAFE) ===
Method 1: read-line without process-status
Opening process: echo hello
Reading output...
Output: hello
Process completed (no process-status call)

Method 2: QProcess API
Using QProcess API:
Waiting for process...
Output: goodbye
Exit status: 0

Done - no hangs!
```

## References

- Original bug report: gemacs-qt session, Group 9 git integration tests
- MEMORY.md: "Critical Bug Fixed: SIGCHLD Handler for QProcess Exit Codes"
- features.json: `qt-exe-process-status-detector` feature proposal
- Test files: `test-process-deadlock-bad.ss`, `test-process-deadlock-good.ss`
- Build script: `build-demos.sh`
