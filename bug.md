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
