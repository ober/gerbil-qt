#!/usr/bin/env bash
# Build script for Qt SIGCHLD bug demo executables

set -e

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENDOR_DIR="$HERE/vendor"

# Set PKG_CONFIG_PATH for Qt6
ARCH=$(gcc -dumpmachine)
SYSDIR="/usr/lib/$ARCH/pkgconfig"
if [ -d "$SYSDIR" ]; then
    export PKG_CONFIG_PATH="$SYSDIR:${PKG_CONFIG_PATH:-}"
fi

# Get Qt6 flags
QT_CFLAGS=$(pkg-config --cflags Qt6Widgets)
QT_LIBS=$(pkg-config --libs Qt6Widgets)

echo "Building test-process-deadlock-bad..."
gxc -exe \
    -cc-options "-I$VENDOR_DIR $QT_CFLAGS" \
    -ld-options "-L$VENDOR_DIR -lqt_shim -Wl,-rpath,$VENDOR_DIR $QT_LIBS" \
    -o test-process-deadlock-bad \
    test-process-deadlock-bad.ss

echo "Building test-process-deadlock-good..."
gxc -exe \
    -cc-options "-I$VENDOR_DIR $QT_CFLAGS" \
    -ld-options "-L$VENDOR_DIR -lqt_shim -Wl,-rpath,$VENDOR_DIR $QT_LIBS" \
    -o test-process-deadlock-good \
    test-process-deadlock-good.ss

echo "Done! Executables created:"
echo "  - test-process-deadlock-bad (WILL HANG)"
echo "  - test-process-deadlock-good (SAFE)"
