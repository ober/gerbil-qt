export PKG_CONFIG_PATH := /usr/lib/x86_64-linux-gnu/pkgconfig:$(PKG_CONFIG_PATH)
export GERBIL_LOADPATH := $(HOME)/.gerbil/lib

GERBIL_LIB := $(HOME)/.gerbil/lib
INSTALL_DIR := $(GERBIL_LIB)/gerbil-qt
STATIC_DIR := $(GERBIL_LIB)/static

EXAMPLES := $(wildcard examples/*.ss)
DEMO_TARGETS := $(patsubst examples/%.ss,demo-%,$(EXAMPLES))

.PHONY: build test clean install uninstall $(DEMO_TARGETS)

build:
	gerbil build

install: build
	@mkdir -p $(INSTALL_DIR) $(STATIC_DIR)
	cp -f .gerbil/lib/gerbil-qt/* $(INSTALL_DIR)/
	cp -f vendor/libqt_shim.so $(INSTALL_DIR)/
	@if ls .gerbil/lib/static/gerbil-qt__* >/dev/null 2>&1; then \
		cp -f .gerbil/lib/static/gerbil-qt__* $(STATIC_DIR)/; \
	fi
	@if command -v patchelf >/dev/null 2>&1; then \
		for f in $(INSTALL_DIR)/libqt~0.o*; do \
			patchelf --add-rpath $(INSTALL_DIR) "$$f" 2>/dev/null || true; \
		done; \
	fi
	@echo "Installed to $(INSTALL_DIR)"
	@echo "Run any example: gxi examples/hello.ss"

uninstall:
	rm -rf $(INSTALL_DIR)
	rm -f $(STATIC_DIR)/gerbil-qt__*
	@echo "Uninstalled gerbil-qt"

test: install
	@QT_QPA_PLATFORM=offscreen gerbil test ./... > /tmp/gerbil-qt-test.log 2>&1 || true; \
	cat /tmp/gerbil-qt-test.log; \
	if grep -q "FAILURE" /tmp/gerbil-qt-test.log; then \
		exit 1; \
	fi

clean:
	gerbil clean
	rm -f vendor/libqt_shim.so

# All demo targets: make demo-hello, make demo-styled, etc.
$(DEMO_TARGETS): demo-%: install
	gxi examples/$*.ss
