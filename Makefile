export PKG_CONFIG_PATH := /usr/lib/x86_64-linux-gnu/pkgconfig:$(PKG_CONFIG_PATH)
export GERBIL_LOADPATH := $(HOME)/.gerbil/lib

.PHONY: build test clean demo-hello demo-counter demo-form demo-editor demo-dashboard demo-filebrowser demo-styled demo-settings demo-painter demo-datainput demo-planner

build:
	gerbil build

test: build
	@QT_QPA_PLATFORM=offscreen LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH \
		gerbil test ./... > /tmp/gerbil-qt-test.log 2>&1 || true; \
	cat /tmp/gerbil-qt-test.log; \
	if grep -q "FAILURE" /tmp/gerbil-qt-test.log; then \
		exit 1; \
	fi

clean:
	gerbil clean
	rm -f vendor/libqt_shim.so

demo-hello: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/hello.ss

demo-counter: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/counter.ss

demo-form: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/form.ss

demo-editor: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/editor.ss

demo-dashboard: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/dashboard.ss

demo-filebrowser: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/filebrowser.ss

demo-styled: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/styled.ss

demo-settings: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/settings.ss

demo-painter: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/painter.ss

demo-datainput: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/datainput.ss

demo-planner: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi examples/planner.ss
