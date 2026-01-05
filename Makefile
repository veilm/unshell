.PHONY: build install test fmt clean run

FEATURE_FLAGS :=
ifeq ($(REPL),off)
FEATURE_FLAGS := --no-default-features
endif

build:
	cargo build --release $(FEATURE_FLAGS)

install:
	@if [ ! -x target/release/ush ]; then \
		echo "ush binary missing. run 'make build' first."; \
		exit 1; \
	fi
	@bin_dir="$(DESTDIR)/usr/local/bin"; \
	init_dir="$(DESTDIR)/etc/unshell"; \
	mkdir -p "$$bin_dir"; \
	install -m755 util/expansion_handler.py "$$bin_dir/ush-expansion-handler"; \
	install -m755 util/quote "$$bin_dir/ush-quote"; \
	dest="$$bin_dir/ush"; \
	if [ -f "$$dest" ]; then \
		if cmp -s target/release/ush "$$dest"; then \
			echo "ush unchanged; skipping install."; \
		else \
			install -m755 target/release/ush "$$dest"; \
		fi; \
	else \
		install -m755 target/release/ush "$$dest"; \
	fi
	@if [ ! -f "$$init_dir/init" ]; then \
		mkdir -p "$$init_dir"; \
		install -m644 util/unshell_init "$$init_dir/init"; \
	fi

test:
	cargo test $(FEATURE_FLAGS)

fmt:
	cargo fmt

clean:
	cargo clean

run:
	cargo run --bin ush $(FEATURE_FLAGS)
