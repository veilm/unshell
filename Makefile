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
	install -Dm755 target/release/ush $(DESTDIR)/usr/local/bin/ush

test:
	cargo test $(FEATURE_FLAGS)

fmt:
	cargo fmt

clean:
	cargo clean

run:
	cargo run --bin ush $(FEATURE_FLAGS)
