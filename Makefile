.PHONY: build install test fmt clean run

build:
	cargo build --release

install:
	@if [ ! -x target/release/ush ]; then \
		echo "ush binary missing. run 'make build' first."; \
		exit 1; \
	fi
	install -Dm755 target/release/ush $(DESTDIR)/usr/local/bin/ush

test:
	cargo test

fmt:
	cargo fmt

clean:
	cargo clean

run:
	cargo run --bin ush
