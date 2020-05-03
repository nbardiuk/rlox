cargo-args=
ifdef features
	cargo-args=--features $(features)
endif

.PHONY: tdd
tdd:
	cargo watch --clear --shell "time cargo test $(only) $(cargo-args) -q -- --nocapture"

.PHONY: bench
bench:
	cargo bench -- $(only) $(cargo-args)

.PHONY: test
test:
	cargo test $(only) $(cargo-args)

.PHONY: clean
clean:
	cargo clean

.PHONY: vm-repl
vm-repl:
	cargo run --bin vm $(cargo-args)

.PHONY: in-repl
in-repl:
	cargo run --bin interpreter $(cargo-args)

