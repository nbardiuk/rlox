.PHONY: tdd
tdd:
	cargo watch --clear --shell "time cargo test $(only) -q -- --nocapture"

.PHONY: bench
bench:
	cargo bench -- $(only)

.PHONY: test
test:
	cargo test $(only)

.PHONY: clean
clean:
	cargo clean

.PHONY: vm-repl
vm-repl:
	cargo run --bin vm

.PHONY: in-repl
in-repl:
	cargo run --bin interpreter
