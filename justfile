install:
	cargo install --path . --bin weresocool

build:
	cargo build

build-release:
	cargo build --release

release-macos: 
	./scripts/macos.sh

release-cargo:
	cargo-smart-release smart-release --bump patch

format-ci:
	cargo fmt --all --check

clippy:
	# cargo +nightly clippy --all-targets -- -D warnings
	cargo clippy --all-targets -- -D warnings

test:
	cargo test --workspace --release
	just test_snapshot

test_generated:
	cargo test --release _generated

test_snapshot:
	cargo run --release --example snapshot
test_rehash:
	cargo run --release --example snapshot -- --rehash

check-licenses: 
  cargo deny check licenses --hide-inclusion-graph

test-github-actions:
	act

