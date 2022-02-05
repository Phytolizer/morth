.PHONY: all cpp rs

all: cpp rs
cpp:
	cmake --build build
rs:
	cargo build --manifest-path porth/Cargo.toml

