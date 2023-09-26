all:
	cargo build --release
	mv target/release/libcddl.a /usr/local/lib/libcddl.a
	cbindgen --crate cddl > cbits/libcddl.h
