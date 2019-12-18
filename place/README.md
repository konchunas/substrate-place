# substrateplace

The Place module is a huge pixel drawing board on Substrate

# Building

Install Rust:

```bash
curl https://sh.rustup.rs -sSf | sh
```

Install required tools:

```bash
./scripts/init.sh
```

Build the WebAssembly binary:

```bash
./scripts/build.sh
```

Build all native code:

```bash
cargo build
```

# Run

You can start a development chain with:

```bash
cargo run -- --dev
```

# UI

There is web UI required for this module to make more sense. It displays this module storage as Pixel board and allows to purchase your very own pixels.
To try it in action follow https://github.com/konchunas/substrate-place-ui
