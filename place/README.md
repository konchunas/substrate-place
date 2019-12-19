# substrateplace

This is an experiment in storing a huge pixel board on blockchain using Substrate runtime. Being unrestricted by capabilities of runtime modules contrary to Smart Contracts gives us ability to keep and access vast amounts of data in an arbitrary manner. Pixel board is one of examples which should map onto blockchain nicely. For example, in Ethereum users have  256-bit addresses. So if we take them all and map onto virtual 2^128 per 2^128 grid and convert balance to color we may see some kind of picture.

In this project I took this concept in a bit other direction. It allows you to store a 2^35 x 2^35 pixel grid on a blockchain. You can purchase a pixel for yourself. But anyone can re-buy it from you for a larger price that you have initially paid.

(talk about influence from Reddit r/place and Satoshi's place)

## Storage

Grid is stored in chunk and yada-yada (continue from documentation)


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
