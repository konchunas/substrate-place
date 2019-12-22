# Substrate Place

This is an experiment in storing a huge pixel board on blockchain using Substrate runtime. Conceptually it is quite similar to Reddit r/place or Satoshi's place. Contrary to smart contracts languages, Substrate facilities are not as limited and able to keep and access vast amounts of data in an arbitrary manner. The idea here is to be able to buy a pixel on even farthest point of the map without any substantial burden on node processing power or storage cost. You might think of every pixel having unique address just like every user balance has an unique address. And since addresses are 256-bit long theoretical board size is promisingly huge.

This runtime module allows you to store a 2^35 x 2^35 pixel grid on a blockchain. You can purchase a pixel at any point on that grid. And anyone can re-buy it from you for a larger price that you have initially paid. Since this is a development version there is a faucet functionality for quicker testing and playing around.

## Storage organization

The whole grid is split in chunks: 8x8 matrices of pixels accessed by (i32, i32) coordinates.V
Chunk matrix is virtual and actually stored as 64-element vector.
When purchasing a pixel client makes a request passing absolute pixel coordinates.
But when downloading the state client is expected to request only chunks and only currently visible ones.
Initially there is no chunks on the map. But as user chooses pixel to buy corresponding chunk gets initialized.

## UI

For reference implementation of the web client refer to https://github.com/konchunas/substrate-place-ui .
It displays this module storage as pixel board and allows to navigate makes purchases.

You can check testnet demo with included faucet at http://konchunas.com:5000/

# Building

Install requirements.
For ubuntu you can use

`sudo apt install git gcc libssl-dev clang pkg-config`

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
cargo build --release
```

# Run

You can start a development chain with:

```bash
cargo run --release -- --dev
```