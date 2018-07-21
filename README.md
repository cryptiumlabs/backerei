## Bäckerei

Tooling for the Cryptium Tezos Bäckerei

### Usage

Coming soon!

#### Verifying Cryptium payouts

Initialize Bäckerei with the [Cryptium Labs tz1 address](https://tzscan.io/tz1eEnQhbwf6trb8Q8mPb2RaPkNk2rN7BKi8):

```bash
backerei init --tz1 tz1eEnQhbwf6trb8Q8mPb2RaPkNk2rN7BKi8
```

### Development

[Stack](https://haskellstack.org) required.

#### Building

```bash
make build
```

#### Testing

```bash
make test
```

#### Linting

First, install [Hlint](https://hackage.haskell.org/package/hlint):

```bash
stack install hlint
```

Then run:

```bash
make lint
```

#### Installing

`~/.local/bin` will need to be on your `$PATH`.

```bash
make install
```
