## Bäckerei

Tooling for the Cryptium Tezos Bäckerei

### Usage

#### Initialization

Initialize Bäckerei with the [Cryptium Labs tz1 address](https://tzscan.io/tz1eEnQhbwf6trb8Q8mPb2RaPkNk2rN7BKi8):

```bash
backerei init --tz1 tz1eEnQhbwf6trb8Q8mPb2RaPkNk2rN7BKi8 --from tz1eEnQhbwf6trb8Q8mPb2RaPkNk2rN7BKi8
```


#### Executing payouts

Bäckerei is stateful and will calculate outstanding payouts automatically, just run:

```bash
backerei payout
```

By default, that command will execute a dry run and only display what payouts need to be sent.

In order to actually send transactions, run:

```bash
backerei payout --no-dry-run
```

#### Verifying payouts

You can find previous payout information [here](https://github.com/cryptiumlabs/library/tree/master/validation-records/tezos).

```bash
backerei payout --verify
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

#### Debugging

To run an interactive REPL with Bäckerei scoped:

```bash
make repl
```

#### Cleaning up

To remove all build files:

```bash
make clean
```

#### Installing

`~/.local/bin` will need to be on your `$PATH`.

```bash
make install
```
