## Bäckerei

Bäckerei is tooling that we wrote for the Cryptium Tezos Bäckerei. At a high
level it manages the payments from us, the baker, to our delegators. Bäckerei
is initialised with a TZ1 address which is used for baking. When run, it connects to
a full-node and scans the entire transaction history to determine who the
delegators are and how much they should get paid. Note that this full-node must
be trusted.

You can check out all the configuration options in `$HOME/.backerei.yaml`.
You can set your fee, the RPC URL of your full node, etc.

When run, Bäckerei reads and updates a simple JSON database file. You can see
ours [here](https://github.com/cryptiumlabs/library/blob/master/validation-records/tezos/db.json).
All your delegators can use that file to check whether they have been paid
correctly, and on top of it you can build front-ends on it like 
[this one](https://tezos.cryptium.ch/dashboard).

Bäckerei also allows you to specify a different payout address. This means that you
can pay from a KT1 account on a separate server instead of having to pay
directly from your Nano Ledger S.

Note that Bäckerei calculates *idealized* payouts, not *realized* payouts. On-chain
and off-chain delegators will be paid according to what your baker would have made if
you had successfully baked and endorsed all scheduled blocks and endorsement slots (plus
realized fees). This is roughly equivalent to insuring liveness.

### Usage

#### Initialization

Initialize Bäckerei with the tz1 address of your baker, for example that of 
[Cryptium Labs](https://tzscan.io/tz1eEnQhbwf6trb8Q8mPb2RaPkNk2rN7BKi8),
the address you want to send payouts from, the alias associated with that address,
the path to your database file, and the first cycle in which you baked or endorsed blocks:

```bash
backerei init \
  --tz1 tz1eEnQhbwf6trb8Q8mPb2RaPkNk2rN7BKi8 \
  --from tz1eEnQhbwf6trb8Q8mPb2RaPkNk2rN7BKi8 \
  --from-name payout \
  --client-config-file $HOME/.tezos-client/config \
  --starting-cycle 11
```

More options can be passed if desired. Run `backerei init --help` for a full 
list. You can also edit the config file directly.

#### Executing payouts

Bäckerei is stateful and will calculate outstanding payouts automatically. 
Simply run:

```bash
backerei payout
```

By default, that command will execute a dry run and only display what payouts 
need to be sent.

In order to actually send transactions, run:

```bash
backerei payout --no-dry-run
```

In order to run continuously (so that payouts will be sent as soon as new cycle 
starts):

```bash
backerei payout --no-dry-run --continuous
```

Bäckerei is fairly well tested, but be careful! Ensure you trust the Tezos node 
to which you are connecting. In any case, you would be well-advised to pay from 
an isolated account with the minimal requisite balance.

#### Verifying payouts

You can find historical payout logs for Cryptium Labs 
[here](https://github.com/cryptiumlabs/library/tree/master/validation-records/tezos)
and cross-check against a payout database generated locally.

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
