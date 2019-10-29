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

#### Fee format

The `--fee` parameter to `backerei init` requires the format "n % d" where `n` is
numerator and `d` is denominator.

For example:

```
# set up a 9.5% fee
backerei init --fee "19 % 200" <...>

# set up a 5% fee
backerei init --fee "5 % 100" <...>
```

#### Constants

Different networks such as alphanet may have different constants. You need to specify these constants during initialisation for Backerei to work correctly.

The default values for these constants work on the Tezos mainnet.

* `--cycle-length` defaults to 4096. For alphanet, set to 2048
* `--snapshot-interval` defaults to 512. For alphanet, set to 256
* `--preserved-cycles`: defaults to 5. For alphanet, set to 3

#### Estimated rewards vs final rewards

The payout calculator assumes 100% liveness of your own baking and endorsing node, and pays
accordingly. However, the effective rewards for a given baker also depends on the behaviour
of other bakers in the network. For example, if bakers fail to endorse your block, or fail
to include your endorsment in their block, then your rewards are lower.

Estimated rewards assume perfect behaviour of the entire set of active delegates for
a given block. Final rewards take into account the actual faulty behaviour of other nodes
(not yours) and are accordingly a few percent lower.

#### Delayed payouts

By default, the payouts are paid after `PRESERVED_CYCLES`, which corresponds to when
the Tezos network unfreezes them. You may choose to pay them earlier or later:

* `--payout-delay 2` will pay the rewards two cycles (six days) later
* `--payout-delay -1` will pay the rewards one cycle (three days) before you actually
get access to them

You can not set a payout delay of less than negative `PRESERVED_CYCLES` because final
rewards can not be computed before the cycle ends. However, you may set up a payout delay
of up to `2*PRESERVED_CYCLES + 1` if you use the `--pay-estimated-rewards` option.

This option will likely pay more than the final fee, so adjust your fee accordingly.

#### Verifying payouts

You can find historical payout logs for Cryptium Labs 
[here](https://github.com/cryptiumlabs/library/tree/master/validation-records/tezos)
and cross-check against a payout database generated locally.

### Development

[Stack](https://haskellstack.org) required for the build process. Once Bäckerei is installed, you can uninstall Stack.

#### Building

```bash
make build
```

#### Testing

```bash
make test
```

#### Linting

Not required unless you want to modify the code and contribute upstream.

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

### Licensing

GPLv3. See [LICENSE](./LICENSE) for full terms.
