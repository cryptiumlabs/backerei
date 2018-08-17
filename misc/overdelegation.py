#!/usr/bin/env python

import sys

if len(sys.argv) != 3:
  print('Usage: ./overdelegation.py [cycle] [staking_balance]')
  sys.exit(1)

cycle = int(sys.argv[1])
staking_balance = float(sys.argv[2])

total_supply = 763306930
fraction_staked = 0.473
blocks_per_cycle = 4096

deposit = 0

for c in range(cycle, cycle + 5):
  if c >= 64:
    adjustment = 1
  else:
    adjustment = c / 64
  baking_deposit = 512 * adjustment
  endorsing_deposit = 64 * adjustment
  endorsers = 32
  net_deposit = baking_deposit + (endorsing_deposit * endorsers)
  my_expected_deposit = net_deposit * blocks_per_cycle * staking_balance / (total_supply * fraction_staked)
  deposit += my_expected_deposit
  print('Expected deposit for cycle {}: {} XTZ'.format(c, my_expected_deposit))

print('Total expected deposit from cycle {} to cycle {}: {} XTZ'.format(cycle, cycle + 4, deposit))
