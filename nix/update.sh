#!/usr/bin/env bash

set -euo pipefail

cd "${BASH_SOURCE[0]%/*}"

stack2nix=$(nix-build --no-out-link https://github.com/input-output-hk/stack2nix/archive/8b92e5d5861e609d723d56c2425d6571a2289d75.tar.gz)

$stack2nix/bin/stack2nix -o default.nix ./..
rm -f ../backerei.cabal
