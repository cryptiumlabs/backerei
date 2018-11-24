#!/usr/bin/env bash

set -euo pipefail

cd "${BASH_SOURCE[0]%/*}"

stack2nix=$(nix-build --no-out-link https://github.com/input-output-hk/stack2nix/archive/8efe2d410ac188b2539b8225401fc944d2ce4a6b.tar.gz)

$stack2nix/bin/stack2nix -o default.nix ./..
rm -f ../backerei.cabal
