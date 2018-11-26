{ pkgs ? import <nixpkgs> {}
}:

((pkgs.extend (self: super: { util = null; })).callPackage ./nix {}).backerei
