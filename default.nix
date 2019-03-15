{ pkgs ? import <nixpkgs> {}
}:

let
  stack-pkgs = pkgs.callPackage ./nix {};
  posix-pty = stack-pkgs.posix-pty.override (attrs: { util = null; });
  backerei = stack-pkgs.backerei.override (attrs: { posix-pty = posix-pty;});
in
  backerei
