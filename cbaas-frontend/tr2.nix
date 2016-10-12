{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
  };
}
