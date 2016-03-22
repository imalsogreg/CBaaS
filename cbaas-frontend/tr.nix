{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});

  # lifted from http://github.com/reflex-frp/reflex-platform
  cabal2nixResult2 = src: nixpkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
    # Support unicode characters in cabal files
    ${if !nixpkgs.stdenv.isDarwin then "LOCALE_ARCHIVE" else null} = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    ${if !nixpkgs.stdenv.isDarwin then "LC_ALL" else null} = "en_US.UTF-8";
  };

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     # reflex-dom-contrib  = (self.callPackage ../deps/reflex-dom-contrib { compilername = "ghcjs"; });
     reflex-dom-contrib  = (self.callPackage (reflex-platform.cabal2nixResult ../deps/reflex-dom-contrib) {});
     tagging-common      = (self.callPackage ../tagging-common/default.nix { compilername = "ghcjs"; });
     servant             = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant) {});
     servant-docs        = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-docs) {});
     yaml-ghcjs          = (self.callPackage (reflex-platform.cabal2nixResult ../deps/yaml-ghcjs) {});
     groundhog-th        = (self.callPackage ../deps/groundhog/groundhog-th { compilername = "ghcjs"; });
     groundhog-postgresql = null;
     servant-js = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-js) {});
     servant-foreign = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-foreign) {});
     servant-server = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-server) {});
     servant-blaze = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-blaze) {});
     servant-matlab = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-matlab) {});

     snap = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap) {});
     heist = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/heist) {});
     xmlhtml = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/xmlhtml) {});
     io-streams = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/io-streams) {});
     io-streams-haproxy = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});
     snap-core = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/snap-core) {});
     snap-server = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap/deps/snap-server) {});
     snap-loader-static = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap-loader-static) {});
     snap-loader-dynamic = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/snap-loader-dynamic) {});
     cbaas-lib            = (self.callPackage (reflex-platform.cabal2nixResult ../cbaas-lib) {});
  };
}
