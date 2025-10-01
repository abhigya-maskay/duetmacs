{
  description = "duetmacs development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    inherit (nixpkgs) lib;
    systems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    forEachSystem = f: lib.genAttrs systems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        compiler = pkgs.haskell.compiler.ghc9122;
        hspkgs = pkgs.haskell.packages.ghc9122;
      in f pkgs compiler hspkgs);
  in {
    devShells = forEachSystem (pkgs: compiler: hspkgs: {
      default = pkgs.mkShell {
        packages = [
          compiler
          hspkgs.cabal-install
          hspkgs.haskell-language-server
          hspkgs.hlint
          hspkgs.ormolu
          pkgs.time
        ];
      };
    });
  };
}
