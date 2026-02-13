# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2024-2025 hyperpolymath
{
  description = "Oikos Bot: Ecological & Economic Code Analysis Platform";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowUnfree = true;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell
            ghc
            cabal-install
            haskell-language-server

            # OCaml
            ocaml
            dune_3
            opam
            ocamlPackages.merlin
            ocamlPackages.yojson
            ocamlPackages.ppx_deriving
            ocamlPackages.re
            ocamlPackages.cmdliner

            # ReScript + Deno
            deno
            nodejs_20

            # Python
            (python311.withPackages (ps: with ps; [
              numpy torch networkx pyyaml aiohttp pytest
            ]))

            # Rust
            (rust-bin.stable.latest.default.override {
              extensions = [ "rust-src" "rust-analyzer" ];
            })

            # Tools
            git gnumake jq ripgrep fd
            podman buildah
            souffle swiProlog
          ];

          shellHook = ''
            echo "🏛️ Oikos Bot Dev Shell"
            export DENO_DIR="$PWD/.deno"
          '';
        };
      }
    );
}
