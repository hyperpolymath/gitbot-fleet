;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;;
;;; Oikos Bot Guix Manifest
;;;
;;; Development environment manifest for oikos-bot.
;;; Use with: guix shell -m manifest.scm

(specifications->manifest
 '(;; ========================================
   ;; Core Languages
   ;; ========================================

   ;; Haskell (for code analyzer)
   "ghc"
   "cabal-install"
   "hlint"
   "haskell-language-server"

   ;; OCaml (for documentation analyzer)
   "ocaml"
   "dune"
   "opam"
   "ocaml-merlin"
   "ocaml-ocp-indent"
   "ocamlformat"

   ;; ReScript (compiles from source, needs node for build)
   "node"  ; Only for rescript compiler, not runtime

   ;; Deno runtime
   "deno"

   ;; Python (for policy engine)
   "python"
   "python-pip"
   "python-virtualenv"

   ;; Rust (for orchestrator)
   "rust"
   "rust-analyzer"

   ;; ========================================
   ;; Databases
   ;; ========================================

   ;; ArangoDB client tools
   "arangodb"

   ;; Virtuoso (SPARQL)
   "virtuoso-ose"

   ;; ========================================
   ;; Logic Programming
   ;; ========================================

   ;; Datalog (Souffle)
   "souffle"

   ;; Prolog (for DeepProbLog base)
   "swi-prolog"

   ;; ========================================
   ;; Build Tools
   ;; ========================================

   "git"
   "make"
   "gcc-toolchain"
   "pkg-config"
   "openssl"
   "zlib"

   ;; ========================================
   ;; Container Tools (Vörðr preferred)
   ;; ========================================

   "podman"
   "buildah"
   "skopeo"
   "cni-plugins"

   ;; ========================================
   ;; Development Utilities
   ;; ========================================

   "jq"
   "ripgrep"
   "fd"
   "bat"
   "direnv"
   "watchexec"))
