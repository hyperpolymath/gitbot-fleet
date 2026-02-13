;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;;
;;; Oikos Bot Guix Package Definition
;;;
;;; This defines the oikos-bot package for Guix.

(define-module (oikos)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages logic))

;; Haskell Code Analyzer
(define-public oikos-analyzer-haskell
  (package
    (name "oikos-analyzer-haskell")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hyperpolymath/oikos-bot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-aeson
           ghc-text
           ghc-containers
           ghc-vector
           ghc-mtl
           ghc-optparse-applicative
           ghc-megaparsec))
    (arguments
     '(#:cabal-file "analyzers/code-haskell/oikos-analyzer.cabal"))
    (synopsis "Haskell code analyzer for Oikos Bot")
    (description
     "Analyzes code for carbon intensity, energy efficiency,
      Pareto optimality, and software quality metrics.")
    (home-page "https://github.com/hyperpolymath/oikos-bot")
    (license license:agpl3+)))

;; OCaml Documentation Analyzer
(define-public oikos-analyzer-ocaml
  (package
    (name "oikos-analyzer-ocaml")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hyperpolymath/oikos-bot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system dune-build-system)
    (inputs
     (list ocaml-yojson
           ocaml-ppx-deriving
           ocaml-re
           ocaml-omd
           ocaml-cmdliner))
    (arguments
     '(#:source-subdir "analyzers/docs-ocaml"))
    (synopsis "OCaml documentation analyzer for Oikos Bot")
    (description
     "Analyzes documentation for completeness, consistency,
      and alignment with ecological/economic principles.")
    (home-page "https://github.com/hyperpolymath/oikos-bot")
    (license license:agpl3+)))

;; Python Policy Engine
(define-public oikos-policy-engine
  (package
    (name "oikos-policy-engine")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hyperpolymath/oikos-bot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system python-build-system)
    (inputs
     (list python-numpy
           python-torch
           souffle
           swi-prolog))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "policy-engine/python"))))))
    (synopsis "Policy engine for Oikos Bot")
    (description
     "Hybrid Datalog + DeepProbLog policy engine for
      deterministic and probabilistic reasoning.")
    (home-page "https://github.com/hyperpolymath/oikos-bot")
    (license license:agpl3+)))

;; Combined oikos-bot package
(define-public oikos-bot
  (package
    (name "oikos-bot")
    (version "0.1.0")
    (source #f)
    (build-system gnu-build-system)
    (inputs
     (list oikos-analyzer-haskell
           oikos-analyzer-ocaml
           oikos-policy-engine
           deno
           arangodb
           virtuoso-ose))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Create wrapper scripts
               (mkdir-p (string-append out "/bin"))
               #t))))))
    (synopsis "Ecological & Economic Code Analysis Platform")
    (description
     "Oikos Bot analyzes code for ecological soundness and economic
      efficiency using Pareto optimality and allocative efficiency
      as normative criteria.")
    (home-page "https://github.com/hyperpolymath/oikos-bot")
    (license license:agpl3+)))
