// SPDX-License-Identifier: MPL-2.0
//! Parser and classification rules. Ported from the original lib.test.js,
//! plus regressions for bugs found in that implementation (P1–P6).

use idris_dependents::catalogue::{classify, is_distinctive, search_stems, Confidence};
use idris_dependents::ipkg::{depends, lex, package_name, Tok};
use idris_dependents::refs::{gitmodules_refs, pack_toml_refs, points_at};

fn fx(name: &str) -> String {
    std::fs::read_to_string(format!(
        "{}/tests/fixtures/{name}",
        env!("CARGO_MANIFEST_DIR")
    ))
    .unwrap()
}
fn own() -> Vec<String> {
    vec!["hyperpolymath".into(), "metadatastician".into()]
}
fn s(v: &[&str]) -> Vec<String> {
    v.iter().map(|x| x.to_string()).collect()
}

// --- parsers on real estate files -----------------------------------------------

#[test]
fn real_ochrance_ipkg_name_and_multiline_depends() {
    let t = fx("ochrance.ipkg");
    assert_eq!(package_name(&t).as_deref(), Some("ochrance"));
    assert_eq!(depends(&t), s(&["base", "contrib", "network"]));
}

#[test]
fn real_proven_ipkg_version_bound_stripped() {
    let t = fx("proven.ipkg");
    assert_eq!(package_name(&t).as_deref(), Some("proven"));
    assert_eq!(depends(&t), s(&["base", "contrib", "network"]));
}

#[test]
fn real_ordering_laws_ipkg_two_of_your_own_packages() {
    let t = fx("ordering-laws.ipkg");
    assert_eq!(package_name(&t).as_deref(), Some("proven-ordering-laws"));
    assert_eq!(depends(&t), s(&["proven-tests", "proven-safeordering"]));
}

#[test]
fn real_pack_toml_local_path_entry_is_not_a_github_ref() {
    assert_eq!(pack_toml_refs(&fx("pack.toml"), &own()).unwrap(), vec![]);
}

// --- edge cases -----------------------------------------------------------------

#[test]
fn depends_comments_leading_commas_bounds_trailing_fields() {
    let t = "package foo
-- depends = fake
{- depends = alsofake -}
depends = base >= 0.6.0 && < 0.8   -- inline comment
        , ochrance
        , proven-dns
modules = Foo
opts = \"--codegen chez\"";
    assert_eq!(depends(t), s(&["base", "ochrance", "proven-dns"]));
}

#[test]
fn no_depends_field() {
    assert!(depends("package x\nmodules = A").is_empty());
}

#[test]
fn depends_field_as_the_last_line() {
    assert_eq!(depends("package x\ndepends = trope"), s(&["trope"]));
}

#[test]
fn commented_out_entry_inside_multiline_depends_does_not_leak() {
    assert_eq!(
        depends("package foo\ndepends = base\n        -- , oldpkg\n        , ochrance"),
        s(&["base", "ochrance"])
    );
}

#[test]
fn whole_depends_line_commented_out() {
    assert!(depends("package proven-dns\n-- depends = proven\nmodules = DNS").is_empty());
}

#[test]
fn multiline_block_comment_inside_depends_list_is_removed() {
    let t = "package foo\ndepends = base,\n  {- proven-old,\n     legacy -}\n  ochrance";
    assert_eq!(depends(t), s(&["base", "ochrance"]));
}

#[test]
fn pack_toml_github_url_to_you_counts_others_do_not_quoted_keys_ok() {
    let t = r#"[custom.all.proven]
type = "github"
url = "https://github.com/hyperpolymath/proven"
commit = "abc"
[custom.all."lsp-lib"]
type = "github"
url = "https://github.com/idris-community/LSP-lib"
[custom.all.tropecheck]
url = "git@github.com:Hyperpolymath/trope-checker.git"  # ssh form, mixed case"#;
    let names: Vec<String> = pack_toml_refs(t, &own())
        .unwrap()
        .into_iter()
        .map(|r| r.name)
        .collect();
    assert_eq!(names, s(&["proven", "tropecheck"]));
}

#[test]
fn gitmodules_refs_basic() {
    let t = "[submodule \"x\"]\n\tpath = vendor/proven\n\turl = https://github.com/hyperpolymath/proven.git\n[submodule \"y\"]\n\turl = https://github.com/someone/else";
    assert_eq!(
        gitmodules_refs(t, &own()),
        s(&["https://github.com/hyperpolymath/proven.git"])
    );
}

#[test]
fn owner_match_is_exact_not_a_prefix() {
    assert!(gitmodules_refs("url = https://github.com/hyperpolymathx/proven", &own()).is_empty());
}

#[test]
fn distinctive_filter_drops_internal_test_bench_packages() {
    for n in [
        "abi",
        "benchmarks",
        "coreonly",
        "test-ffi",
        "proven-tests",
        "proven-spec-suite",
        "proven-tests-bench",
    ] {
        assert!(!is_distinctive(n), "{n}");
    }
    for n in [
        "proven",
        "proven-dns",
        "ochrance",
        "typedwasm",
        "phi-lam",
        "nimiser-abi",
    ] {
        assert!(is_distinctive(n), "{n}");
    }
}

#[test]
fn stems_collapse_families_keep_short_stem_names_whole() {
    assert_eq!(
        search_stems(&s(&[
            "proven",
            "proven-dns",
            "proven-ffi",
            "phi-lam",
            "ochrance",
            "test-ffi"
        ])),
        s(&["ochrance", "phi-lam", "proven"])
    );
}

#[test]
fn classification_ladder() {
    assert_eq!(classify(&[], 1, 0), Some(Confidence::High));
    assert_eq!(
        classify(&s(&["proven-dns"]), 0, 0),
        Some(Confidence::Medium)
    );
    assert_eq!(classify(&s(&["proven"]), 0, 0), Some(Confidence::Low));
    assert_eq!(classify(&[], 0, 0), None);
}

// --- regressions for bugs in the JS implementation ----------------------------

#[test]
fn p1_crlf_line_endings() {
    assert_eq!(
        depends("package x\r\ndepends = base, ochrance\r\nmodules = A\r\n"),
        s(&["base", "ochrance"])
    );
    assert_eq!(package_name("package x\r\n").as_deref(), Some("x"));
}

#[test]
fn p2_eq_bound_on_continuation_line() {
    let t = "package x\ndepends = base,\n  ochrance == 0.1.0,\n  proven-dns\nmodules = A";
    assert_eq!(depends(t), s(&["base", "ochrance", "proven-dns"]));
}

#[test]
fn p3_block_opener_inside_line_comment_is_inert() {
    assert_eq!(
        depends("package x\n-- see {- note\ndepends = ochrance\n-- end -}\nmodules = A"),
        s(&["ochrance"])
    );
}

#[test]
fn p4_nested_block_comments() {
    assert_eq!(
        depends("package x\n{- outer {- inner -} depends = fake -}\ndepends = ochrance"),
        s(&["ochrance"])
    );
}

#[test]
fn p5_hash_inside_quoted_pack_toml_url() {
    assert_eq!(
        pack_toml_refs(
            "[custom.all.p]\nurl = \"https://github.com/hyperpolymath/p#readme\"",
            &own()
        )
        .unwrap()
        .len(),
        1
    );
}

#[test]
fn p6_single_quoted_pack_toml_url() {
    assert_eq!(
        pack_toml_refs(
            "[custom.all.p]\nurl = 'https://github.com/hyperpolymath/p'",
            &own()
        )
        .unwrap()
        .len(),
        1
    );
}

// --- Idris2 grammar details (Parser/Lexer/Common.idr on Idris2 main) ----------

#[test]
fn dash_dash_inside_block_comment_hides_close_on_same_line() {
    // `--` inside a block comment is a line comment, so this `-}` does not close it.
    let t = "package x\n{- a -- b -}\ndepends = fake\n-}\ndepends = ochrance";
    assert_eq!(depends(t), s(&["ochrance"]));
}

#[test]
fn close_delimiter_inside_string_inside_block_comment_is_inert() {
    let t = "package x\n{- \"-}\" depends = fake -}\ndepends = ochrance";
    assert_eq!(depends(t), s(&["ochrance"]));
}

#[test]
fn dashes_in_strings_are_not_comments() {
    let t = "package x\nopts = \"--total -- depends = fake\"\ndepends = ochrance";
    assert_eq!(depends(t), s(&["ochrance"]));
}

#[test]
fn trailing_comma_does_not_swallow_next_field_key() {
    assert_eq!(
        depends("package x\ndepends = base,\nmodules = A"),
        s(&["base"])
    );
}

#[test]
fn several_depends_fields_and_duplicates() {
    assert_eq!(
        depends("package x\ndepends = a, b\nmodules = M\ndepends = b, c"),
        s(&["a", "b", "c"])
    );
}

#[test]
fn module_names_are_not_package_names() {
    assert_eq!(
        depends("package x\ndepends = Proven.SafeMath, ochrance"),
        Vec::<String>::new()
    );
}

#[test]
fn package_needs_a_name_not_a_field() {
    assert_eq!(package_name("package = 3"), None);
}

#[test]
fn host_must_be_github_exactly() {
    let o = own();
    assert!(points_at("https://github.com/hyperpolymath/proven", &o));
    assert!(points_at("ssh://git@github.com/hyperpolymath/proven", &o));
    assert!(points_at("git@github.com:metadatastician/x.git", &o));
    assert!(points_at("https://www.github.com/hyperpolymath/proven", &o));
    assert!(!points_at("https://notgithub.com/hyperpolymath/proven", &o));
    assert!(!points_at("https://github.com/hyperpolymath", &o));
}

#[test]
fn gitmodules_quoted_and_crlf() {
    let t = "[submodule \"x\"]\r\n\turl = \"https://github.com/hyperpolymath/proven\"\r\n";
    assert_eq!(
        gitmodules_refs(t, &own()),
        s(&["https://github.com/hyperpolymath/proven"])
    );
}

#[test]
fn lexer_distinguishes_field_assignment_from_every_version_operator() {
    assert_eq!(
        lex("= == < <= > >= && , . 012"),
        (
            vec![
                Tok::Equals,
                Tok::EqOp,
                Tok::Lt,
                Tok::Lte,
                Tok::Gt,
                Tok::Gte,
                Tok::AndOp,
                Tok::Sep,
                Tok::Dot,
                Tok::Int("012".into()),
            ],
            0,
        )
    );
}

#[test]
fn lexer_preserves_identifiers_strings_and_counts_only_unrecognised_characters() {
    let (tokens, skipped) = lex("\0@$ _pkg' café-lib Data.Module \"a\\\"--{-b\"");
    assert_eq!(skipped, 3);
    assert_eq!(
        tokens,
        vec![
            Tok::Ident {
                text: "_pkg'".into(),
                dotted: false
            },
            Tok::Ident {
                text: "café-lib".into(),
                dotted: false
            },
            Tok::Ident {
                text: "Data.Module".into(),
                dotted: true
            },
            Tok::Str("a\\\"--{-b".into()),
        ]
    );
}

#[test]
fn fake_declarations_in_comments_strings_and_dotted_names_are_ignored() {
    let text = r#"-- package fake
{- package hidden -}
opts = "package quoted depends = bogus \" depends = escaped"
package Wrong.Module
package real
depends = café-lib, _ffi', café-lib
"#;
    assert_eq!(package_name(text).as_deref(), Some("real"));
    assert_eq!(depends(text), s(&["café-lib", "_ffi'"]));
}

#[test]
fn depends_requires_an_exact_field_key_and_single_equals() {
    for text in [
        "mydepends = ochrance",
        "Depends = ochrance",
        "Project.depends = ochrance",
        "depends == ochrance",
        "depends >= ochrance",
        "depends",
        "depends =",
        "depends = 123",
    ] {
        assert!(depends(text).is_empty(), "{text}");
    }
}

#[test]
fn all_version_bounds_preserve_dependency_order_and_stop_at_fields() {
    assert_eq!(
        depends("depends = a < 2, b <= 2.1, c > 0, d >= 1.0, e == 3, f >= 1 && < 2,\nmodules = M\ndepends = e, g"),
        s(&["a", "b", "c", "d", "e", "f", "g"])
    );
}

#[test]
fn unterminated_block_comment_preserves_only_preceding_dependencies() {
    let text = "package real\ndepends = ochrance\n{- unfinished {- nested -}\ndepends = fake";
    assert_eq!(package_name(text).as_deref(), Some("real"));
    assert_eq!(depends(text), s(&["ochrance"]));
    assert!(depends("{- depends = fake").is_empty());
}

#[test]
fn empty_and_comment_only_files_have_no_package_or_dependencies() {
    for text in ["", " \t\r\n", "-- package fake\r\n", "{- depends = fake -}"] {
        assert_eq!(package_name(text), None, "{text}");
        assert!(depends(text).is_empty(), "{text}");
    }
}

#[test]
fn internal_package_filter_is_case_insensitive_and_respects_affix_boundaries() {
    for name in [
        "ABI",
        "Benchmark",
        "BENCHMARKS",
        "Test",
        "TESTS",
        "CoreOnly",
        "FFI-Test",
        "Integration-Tests",
        "Property-Tests",
        "TEST-helper",
        "Pkg-TEST",
        "Pkg-TESTS",
        "Pkg-BENCH",
        "Pkg-SUITE",
    ] {
        assert!(!is_distinctive(name), "{name}");
    }
    for name in [
        "test-",
        "testing",
        "contest",
        "benchmarker",
        "suite",
        "abi-tools",
    ] {
        assert!(is_distinctive(name), "{name}");
    }
}

#[test]
fn search_stem_boundary_counts_characters_and_returns_sorted_unique_names() {
    assert_eq!(
        search_stems(&s(&[
            "ééééé-core",
            "éééé-core",
            "abcde-two",
            "abcd-two",
            "abcde-one",
            "abcde",
            "abcde-tests",
            "TEST-helper",
            "abcd-two",
        ])),
        s(&["abcd-two", "abcde", "éééé-core", "ééééé"])
    );
    assert!(search_stems(&s(&["test", "abi", "pkg-bench"])).is_empty());
}

#[test]
fn classification_accepts_submodules_and_promotes_mixed_evidence() {
    for (deps, pack, sub, expected) in [
        (s(&[]), 0, 1, Some(Confidence::High)),
        (s(&["proven", "trope"]), 0, 0, Some(Confidence::Low)),
        (s(&["proven", "ochrance"]), 0, 0, Some(Confidence::Medium)),
        (s(&["trope"]), 1, 0, Some(Confidence::High)),
        (s(&["ochrance"]), 0, 1, Some(Confidence::High)),
        (s(&[]), 0, 0, None),
    ] {
        assert_eq!(
            classify(&deps, pack, sub),
            expected,
            "{deps:?}, {pack}, {sub}"
        );
    }
}

#[test]
fn github_references_require_owner_and_host_boundaries() {
    for url in [
        "HTTPS://GITHUB.COM/HyperPolymath/repo.git",
        "git@github.com:METADATASTICIAN/repo.git",
        "ssh://git@github.com/hyperpolymath/repo.git",
    ] {
        assert!(points_at(url, &own()), "{url}");
        assert!(!points_at(url, &[]), "empty owner list: {url}");
    }
    for url in [
        "https://github.com/hyperpolymath-extra/repo",
        "https://github.com/not-hyperpolymath/repo",
        "https://github.com/hyperpolymath",
        "https://github.com.evil.test/hyperpolymath/repo",
        "https://github.com@evil.test/hyperpolymath/repo",
        "https://notgithub.com/hyperpolymath/repo",
        "../hyperpolymath/repo",
        "",
    ] {
        assert!(!points_at(url, &own()), "{url}");
    }
}

#[test]
fn pack_refs_keep_collection_and_entry_order_and_skip_irrelevant_shapes() {
    let text = r#"
[package]
url = "https://github.com/hyperpolymath/not-custom"
[custom.z]
scalar = 3
wrong-type = { url = 42 }
local = { path = "." }
external = { url = "https://github.com/someone/else" }
z-last-alphabetically = { url = 'https://github.com/hyperpolymath/first' }
a-first-alphabetically = { url = "git@github.com:metadatastician/second.git" }
[custom.a.third]
url = "https://github.com/hyperpolymath/third#readme"
"#;
    let refs = pack_toml_refs(text, &own()).unwrap();
    assert_eq!(
        refs.iter()
            .map(|r| (r.name.as_str(), r.url.as_str()))
            .collect::<Vec<_>>(),
        vec![
            (
                "z-last-alphabetically",
                "https://github.com/hyperpolymath/first"
            ),
            (
                "a-first-alphabetically",
                "git@github.com:metadatastician/second.git"
            ),
            ("third", "https://github.com/hyperpolymath/third#readme"),
        ]
    );
    assert!(pack_toml_refs(text, &[]).unwrap().is_empty());
}

#[test]
fn pack_refs_reject_invalid_toml_and_tolerate_absent_custom_tables() {
    for text in ["[custom", "url = 'unterminated", "custom = 1\ncustom = 2"] {
        assert!(pack_toml_refs(text, &own()).is_err(), "{text}");
    }
    for text in [
        "",
        "custom = 1",
        "custom = []",
        "[custom]\ncollection = 7",
        "[package]\nname = 'x'",
    ] {
        assert!(pack_toml_refs(text, &own()).unwrap().is_empty(), "{text}");
    }
}

#[test]
fn gitmodules_url_keys_comments_and_quoted_fragments() {
    let text = r#"
# url = https://github.com/hyperpolymath/comment
; url = https://github.com/hyperpolymath/comment
url-extra = https://github.com/hyperpolymath/not-a-url-key
path = https://github.com/hyperpolymath/not-a-url
url = https://github.com/hyperpolymath/first.git ; trailing comment
url = https://github.com/metadatastician/second.git # trailing comment
url = "https://github.com/hyperpolymath/third#fragment;part" # comment
url = https://github.com/hyperpolymathx/not-ours
url = ""
"#;
    assert_eq!(
        gitmodules_refs(text, &own()),
        s(&[
            "https://github.com/hyperpolymath/first.git",
            "https://github.com/metadatastician/second.git",
            "https://github.com/hyperpolymath/third#fragment;part",
        ])
    );
}
