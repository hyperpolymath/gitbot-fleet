// SPDX-License-Identifier: MPL-2.0
//! Parser and classification rules. Ported from the original lib.test.js,
//! plus regressions for bugs found in that implementation (P1–P6).

use idris_dependents::catalogue::{classify, is_distinctive, search_stems, Confidence};
use idris_dependents::ipkg::{depends, package_name};
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
