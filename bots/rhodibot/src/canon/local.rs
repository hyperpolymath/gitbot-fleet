// SPDX-License-Identifier: MPL-2.0

//! Reading a repository that is already on disk.
//!
//! The same checks run against a local checkout as against the API, which is
//! useful for three things: a repository too large for GitHub to list in one
//! response, a private repository with no token to hand, and checking before
//! pushing rather than after.
//!
//! What "the repository's files" means here is the **tracked** set, taken from
//! `git ls-files` when the directory is a work tree. A walk of the working
//! directory would count build output and editor scratch files, and one of
//! those can easily satisfy a criterion by accident -- a `CODE_OF_CONDUCT.md`
//! inside `target/` is not the repository's code of conduct. Where there is no
//! git to ask, the walk is the fallback, and it skips `.git` because those are
//! not files anybody commits.

use std::path::Path;
use std::process::Command;

use anyhow::{Context, Result, ensure};

/// Where a **legacy** `rsr-profile.a2ml` may still be, in preference order.
///
/// A read fallback only, consulted when the chora deed has no
/// `(rsr-profile ...)` clause. Never a thing to create or ask for.
const PROFILE_PATHS: [&str; 2] = [
    ".machine_readable/rsr-profile.a2ml",
    "machine-readable/rsr-profile.a2ml",
];

/// A local checkout, read.
#[derive(Debug)]
pub struct LocalSource {
    /// The repository's tracked files, relative to its root.
    pub files: Vec<String>,
    /// The text of a legacy profile file, when it has one (a read fallback).
    pub profile: Option<String>,
    /// The text of the root `*_chora.deed`, when it has one.
    pub chora_deed: Option<String>,
    /// Whether the file list came from git or from a walk.
    from_git: bool,
}

impl LocalSource {
    /// Whether the file list came from git rather than a walk.
    ///
    /// Worth reporting: a walked list includes untracked files, so a criterion
    /// can be satisfied by something the repository does not actually carry.
    pub fn from_git(&self) -> bool {
        self.from_git
    }

    /// Kept private so a `LocalSource` can only come from `read`, which is the
    /// only thing that knows whether the list is the tracked set.
    fn new(
        files: Vec<String>,
        profile: Option<String>,
        chora_deed: Option<String>,
        from_git: bool,
    ) -> Self {
        Self {
            files,
            profile,
            chora_deed,
            from_git,
        }
    }
}

/// Read a checkout: its files, and its profile if it has one.
pub fn read(root: &Path) -> Result<LocalSource> {
    ensure!(
        root.is_dir(),
        "{} is not a directory; --path expects a checkout",
        root.display()
    );

    let git_files = tracked_files(root);
    let from_git = git_files.is_some();
    let files = match git_files {
        Some(files) => files,
        None => {
            let mut files = Vec::new();
            walk(root, root, &mut files).with_context(|| format!("walking {}", root.display()))?;
            files.sort();
            files
        }
    };

    let chora_deed = match super::overlay::find_chora_deed(&files)? {
        Some(name) => {
            let path = root.join(&name);
            Some(
                std::fs::read_to_string(&path)
                    .with_context(|| format!("reading {}", path.display()))?,
            )
        }
        None => None,
    };

    Ok(LocalSource::new(
        files,
        profile(root)?,
        chora_deed,
        from_git,
    ))
}

/// The repository's tracked files, when git can be asked.
fn tracked_files(root: &Path) -> Option<Vec<String>> {
    let output = Command::new("git")
        .arg("-C")
        .arg(root)
        .arg("ls-files")
        .arg("-z")
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    // NUL-separated and raw: without `-z`, git C-quotes an unusual name (a
    // newline, a quote, non-ASCII), and a quoted `"x.a2ml"` would slip past a
    // suffix test. A name that is not UTF-8 is kept, lossily, rather than lost.
    let mut files: Vec<String> = output
        .stdout
        .split(|b| *b == 0)
        .filter(|name| !name.is_empty())
        .map(|name| String::from_utf8_lossy(name).into_owned())
        .collect();
    files.sort();
    files.dedup();
    Some(files)
}

/// Every file under `directory`, as a path relative to `root`.
fn walk(root: &Path, directory: &Path, files: &mut Vec<String>) -> Result<()> {
    for entry in
        std::fs::read_dir(directory).with_context(|| format!("reading {}", directory.display()))?
    {
        let entry = entry?;
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();

        if path.is_dir() {
            if name == ".git" {
                continue;
            }
            walk(root, &path, files)?;
            continue;
        }

        let relative = path
            .strip_prefix(root)
            .with_context(|| format!("{} is not under {}", path.display(), root.display()))?;
        files.push(relative.to_string_lossy().replace('\\', "/"));
    }
    Ok(())
}

/// The repository's profile text, if it has one.
fn profile(root: &Path) -> Result<Option<String>> {
    for candidate in PROFILE_PATHS {
        let path = root.join(candidate);
        match std::fs::read_to_string(&path) {
            Ok(text) => return Ok(Some(text)),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => {
                // Present but unreadable is not absent: reading it as "no
                // profile" would silently shrink the check to the universal
                // criteria and report a cleaner scorecard than the truth.
                return Err(error).with_context(|| format!("reading {}", path.display()));
            }
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A scratch directory with a unique name, removed on drop.
    struct Scratch(std::path::PathBuf);

    impl Scratch {
        fn new(label: &str) -> Self {
            let path =
                std::env::temp_dir().join(format!("rhodibot-local-{}-{label}", std::process::id()));
            let _ = std::fs::remove_dir_all(&path);
            std::fs::create_dir_all(&path).expect("the scratch directory is creatable");
            Self(path)
        }

        fn write(&self, relative: &str, contents: &str) -> &Self {
            let path = self.0.join(relative);
            std::fs::create_dir_all(path.parent().expect("has a parent"))
                .expect("parent directories are creatable");
            std::fs::write(&path, contents).expect("the file is writable");
            self
        }

        fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn a_walk_skips_dot_git_and_finds_the_rest() {
        let scratch = Scratch::new("walk");
        scratch.write("README.adoc", "hi");
        scratch.write(".machine_readable/descriptiles/STATE.a2ml", "state");
        // A file inside .git is not a file the repository carries.
        scratch.write(".git/objects/deadbeef", "not yours");

        let files = file_list_for(&scratch);
        assert_eq!(
            files,
            vec![
                ".machine_readable/descriptiles/STATE.a2ml".to_string(),
                "README.adoc".to_string()
            ]
        );
    }

    #[test]
    fn git_listing_keeps_unusual_names_raw() {
        // Without `ls-files -z`, git C-quotes a name with a newline in it and
        // the quoted form no longer ends in `.a2ml`.
        let scratch = Scratch::new("ls-files-z");
        scratch.write("evil\nname.a2ml", "x");
        scratch.write("plain.adoc", "x");
        let git = |args: &[&str]| {
            let ok = Command::new("git")
                .arg("-C")
                .arg(scratch.path())
                .args(args)
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false);
            assert!(ok, "git {args:?}");
        };
        git(&["init", "-q"]);
        git(&["add", "-A"]);
        let files = tracked_files(scratch.path()).expect("git can list the index");
        assert!(files.contains(&"evil\nname.a2ml".to_string()), "{files:?}");
        assert!(files.contains(&"plain.adoc".to_string()), "{files:?}");
    }

    #[test]
    fn a_profile_is_read_from_either_allowed_path() {
        let estate = Scratch::new("profile-estate");
        estate.write(
            ".machine_readable/rsr-profile.a2ml",
            "[rsr-profile]\ncapabilities = [\"bash\"]\n",
        );
        let source = read(estate.path()).expect("reads");
        assert!(
            source
                .profile
                .as_deref()
                .expect("a profile")
                .contains("capabilities")
        );

        let alternate = Scratch::new("profile-alt");
        alternate.write(
            "machine-readable/rsr-profile.a2ml",
            "[rsr-profile]\ncapabilities = [\"bash\"]\n",
        );
        assert!(read(alternate.path()).expect("reads").profile.is_some());
    }

    #[test]
    fn no_profile_is_none_rather_than_an_error() {
        let scratch = Scratch::new("no-profile");
        scratch.write("README.adoc", "hi");
        let source = read(scratch.path()).expect("reads");
        assert_eq!(source.profile, None, "a repository may declare nothing");
    }

    #[test]
    fn a_path_that_is_not_a_directory_is_refused() {
        let error = read(Path::new("/nonexistent-rhodibot-check")).expect_err("not a directory");
        assert!(
            format!("{error:#}").contains("not a directory"),
            "{error:#}"
        );
    }

    /// The walk's own answer, bypassing git: the scratch directory is not a
    /// work tree, so this is what `read` produces, but asserting on it directly
    /// keeps the test meaningful on a machine where /tmp is inside a repository.
    fn file_list_for(scratch: &Scratch) -> Vec<String> {
        let mut files = Vec::new();
        walk(scratch.path(), scratch.path(), &mut files).expect("the walk succeeds");
        files.sort();
        files
    }
}
