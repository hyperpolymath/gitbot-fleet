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

/// The two paths the canon's baseline allows for a profile, in preference
/// order: both are acceptable, and the first is the estate's.
const PROFILE_PATHS: [&str; 2] = [
    ".machine_readable/rsr-profile.a2ml",
    "machine-readable/rsr-profile.a2ml",
];

/// A local checkout, read.
#[derive(Debug)]
pub struct LocalSource {
    /// The repository's tracked files, relative to its root.
    pub files: Vec<String>,
    /// The text of its profile, when it has one.
    pub profile: Option<String>,
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
    fn new(files: Vec<String>, profile: Option<String>, from_git: bool) -> Self {
        Self {
            files,
            profile,
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

    Ok(LocalSource::new(files, profile(root)?, from_git))
}

/// The repository's tracked files, when git can be asked.
fn tracked_files(root: &Path) -> Option<Vec<String>> {
    let output = Command::new("git")
        .arg("-C")
        .arg(root)
        .arg("ls-files")
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let listing = String::from_utf8(output.stdout).ok()?;
    let mut files: Vec<String> = listing
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(str::to_string)
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
