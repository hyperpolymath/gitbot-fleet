// SPDX-License-Identifier: MPL-2.0
//! Lock-map inventory and reviewed-graph comparison, not source provenance.
use saphyr::{MappingOwned as Mapping, ScalarOwned, ScalarStyle, YamlLoader, YamlOwned as Value};
use saphyr_parser::Parser;
use std::{
    collections::{BTreeMap, BTreeSet},
    env, fs,
    path::{Path, PathBuf},
    process,
};
type Result<T> = std::result::Result<T, String>;
type Set = BTreeSet<String>;

fn mapping<'a>(value: &'a Value, context: &str) -> Result<&'a Mapping> {
    value
        .as_mapping()
        .ok_or_else(|| format!("{context}: expected mapping"))
}
fn list<'a>(value: &'a Value, context: &str) -> Result<&'a Vec<Value>> {
    value
        .as_sequence()
        .ok_or_else(|| format!("{context}: expected list"))
}
fn string<'a>(value: &'a Value, context: &str) -> Result<&'a str> {
    value
        .as_str()
        .ok_or_else(|| format!("{context}: expected string"))
}
fn get<'a>(map: &'a Mapping, key: &str) -> Option<&'a Value> {
    map.get(&Value::Value(ScalarOwned::String(key.into())))
}
fn read_yaml(path: &Path) -> Result<Value> {
    let data = fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))?;
    // Keep spelling/style until schema conversion: this preserves the old
    // SafeLoader boolean type checks without losing quoted ref strings.
    let mut loader = YamlLoader::<Value>::default();
    loader.early_parse(false);
    loader.allow_duplicate_keys(false);
    loader.alias_limit(100_000);
    let mut parser = Parser::new_from_str(&data);
    parser
        .load(&mut loader, true)
        .map_err(|e| format!("{}: {e}", path.display()))?;
    if let Some(error) = loader.error() {
        let error = error.to_string();
        return Err(if error.contains("duplicated key") {
            format!("{}: duplicate YAML key: {error}", path.display())
        } else {
            format!("{}: {error}", path.display())
        });
    }
    let mut docs = loader.into_documents();
    if docs.len() != 1 {
        return Err("expected exactly one YAML document".into());
    }
    resolve_yaml(docs.pop().unwrap())
}
fn resolve_yaml(value: Value) -> Result<Value> {
    match value {
        Value::Representation(raw, style, tag) => {
            // Explicit tags and merge keys are outside this audit's supported
            // schema. Refuse them instead of auditing a different document.
            if tag.is_some() {
                return Err("YAML tags are unsupported".into());
            }
            if style == ScalarStyle::Plain {
                let bytes = raw.as_bytes();
                let timestamp = bytes.len() >= 10
                    && bytes[4] == b'-'
                    && bytes[7] == b'-'
                    && bytes[..4]
                        .iter()
                        .chain(bytes[5..7].iter())
                        .chain(bytes[8..10].iter())
                        .all(u8::is_ascii_digit);
                let sexagesimal = raw.contains(':')
                    && raw
                        .chars()
                        .all(|c| c.is_ascii_digit() || matches!(c, ':' | '+' | '-' | '.' | '_'));
                if timestamp || sexagesimal {
                    return Err("ambiguous YAML 1.1 typed scalar; quote strings explicitly".into());
                }
            }
            let scalar = if style == ScalarStyle::Plain {
                match raw.as_str() {
                    "yes" | "Yes" | "YES" | "on" | "On" | "ON" => ScalarOwned::Boolean(true),
                    "no" | "No" | "NO" | "off" | "Off" | "OFF" => ScalarOwned::Boolean(false),
                    _ => ScalarOwned::parse_from_cow_and_metadata(raw.clone().into(), style, None)
                        .ok_or("invalid YAML scalar")?,
                }
            } else {
                ScalarOwned::String(raw.clone())
            };
            // The YAML 1.2 parser bounds integers to i64; the old loader did
            // not. Never turn an overflowing bare number into an accepted ref.
            if style == ScalarStyle::Plain
                && matches!(scalar, ScalarOwned::String(_))
                && raw.parse::<f64>().is_ok()
            {
                return Err(
                    "out-of-range plain numeric YAML scalar; quote strings explicitly".into(),
                );
            }
            Ok(Value::Value(scalar))
        }
        Value::Mapping(map) => {
            let mut resolved = Mapping::new();
            for (key, value) in map {
                let key = resolve_yaml(key)?;
                if matches!(key, Value::Mapping(_) | Value::Sequence(_)) {
                    return Err("complex YAML mapping keys are unsupported".into());
                }
                if key.as_str() == Some("<<") {
                    return Err("YAML merge keys are unsupported".into());
                }
                let value = resolve_yaml(value)?;
                // Differently quoted spellings and YAML 1.1 boolean aliases
                // must not bypass duplicate detection during normalization.
                if resolved.insert(key, value).is_some() {
                    return Err("duplicate YAML key after scalar resolution".into());
                }
            }
            Ok(Value::Mapping(resolved))
        }
        Value::Sequence(items) => Ok(Value::Sequence(
            items.into_iter().map(resolve_yaml).collect::<Result<_>>()?,
        )),
        Value::Tagged(_, _) => Err("YAML tags are unsupported".into()),
        Value::Alias(_) | Value::BadValue => Err("unresolved or invalid YAML node".into()),
        Value::Value(_) => Ok(value),
    }
}
fn reference(raw: &str, dependency: bool) -> Result<Option<String>> {
    if raw.is_empty() || raw.chars().any(char::is_whitespace) {
        return Err(format!("invalid uses: {raw:?}"));
    }
    if !dependency && (raw.starts_with("./") || raw.starts_with("docker://")) {
        return Ok(None);
    }
    let (repo, pin) = raw
        .split_once('@')
        .ok_or_else(|| format!("invalid dependency reference: {raw:?}"))?;
    let parts: Vec<_> = repo.split('/').collect();
    if pin.is_empty()
        || pin.contains('@')
        || parts.len() < 2
        || parts.iter().any(|p| p.is_empty())
        || (dependency && parts.len() != 2)
    {
        return Err(format!("invalid dependency reference: {raw:?}"));
    }
    Ok(Some(format!(
        "{}/{}@{}",
        parts[0].to_lowercase(),
        parts[1].to_lowercase(),
        pin
    )))
}
fn key(value: &Value, strict: bool) -> Result<String> {
    reference(string(value, "dependency reference")?, strict)?
        .ok_or_else(|| "invalid dependency reference: local/docker reference".into())
}
fn sha(pin: &str) -> bool {
    pin.len() == 40 && pin.bytes().all(|b| b.is_ascii_hexdigit())
}
fn lock_parts(value: &Value) -> Result<(&Mapping, &Mapping)> {
    let doc = mapping(value, "lock")?;
    let w = get(doc, "workflows").and_then(Value::as_mapping);
    let d = get(doc, "dependencies").and_then(Value::as_mapping);
    w.zip(d)
        .ok_or_else(|| "expected workflows and dependencies mappings".into())
}
#[derive(PartialEq)]
struct Record {
    reference: String,
    commit: String,
    edges: Set,
}
fn load_lock(path: &Path) -> Result<(BTreeMap<String, Record>, Set)> {
    let value = read_yaml(path)?;
    let (workflows, dependencies) = lock_parts(&value)?;
    let mut deps = BTreeMap::new();
    for (name, record) in dependencies {
        let k = key(name, true)?;
        let rec = record
            .as_mapping()
            .ok_or_else(|| format!("duplicate or invalid record {k}"))?;
        if deps.contains_key(&k) {
            return Err(format!("duplicate or invalid record {k}"));
        }
        let reference = get(rec, "ref")
            .and_then(Value::as_str)
            .ok_or_else(|| format!("invalid ref for {k}"))?;
        if reference.is_empty()
            || reference.chars().any(char::is_whitespace)
            || reference.contains('@')
            || reference.starts_with("sha1-")
        {
            return Err(format!("invalid ref for {k}"));
        }
        let commit = get(rec, "commit")
            .and_then(Value::as_str)
            .ok_or_else(|| format!("invalid commit for {k}"))?;
        if !commit
            .strip_prefix("sha1-")
            .is_some_and(|s| sha(s) && s.bytes().all(|b| !b.is_ascii_uppercase()))
        {
            return Err(format!("invalid commit for {k}"));
        }
        let named = k.rsplit_once('@').unwrap().1;
        if sha(named) && named.bytes().all(|b| !b.is_ascii_uppercase()) {
            if commit != format!("sha1-{named}") {
                return Err(format!("SHA key and commit differ for {k}"));
            }
        } else if named != reference {
            return Err(format!("tag ref differs from key for {k}"));
        }
        let mut edges = Set::new();
        if let Some(uses) = get(rec, "uses") {
            for edge in list(uses, &format!("invalid uses for {k}"))? {
                edges.insert(key(edge, true)?);
            }
        }
        deps.insert(
            k,
            Record {
                reference: reference.into(),
                commit: commit.into(),
                edges,
            },
        );
    }
    let mut roots = Set::new();
    for (workflow, edges) in workflows {
        string(workflow, "invalid workflow")?;
        for edge in list(edges, "invalid workflow")? {
            roots.insert(key(edge, true)?);
        }
    }
    for edge in roots
        .iter()
        .chain(deps.values().flat_map(|r| r.edges.iter()))
    {
        if !deps.contains_key(edge) {
            return Err(format!("missing dependency {edge}"));
        }
    }
    Ok((deps, roots))
}
fn regeneration(old_path: &Path, new_path: &Path) -> Result<()> {
    let (old, _) = load_lock(old_path)?;
    let (new, roots) = load_lock(new_path)?;
    let mut queue: Vec<_> = roots.into_iter().filter(|k| old.contains_key(k)).collect();
    let mut seen = Set::new();
    while let Some(k) = queue.pop() {
        if !seen.insert(k.clone()) {
            continue;
        }
        let reviewed = &old[&k];
        let proposed = new
            .get(&k)
            .ok_or_else(|| format!("dropped reviewed dependency {k}"))?;
        if reviewed.reference != proposed.reference || reviewed.commit != proposed.commit {
            return Err(format!("changed reviewed ref/commit {k}"));
        }
        let missing: Vec<_> = reviewed.edges.difference(&proposed.edges).collect();
        if !missing.is_empty() {
            return Err(format!("dropped reviewed edges from {k}: {missing:?}"));
        }
        queue.extend(reviewed.edges.iter().cloned());
    }
    println!("Preserved reviewed reachable closure ({} records); new sources require independent review.", seen.len());
    Ok(())
}
fn inventory(root: &Path, unmanaged: &Set) -> Result<(Vec<String>, Vec<String>)> {
    let root = fs::canonicalize(root).map_err(|e| e.to_string())?;
    let wfdir = root.join(".github/workflows");
    let canonical_wfdir = fs::canonicalize(&wfdir).map_err(|e| e.to_string())?;
    let lock = read_yaml(&wfdir.join("actions.lock"))?;
    let (workflows, deps) = lock_parts(&lock)?;
    let mut files = BTreeMap::new();
    for entry in fs::read_dir(&wfdir).map_err(|e| e.to_string())? {
        let path = entry.map_err(|e| e.to_string())?.path();
        if path.is_file()
            && matches!(
                path.extension().and_then(|s| s.to_str()),
                Some("yml" | "yaml")
            )
        {
            files.insert(
                format!(
                    ".github/workflows/{}",
                    path.file_name()
                        .and_then(|s| s.to_str())
                        .ok_or("non-UTF8 workflow name")?
                ),
                path,
            );
        }
    }
    let mut findings = Vec::new();
    let mut notes = Vec::new();
    for path in unmanaged {
        if !files.contains_key(path) {
            findings.push(format!("UNMANAGED-UNKNOWN {path}"));
        }
    }
    for name in workflows.keys() {
        let name = string(name, "workflow path")?;
        if !files.contains_key(name) {
            findings.push(format!("STALE-WORKFLOW {name}"));
        }
    }
    let mut depkeys = Set::new();
    for dep in deps.keys() {
        if !depkeys.insert(key(dep, false)?) {
            return Err("invalid or duplicate dependency key".into());
        }
    }
    for (dep, record) in deps {
        let dep = string(dep, "dependency key")?;
        let record = mapping(record, "invalid dependency record")?;
        if let Some(edges) = get(record, "uses") {
            for edge in list(edges, "dependency uses")? {
                let normalized = key(edge, false)?;
                if !depkeys.contains(&normalized) {
                    findings.push(format!("MISSING-TRANSITIVE-RECORD {dep} {normalized}"));
                }
            }
        }
    }
    for (name, file) in &files {
        let doc = read_yaml(file)?;
        let doc = mapping(&doc, "workflow")?;
        let jobs = get(doc, "jobs")
            .and_then(Value::as_mapping)
            .ok_or_else(|| format!("{name}: expected jobs mapping"))?;
        let mut refs = Set::new();
        let mut remote_calls = Set::new();
        let mut local_calls = Vec::new();
        let mut unsupported = Vec::new();
        for job in jobs.values() {
            let job = mapping(job, "invalid job")?;
            let mut entries = Vec::new();
            if let Some(uses) = get(job, "uses") {
                entries.push((uses, true));
            }
            if let Some(steps) = get(job, "steps") {
                for step in list(steps, "invalid steps")? {
                    let step = mapping(step, "invalid step")?;
                    if let Some(uses) = get(step, "uses") {
                        entries.push((uses, false));
                    }
                }
            }
            for (raw, is_job) in entries {
                let raw = string(raw, "uses")?;
                match reference(raw, false)? {
                    Some(normalized) if is_job => {
                        if !sha(normalized.rsplit_once('@').unwrap().1) {
                            findings.push(format!("REMOTE-REUSABLE-NOT-SHA {name} {normalized}"));
                        }
                        notes.push(format!("REMOTE-REUSABLE-SOURCE-REVIEW {name} {normalized}"));
                        notes.push(format!("UNSUPPORTED-LOCK-SCOPE {name} {normalized} (external reusable dependencies not locked)"));
                        remote_calls.insert(normalized);
                    }
                    Some(normalized) => {
                        refs.insert(normalized);
                    }
                    None if raw.starts_with("./") && is_job => {
                        let local = fs::canonicalize(root.join(raw));
                        match local {
                            Ok(local)
                                if local.starts_with(&canonical_wfdir)
                                    && local.is_file()
                                    && matches!(
                                        local.extension().and_then(|s| s.to_str()),
                                        Some("yml" | "yaml")
                                    ) =>
                            {
                                local_calls.push(
                                    local
                                        .strip_prefix(&root)
                                        .map_err(|e| e.to_string())?
                                        .to_string_lossy()
                                        .into_owned(),
                                );
                            }
                            _ => findings.push(format!("INVALID-LOCAL-WORKFLOW {name} {raw}")),
                        }
                    }
                    None => unsupported.push(raw.to_owned()),
                }
            }
        }
        for raw in &unsupported {
            notes.push(format!("NOT-LOCKED-SOURCE-REVIEW {name} {raw}"));
        }
        let Some(mapped) = get(workflows, name) else {
            if unmanaged.contains(name) {
                notes.push(format!(
                    "INTENTIONALLY-UNMANAGED {name} (dependency closure not verified)"
                ));
            } else {
                findings.push(format!("MISSING-WORKFLOW {name}"));
            }
            continue;
        };
        if unmanaged.contains(name) {
            findings.push(format!("UNMANAGED-BUT-MAPPED {name}"));
        }
        let mut mapped_set = Set::new();
        for item in list(mapped, "workflow lock list")? {
            if !mapped_set.insert(key(item, false)?) {
                return Err(format!("{name}: invalid or duplicate lock reference"));
            }
        }
        for r in refs.difference(&mapped_set) {
            findings.push(format!("MISSING-DIRECT-EDGE {name} {r}"));
        }
        for r in mapped_set.difference(&depkeys) {
            findings.push(format!("MISSING-RECORD {name} {r}"));
        }
        if refs.is_empty()
            && remote_calls.is_empty()
            && local_calls.is_empty()
            && unsupported.is_empty()
            && mapped_set.is_empty()
        {
            notes.push(format!("NO-USES-MAPPED {name}"));
        }
        for target in local_calls {
            notes.push(format!(
                "LOCAL-REUSABLE {name} -> {target} (callee checked separately)"
            ));
        }
        for r in mapped_set.difference(&refs) {
            notes.push(format!(
                "EXTRA-OR-TRANSITIVE-EDGE {name} {r} (source review needed)"
            ));
        }
    }
    Ok((findings, notes))
}
fn main() {
    let args: Vec<_> = env::args().skip(1).collect();
    let code = match args.first().map(String::as_str) {
        Some("regeneration") if args.len() == 3 => {
            match regeneration(Path::new(&args[1]), Path::new(&args[2])) {
                Ok(()) => 0,
                Err(e) => {
                    eprintln!("lock regeneration audit: {e}");
                    1
                }
            }
        }
        Some("regeneration") => {
            eprintln!("usage: audit-lock-regeneration.sh REVIEWED_LOCK PROPOSED_LOCK");
            1
        }
        Some("map") => {
            let mut root = PathBuf::from(".");
            let mut has_root = false;
            let mut unmanaged = Set::new();
            let mut rest = args.iter().skip(1);
            let mut invalid = false;
            while let Some(arg) = rest.next() {
                if arg == "--unmanaged" {
                    if let Some(path) = rest.next() {
                        unmanaged.insert(path.clone());
                    } else {
                        invalid = true;
                    }
                } else if arg.starts_with('-') || has_root {
                    invalid = true;
                } else {
                    root = PathBuf::from(arg);
                    has_root = true;
                }
            }
            if invalid {
                eprintln!("usage: audit-workflow-lock-map.sh [ROOT] [--unmanaged WORKFLOW]");
                process::exit(2);
            }
            match inventory(&root, &unmanaged) {
                Err(e) => {
                    eprintln!("NOT-RUN: {e}");
                    2
                }
                Ok((findings, notes)) => {
                    for note in notes {
                        println!("NOTE: {note}");
                    }
                    for finding in &findings {
                        println!("FINDING: {finding}");
                    }
                    println!("workflow-map findings={}; remote source closure and provenance NOT VERIFIED", findings.len());
                    if !findings.is_empty() {
                        1
                    } else if !unmanaged.is_empty() {
                        2
                    } else {
                        0
                    }
                }
            }
        }
        _ => {
            eprintln!("usage: fleet-workflow-lock-audit map|regeneration ...");
            2
        }
    };
    process::exit(code);
}
