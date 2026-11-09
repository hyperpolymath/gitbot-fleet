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
            let scalar = resolve_scalar(raw, style)?;
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
fn resolve_scalar(raw: String, style: ScalarStyle) -> Result<ScalarOwned> {
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
    // not. Never turn a bare number (including separator spellings) into an accepted ref.
    if style == ScalarStyle::Plain
        && matches!(scalar, ScalarOwned::String(_))
        && is_legacy_number(&raw)
    {
        return Err("numeric YAML scalar cannot be a string; quote strings explicitly".into());
    }
    Ok(scalar)
}
/// Recognize numeric spellings that a bounded YAML 1.2 scalar parser may
/// leave as strings. YAML 1.1 allows underscore separators and arbitrary
/// precision integers; neither may silently become a valid string ref.
fn is_legacy_number(raw: &str) -> bool {
    let unsigned = raw.strip_prefix(['+', '-']).unwrap_or(raw);
    // A nonempty underscore-only radix tail matched the old numeric resolver
    // but failed conversion. Keep it an error, rather than accepting a ref.
    let radix_tail = |tail: &str, radix: u32| {
        !tail.is_empty() && tail.chars().all(|c| c == '_' || c.is_digit(radix))
    };
    if let Some(binary) = unsigned.strip_prefix("0b") {
        return radix_tail(binary, 2);
    }
    if let Some(hexadecimal) = unsigned.strip_prefix("0x") {
        return radix_tail(hexadecimal, 16);
    }
    let compact = unsigned.replace('_', "");
    let digits = |text: &str| !text.is_empty() && text.chars().all(|c| c.is_ascii_digit());
    // The unseparated form also covers octal integers; only the scalar's
    // non-string classification matters here, not its numeric value.
    digits(&compact) || compact.parse::<f64>().is_ok()
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
#[derive(Default)]
struct InventoryReport {
    findings: Vec<String>,
    notes: Vec<String>,
}
#[derive(Default)]
struct WorkflowUses {
    refs: Set,
    remote_calls: Set,
    local_calls: Vec<String>,
    unsupported: Vec<String>,
}
impl WorkflowUses {
    fn is_empty(&self) -> bool {
        self.refs.is_empty()
            && self.remote_calls.is_empty()
            && self.local_calls.is_empty()
            && self.unsupported.is_empty()
    }
}
fn workflow_files(wfdir: &Path) -> Result<BTreeMap<String, PathBuf>> {
    let mut files = BTreeMap::new();
    for entry in fs::read_dir(wfdir).map_err(|e| e.to_string())? {
        let path = entry.map_err(|e| e.to_string())?.path();
        if path.is_file() && is_workflow_path(&path) {
            let filename = path
                .file_name()
                .and_then(|s| s.to_str())
                .ok_or("non-UTF8 workflow name")?;
            files.insert(format!(".github/workflows/{filename}"), path);
        }
    }
    Ok(files)
}
fn is_workflow_path(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|s| s.to_str()),
        Some("yml" | "yaml")
    )
}
fn dependency_keys(deps: &Mapping, report: &mut InventoryReport) -> Result<Set> {
    let mut keys = Set::new();
    for dep in deps.keys() {
        if !keys.insert(key(dep, false)?) {
            return Err("invalid or duplicate dependency key".into());
        }
    }
    for (dep, record) in deps {
        let dep = string(dep, "dependency key")?;
        let record = mapping(record, "invalid dependency record")?;
        if let Some(edges) = get(record, "uses") {
            for edge in list(edges, "dependency uses")? {
                let normalized = key(edge, false)?;
                if !keys.contains(&normalized) {
                    report
                        .findings
                        .push(format!("MISSING-TRANSITIVE-RECORD {dep} {normalized}"));
                }
            }
        }
    }
    Ok(keys)
}
fn job_entries(job: &Value) -> Result<Vec<(&Value, bool)>> {
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
    Ok(entries)
}
struct InventoryContext<'a> {
    root: &'a Path,
    wfdir: &'a Path,
    workflows: &'a Mapping,
    depkeys: Set,
    unmanaged: &'a Set,
}
impl InventoryContext<'_> {
    fn local_target(&self, raw: &str) -> Result<Option<String>> {
        let Ok(local) = fs::canonicalize(self.root.join(raw)) else {
            return Ok(None);
        };
        if !local.starts_with(self.wfdir) || !local.is_file() || !is_workflow_path(&local) {
            return Ok(None);
        }
        Ok(Some(
            local
                .strip_prefix(self.root)
                .map_err(|e| e.to_string())?
                .to_string_lossy()
                .into_owned(),
        ))
    }
    fn inspect_uses(
        &self,
        name: &str,
        jobs: &Mapping,
        report: &mut InventoryReport,
    ) -> Result<WorkflowUses> {
        let mut uses = WorkflowUses::default();
        for job in jobs.values() {
            for (raw, is_job) in job_entries(job)? {
                self.inspect_entry(name, string(raw, "uses")?, is_job, &mut uses, report)?;
            }
        }
        for raw in &uses.unsupported {
            report
                .notes
                .push(format!("NOT-LOCKED-SOURCE-REVIEW {name} {raw}"));
        }
        Ok(uses)
    }
    fn inspect_entry(
        &self,
        name: &str,
        raw: &str,
        is_job: bool,
        uses: &mut WorkflowUses,
        report: &mut InventoryReport,
    ) -> Result<()> {
        match reference(raw, false)? {
            Some(normalized) if is_job => {
                if !sha(normalized.rsplit_once('@').unwrap().1) {
                    report
                        .findings
                        .push(format!("REMOTE-REUSABLE-NOT-SHA {name} {normalized}"));
                }
                report
                    .notes
                    .push(format!("REMOTE-REUSABLE-SOURCE-REVIEW {name} {normalized}"));
                report.notes.push(format!("UNSUPPORTED-LOCK-SCOPE {name} {normalized} (external reusable dependencies not locked)"));
                uses.remote_calls.insert(normalized);
            }
            Some(normalized) => {
                uses.refs.insert(normalized);
            }
            None if raw.starts_with("./") && is_job => match self.local_target(raw)? {
                Some(local) => uses.local_calls.push(local),
                None => report
                    .findings
                    .push(format!("INVALID-LOCAL-WORKFLOW {name} {raw}")),
            },
            None => uses.unsupported.push(raw.to_owned()),
        }
        Ok(())
    }
    fn compare_map(
        &self,
        name: &str,
        uses: WorkflowUses,
        report: &mut InventoryReport,
    ) -> Result<()> {
        let Some(mapped) = get(self.workflows, name) else {
            if self.unmanaged.contains(name) {
                report.notes.push(format!(
                    "INTENTIONALLY-UNMANAGED {name} (dependency closure not verified)"
                ));
            } else {
                report.findings.push(format!("MISSING-WORKFLOW {name}"));
            }
            return Ok(());
        };
        if self.unmanaged.contains(name) {
            report.findings.push(format!("UNMANAGED-BUT-MAPPED {name}"));
        }
        let mapped_set = mapped_references(name, mapped)?;
        for r in uses.refs.difference(&mapped_set) {
            report
                .findings
                .push(format!("MISSING-DIRECT-EDGE {name} {r}"));
        }
        for r in mapped_set.difference(&self.depkeys) {
            report.findings.push(format!("MISSING-RECORD {name} {r}"));
        }
        if uses.is_empty() && mapped_set.is_empty() {
            report.notes.push(format!("NO-USES-MAPPED {name}"));
        }
        for target in uses.local_calls {
            report.notes.push(format!(
                "LOCAL-REUSABLE {name} -> {target} (callee checked separately)"
            ));
        }
        for r in mapped_set.difference(&uses.refs) {
            report.notes.push(format!(
                "EXTRA-OR-TRANSITIVE-EDGE {name} {r} (source review needed)"
            ));
        }
        Ok(())
    }
    fn inspect_file(&self, name: &str, file: &Path, report: &mut InventoryReport) -> Result<()> {
        let doc = read_yaml(file)?;
        let doc = mapping(&doc, "workflow")?;
        let jobs = get(doc, "jobs")
            .and_then(Value::as_mapping)
            .ok_or_else(|| format!("{name}: expected jobs mapping"))?;
        let uses = self.inspect_uses(name, jobs, report)?;
        self.compare_map(name, uses, report)
    }
}
fn mapped_references(name: &str, mapped: &Value) -> Result<Set> {
    let mut refs = Set::new();
    for item in list(mapped, "workflow lock list")? {
        if !refs.insert(key(item, false)?) {
            return Err(format!("{name}: invalid or duplicate lock reference"));
        }
    }
    Ok(refs)
}
fn inventory(root: &Path, unmanaged: &Set) -> Result<(Vec<String>, Vec<String>)> {
    let root = fs::canonicalize(root).map_err(|e| e.to_string())?;
    let wfdir = root.join(".github/workflows");
    let canonical_wfdir = fs::canonicalize(&wfdir).map_err(|e| e.to_string())?;
    let lock = read_yaml(&wfdir.join("actions.lock"))?;
    let (workflows, deps) = lock_parts(&lock)?;
    let files = workflow_files(&wfdir)?;
    let mut report = InventoryReport::default();
    for path in unmanaged {
        if !files.contains_key(path) {
            report.findings.push(format!("UNMANAGED-UNKNOWN {path}"));
        }
    }
    for name in workflows.keys() {
        let name = string(name, "workflow path")?;
        if !files.contains_key(name) {
            report.findings.push(format!("STALE-WORKFLOW {name}"));
        }
    }
    let depkeys = dependency_keys(deps, &mut report)?;
    let context = InventoryContext {
        root: &root,
        wfdir: &canonical_wfdir,
        workflows,
        depkeys,
        unmanaged,
    };
    for (name, file) in &files {
        context.inspect_file(name, file, &mut report)?;
    }
    Ok((report.findings, report.notes))
}
fn parse_map_arguments(args: &[String]) -> Result<(PathBuf, Set)> {
    let mut root = PathBuf::from(".");
    let mut has_root = false;
    let mut unmanaged = Set::new();
    let mut rest = args.iter();
    while let Some(arg) = rest.next() {
        if arg == "--unmanaged" {
            unmanaged.insert(rest.next().ok_or("missing unmanaged workflow")?.clone());
        } else if arg.starts_with('-') || has_root {
            return Err("unexpected map argument".into());
        } else {
            root = PathBuf::from(arg);
            has_root = true;
        }
    }
    Ok((root, unmanaged))
}
fn map_command(args: &[String]) -> i32 {
    let Ok((root, unmanaged)) = parse_map_arguments(args) else {
        eprintln!("usage: audit-workflow-lock-map.sh [ROOT] [--unmanaged WORKFLOW]");
        return 2;
    };
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
            println!(
                "workflow-map findings={}; remote source closure and provenance NOT VERIFIED",
                findings.len()
            );
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
        Some("map") => map_command(&args[1..]),
        _ => {
            eprintln!("usage: fleet-workflow-lock-audit map|regeneration ...");
            2
        }
    };
    process::exit(code);
}
