# gitbot-fleet report-backs

One JSON envelope per enrolled fleet member, `<member>.json`, in the shape
`docs/VERDICT-GRID-SPEC.adoc` of metadatastician/standards defines and
`docs/REPORT-BACK.adoc` in this repository explains.

**This directory is empty in git on purpose, and stays so.** Envelopes are
produced per run by `scripts/fleet-report-back.sh` (through
`.github/workflows/fleet-report-back-reusable.yml`), uploaded as the
`fleet-report-backs` artifact, and placed here by the Hypatia lane only at
render time. A committed envelope would be stale evidence about an older
commit, so none is ever committed. An enrolled member with no envelope
renders **orange** ("enrolled, but no report-back was received"); only a real
run may change that.

Only `*.json` files are read, so this README is inert.
