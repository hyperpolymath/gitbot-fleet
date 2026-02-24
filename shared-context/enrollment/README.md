# Repo Enrollment Registry

This directory stores generated enrollment state for gitbot-fleet and hypatia coverage.

## Generate/refresh

```bash
just enroll-repos
```

## Apply enrollment directives to repos

```bash
just enroll-repos /var/mnt/eclipse/repos true
```

This writes `.machine_readable/bot_directives/FLEET-ENROLLMENT.a2ml` into repos that already have `.machine_readable/`.

## Hard-pass maintenance gate

Use `just maintenance-hard-pass <repo-path>` to run release-gate maintenance with fail-on-warn behavior.
