# rtauargus.native: Agent Rules

## Branching

`master` is the only long-lived branch and the GitHub default. Never
commit directly to `master`: work on a short-lived feature branch
(`<type>/<short-topic>`), open a PR, merge it, delete the branch. No
long-lived staging branches. Release-train commits (the engine pin
bump, `chore(release)`) use the same flow.

## Commits

Conventional Commits: lowercase type, optional scope, imperative
subject. No `Co-authored-by` trailer.

## Versions

The release version lives in the `r-v*` tag. `DESCRIPTION` tracks the
INSEE upstream version and is not bumped by releases. The engine
binding version
(`tauargus-engine/bindings/r/tauargusengine/DESCRIPTION`) must match
the tag before it is cut (the release workflow gates on it).

## More rules

Build commands, verification, the release procedure and the
documentation style live in the tauargus-orchestration repo: `AGENTS.md`
and the `release` skill.
