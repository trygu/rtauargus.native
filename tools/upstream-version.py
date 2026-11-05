#!/usr/bin/env python3
"""Record an imported INSEE revision and derive the native R package version."""
import argparse
import hashlib
import json
import re
import subprocess
from pathlib import Path

REPOSITORY = "InseeFrLab/rtauargus"
VERSION = re.compile(r"[0-9]+(?:[.-][0-9]+)+")


def git(root, *args):
    return subprocess.check_output(["git", "-C", str(root), *args], text=True, encoding="utf-8").strip()


def read_fields(text):
    fields = {}
    key = None
    for line in text.splitlines():
        if line and not line[0].isspace() and ":" in line:
            key, value = line.split(":", 1)
            if key in fields:
                raise ValueError("Duplicate DESCRIPTION field: " + key)
            fields[key] = value.strip()
        elif line.strip() and key:
            fields[key] += "\n" + line
    return fields


def version(text):
    value = read_fields(text).get("Version", "")
    if not VERSION.fullmatch(value):
        raise ValueError("DESCRIPTION must contain a numeric R version")
    return value


def compose(upstream, native):
    if not VERSION.fullmatch(upstream) or not VERSION.fullmatch(native):
        raise ValueError("Both version components must be numeric R versions")
    return upstream + "-" + native


def set_field(text, name, value):
    lines = text.splitlines()
    start = next((i for i, line in enumerate(lines) if line.startswith(name + ":")), None)
    replacement = [name + ": " + value]
    if start is None:
        return text.rstrip() + "\n" + replacement[0] + "\n"
    end = start + 1
    while end < len(lines) and lines[end] and lines[end][0].isspace():
        end += 1
    return "\n".join(lines[:start] + replacement + lines[end:]) + "\n"


def load_lock(root):
    lock = json.loads((root / "upstream.json").read_text(encoding="utf-8"))
    if lock.get("repository") != REPOSITORY:
        raise ValueError("Unexpected upstream repository")
    if not re.fullmatch(r"[0-9a-f]{40}", lock.get("revision", "")):
        raise ValueError("Upstream revision must be a full commit hash")
    source = subprocess.check_output(
        ["git", "-C", str(root), "show", lock["revision"] + ":DESCRIPTION"]
    )
    if hashlib.sha256(source).hexdigest() != lock.get("description_sha256"):
        raise ValueError("Recorded upstream DESCRIPTION hash differs")
    if version(source.decode()) != lock.get("version"):
        raise ValueError("Recorded upstream version differs from its source")
    subprocess.run(["git", "-C", str(root), "merge-base", "--is-ancestor",
                    lock["revision"], "HEAD"], check=True)
    return lock


def record(root, ref):
    if not ref.startswith("upstream/") or not re.fullmatch(r"upstream/[A-Za-z0-9_./-]+", ref):
        raise ValueError("Select a fetched upstream/... reference")
    url = git(root, "remote", "get-url", "upstream").lower()
    if url.endswith(".git"): url = url[:-4]
    if url not in {"https://github.com/inseefrlab/rtauargus",
                   "git@github.com:inseefrlab/rtauargus"}:
        raise ValueError("The upstream remote must identify INSEE's repository")
    revision = git(root, "rev-parse", "--verify", ref + "^{commit}")
    subprocess.run(["git", "-C", str(root), "merge-base", "--is-ancestor",
                    revision, "HEAD"], check=True)
    source = subprocess.check_output(["git", "-C", str(root), "show",
                                      revision + ":DESCRIPTION"])
    lock = dict(repository=REPOSITORY, revision=revision, version=version(source.decode()),
                description_sha256=hashlib.sha256(source).hexdigest())
    (root / "upstream.json").write_text(json.dumps(lock, indent=2) + "\n", encoding="utf-8")


def refresh(root, check=False):
    lock = load_lock(root)
    native = version((root / "tauargus-engine/bindings/r/tauargusengine/DESCRIPTION").read_text(encoding="utf-8"))
    original = (root / "DESCRIPTION").read_text(encoding="utf-8")
    updated = original
    for name, value in {
        "Version": compose(lock["version"], native),
        "Config/rtauargus/upstream-version": lock["version"],
        "Config/rtauargus/upstream-revision": lock["revision"],
        "Config/tauargus/native-version": native,
    }.items():
        updated = set_field(updated, name, value)
    if check:
        if original != updated:
            raise ValueError("Version metadata is stale; run tools/upstream-version.py")
    else:
        (root / "DESCRIPTION").write_text(updated, encoding="utf-8")
    return compose(lock["version"], native)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--record-upstream", metavar="UPSTREAM_REF")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    if args.record_upstream:
        if args.check:
            parser.error("--record-upstream and --check are separate operations")
        record(root, args.record_upstream)
    print(refresh(root, args.check))
