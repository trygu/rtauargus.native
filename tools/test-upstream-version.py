import importlib.util
from pathlib import Path
import tempfile
import json
import subprocess
import unittest

spec = importlib.util.spec_from_file_location("upstream_version", Path(__file__).with_name("upstream-version.py"))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class VersionTests(unittest.TestCase):
    def test_composite(self):
        self.assertEqual(module.compose("1.3.6", "0.6.1"), "1.3.6-0.6.1")
        with self.assertRaises(ValueError):
            module.compose("1.3.6", "0.6.1rc1")

    def test_field_update_preserves_other_fields(self):
        original = "Package: example\nVersion: 1.3.7\nDescription: First line.\n    Second line.\n"
        changed = module.set_field(original, "Version", "1.3.6-0.6.1")
        self.assertEqual(module.version(changed), "1.3.6-0.6.1")
        self.assertIn("Description: First line.\n    Second line.\n", changed)
        self.assertEqual(module.set_field(changed, "Version", "1.3.6-0.6.1"), changed)

    def test_pinned_source_is_verified_and_unimported_update_rejected(self):
        with tempfile.TemporaryDirectory() as work:
            root = Path(work)
            def git(*args):
                return subprocess.check_output(["git", "-C", str(root), *args], text=True).strip()
            git("init", "-q", "-b", "native")
            git("config", "user.name", "Trygve Tatsuya Falch")
            git("config", "user.email", "trygve.falch@gmail.com")
            git("remote", "add", "upstream", "https://github.com/InseeFrLab/rtauargus.git")
            (root / "DESCRIPTION").write_text("Package: example\nVersion: 1.3.6\n")
            git("add", "DESCRIPTION"); git("commit", "-qm", "upstream fixture")
            upstream = git("rev-parse", "HEAD")
            git("update-ref", "refs/remotes/upstream/master", upstream)
            native = root / "tauargus-engine/bindings/r/tauargusengine"
            native.mkdir(parents=True)
            (native / "DESCRIPTION").write_text("Version: 0.6.1\n")
            (root / "DESCRIPTION").write_text("Package: example\nVersion: 1.3.7\n")
            git("add", "."); git("commit", "-qm", "native fixture")
            module.record(root, "upstream/master")
            self.assertEqual(module.refresh(root), "1.3.6-0.6.1")
            self.assertEqual(module.refresh(root, True), "1.3.6-0.6.1")
            good_lock = (root / "upstream.json").read_text()
            lock = json.loads(good_lock); lock["version"] = "9.9"
            (root / "upstream.json").write_text(json.dumps(lock))
            with self.assertRaises(ValueError): module.load_lock(root)
            (root / "upstream.json").write_text(good_lock)
            git("add", "DESCRIPTION"); git("commit", "-qm", "version fixture")
            git("checkout", "-qb", "new-upstream", upstream)
            (root / "DESCRIPTION").write_text("Version: 1.3.7\n")
            git("add", "DESCRIPTION"); git("commit", "-qm", "unimported upstream fixture")
            git("update-ref", "refs/remotes/upstream/next", git("rev-parse", "HEAD"))
            git("checkout", "-q", "native")
            with self.assertRaises(subprocess.CalledProcessError): module.record(root, "upstream/next")
            self.assertEqual((root / "upstream.json").read_text(), good_lock)

    def test_invalid_and_duplicate_version(self):
        for value in ["Version: latest\n", "Version: 1.3.6\nVersion: 1.3.7\n"]:
            with self.assertRaises(ValueError):
                module.version(value)


if __name__ == "__main__":
    unittest.main()
