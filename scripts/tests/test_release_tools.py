"""Regression checks for release gates; never connect to Trello."""
import importlib.util
import io
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from contextlib import redirect_stdout

SCRIPTS = Path(__file__).resolve().parents[1]


def load(name):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class PrivateTestReports(unittest.TestCase):
    def setUp(self):
        self.runner = load("run_ct")
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.allowed = {"demo_SUITE": {"works", "init_per_suite", "end_per_suite"}}

    def tearDown(self):
        self.temp.cleanup()

    def report(self, body, counts='tests="1" failures="0" errors="0" skipped="0"'):
        report = self.root / "junit_report.xml"
        report.write_text(f'<testsuites><testsuite name="demo_SUITE" {counts}>{body}</testsuite></testsuites>')
        return report

    def test_failure_details_never_reach_public_output(self):
        sentinel = "SECRET_CREDENTIAL_SENTINEL_938274"
        self.report(f'<testcase name="works"><failure message="{sentinel}">{sentinel}</failure></testcase>',
                    'tests="1" failures="1" errors="0" skipped="0"')
        output = io.StringIO()
        with redirect_stdout(output):
            self.assertFalse(self.runner.summarize(self.root, self.allowed))
        self.assertNotIn(sentinel, output.getvalue())
        self.assertIn("demo_SUITE.works: failed", output.getvalue())

    def test_unknown_names_do_not_reach_public_output(self):
        self.report('<testcase name="SECRET_CREDENTIAL_SENTINEL"/>')
        output = io.StringIO()
        with redirect_stdout(output):
            self.assertFalse(self.runner.summarize(self.root, self.allowed))
        self.assertNotIn("SECRET_CREDENTIAL_SENTINEL", output.getvalue())

    def test_skip_missing_suite_and_missing_report_fail_closed(self):
        with redirect_stdout(io.StringIO()):
            self.assertFalse(self.runner.summarize(self.root, self.allowed))
            self.report('<testcase name="works"><skipped/></testcase>',
                        'tests="1" failures="0" errors="0" skipped="1"')
            self.assertFalse(self.runner.summarize(self.root, self.allowed))
            self.report('<testcase name="works"/>')
            self.assertFalse(self.runner.summarize(self.root, {**self.allowed, "missing_SUITE": {"works"}}))

    def test_pass_and_failed_fixture(self):
        with redirect_stdout(io.StringIO()):
            self.report('<testcase name="init_per_suite"/><testcase name="works"/><testcase name="end_per_suite"/>')
            self.assertTrue(self.runner.summarize(self.root, self.allowed))
            self.report('<testcase name="works"/><testcase name="end_per_suite"><failure>private</failure></testcase>')
            self.assertFalse(self.runner.summarize(self.root, self.allowed))

    def test_ci_rejects_private_log_retention_before_running(self):
        result = subprocess.run(["python3", str(SCRIPTS / "run_ct.py"), "--keep-logs", str(self.root / "logs")],
                                env={**os.environ, "CI": "true"}, capture_output=True, text=True)
        self.assertNotEqual(0, result.returncode)
        self.assertFalse((self.root / "logs").exists())
        self.assertNotIn("Traceback", result.stderr)


class RealPrivateRunner(unittest.TestCase):
    @unittest.skipUnless(shutil.which("escript") and shutil.which("rebar3"), "requires Erlang and rebar3")
    def test_real_common_test_serialization_and_failure_cleanup(self):
        sentinel = 'SECRET_SENTINEL_"quoted"\\escaped\nnew-line'
        with tempfile.TemporaryDirectory(prefix="trellang-script-test-") as temporary:
            root = Path(temporary)
            for directory in ("scripts", "src", "test", "private"):
                (root / directory).mkdir()
            for name in ("run_ct.py", "private_ct_config.escript"):
                shutil.copy(SCRIPTS / name, root / "scripts" / name)
            (root / "rebar.config").write_text('{ct_opts, [{ct_hooks, [cth_surefire]}]}.\n')
            (root / "src/demo.app.src").write_text('{application,demo,[{description,"script regression fixture"},'
                '{vsn,"0.1.0"},{applications,[kernel,stdlib]},{modules,[]}]}.\n')
            source = ('-module(demo_SUITE).\n-export([all/0, works/1]).\nall() -> [works].\n'
                      'works(_) -> {ok,true}=application:get_env(trellang,test_cleanup), '
                      '{ok,Value}=application:get_env(trellang,trello_token), '
                      'true=lists:member(10,Value), true=lists:member(34,Value), '
                      'true=lists:member(92,Value), ok.\n')
            suite = root / "test/demo_SUITE.erl"
            suite.write_text(source)
            env = {**os.environ, "CI": "true", "TRELLO_KEY": "dummy-key", "TRELLO_TOKEN": sentinel,
                   "TRELLO_BOARD_ID": "dummy-board", "TRELLO_LIST_ID": "dummy-list", "TMPDIR": str(root / "private")}
            command = ["python3", str(root / "scripts/run_ct.py")]
            result = subprocess.run(command, env=env, capture_output=True, text=True, timeout=90)
            self.assertEqual(0, result.returncode, result.stdout + result.stderr)
            self.assertEqual([], list((root / "private").glob("trellang-ct-*")))
            suite.write_text(source.replace('ok.\n', 'ct:fail(Value).\n'))
            result = subprocess.run(command, env=env, capture_output=True, text=True, timeout=90)
            self.assertNotEqual(0, result.returncode)
            self.assertIn("demo_SUITE.works: failed", result.stdout)
            self.assertNotIn("SECRET_SENTINEL", result.stdout + result.stderr)
            self.assertEqual([], list((root / "private").glob("trellang-ct-*")))

    @unittest.skipUnless(shutil.which("escript"), "requires Erlang")
    def test_private_config_permissions_and_local_cleanup_override(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "local.config"
            target = root / "private.config"
            source.write_text('[{trellang,[{trello_key,"key"},{trello_token,"token"},'
                              '{board_id,"board"},{list_id,"list"},{test_cleanup,false}]}].\n')
            result = subprocess.run(["escript", str(SCRIPTS / "private_ct_config.escript"), str(target), str(source)],
                                    capture_output=True, text=True)
            self.assertEqual(0, result.returncode)
            self.assertEqual(0o600, target.stat().st_mode & 0o777)
            self.assertIn("{test_cleanup,true}", target.read_text())
            self.assertNotIn("token", result.stdout + result.stderr)


class ReleaseMetadata(unittest.TestCase):
    @unittest.skipUnless(shutil.which("escript"), "requires Erlang")
    def test_metadata_and_tag_fail_closed(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "scripts").mkdir()
            (root / "src").mkdir()
            shutil.copy(SCRIPTS / "check_release.escript", root / "scripts/check_release.escript")
            (root / "src/trellang.app.src").write_text('{application,trellang,[{vsn,"1.2.0"}]}.\n')
            changelog = root / "CHANGELOG.md"
            command = ["escript", str(root / "scripts/check_release.escript")]
            packaged_app = root / "packaged.app.src"
            packaged_app.write_text('{application, trellang, [\n  {vsn, "1.2.0"}\n]}.\n')
            self.assertEqual(0, subprocess.run(command + ["--app-source", str(packaged_app)], capture_output=True).returncode)
            packaged_app.write_text('{application, trellang, [{vsn, "1.1.0"}]}.\n')
            self.assertNotEqual(0, subprocess.run(command + ["--app-source", str(packaged_app)], capture_output=True).returncode)
            changelog.write_text("## v1.2.0 — Unreleased\n\n## v1.1.0 — 2025-09-18\n")
            self.assertEqual(0, subprocess.run(command, capture_output=True).returncode)
            self.assertNotEqual(0, subprocess.run(command + ["--tag", "v1.2.0"], capture_output=True).returncode)
            for invalid in ("## v1.1.0 — Unreleased\n", "## v1.2.0 — 2026-02-30\n",
                            "## v1.2.0\n", "## v1.2.0 — Unreleased\n## v1.2.0 — Unreleased\n"):
                changelog.write_text(invalid)
                self.assertNotEqual(0, subprocess.run(command, capture_output=True).returncode)
            changelog.write_text("## v1.2.0 — 2026-10-14\n")
            def git(*args):
                subprocess.run(["git", "-c", "user.name=Release Gate Test", "-c", "user.email=release-test@example.invalid",
                                *args], cwd=root, capture_output=True, check=True)
            git("init")
            git("add", ".")
            git("commit", "-m", "fixture")
            git("tag", "v1.2.0")
            self.assertNotEqual(0, subprocess.run(command + ["--tag", "v1.2.0"], capture_output=True).returncode)
            git("tag", "-d", "v1.2.0")
            git("tag", "-a", "v1.2.0", "-m", "annotated fixture")
            self.assertEqual(0, subprocess.run(command + ["--tag", "v1.2.0"], capture_output=True).returncode)
            git("commit", "--allow-empty", "-m", "wrong HEAD")
            self.assertNotEqual(0, subprocess.run(command + ["--tag", "v1.2.0"], capture_output=True).returncode)


class ArtifactRejection(unittest.TestCase):
    def test_exdoc_project_source_link_and_manifest(self):
        checker = load("check_artifacts")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for name in ("index.html", "readme.html", "changelog.html", "agents.html", "releases.html"):
                (root / name).write_text("documentation")
            (root / ".build").write_text("ExDoc manifest")
            (root / "api-reference.html").write_text('trellang v1.2.0 <a title="View Source" '
                'href="https://github.com/stritzinger/trellang">source</a>')
            for name in ("trello", "trello_board_server"):
                (root / f"{name}.html").write_text('trellang v1.2.0 <a title="View Source" '
                    f'href="https://github.com/stritzinger/trellang/blob/v1.2.0/src/{name}.erl">source</a>')
            checker.docs(root, "1.2.0")
            module = root / "trello.html"
            module.write_text(module.read_text().replace("blob/v1.2.0", "blob/v1.1.0"))
            with self.assertRaises(ValueError):
                checker.docs(root, "1.2.0")
            module.write_text('trellang v1.2.0 <a title="View Source" '
                              'href="https://github.com/stritzinger/trellang">source</a>')
            with self.assertRaises(ValueError):
                checker.docs(root, "1.2.0")

    def test_source_links_are_distinct_from_manually_authored_links(self):
        checker = load("check_artifacts")
        parser = checker.SourceLinks()
        parser.feed('<a href="https://github.com/stritzinger/trellang/blob/main/docs/dev.config.example">example</a>'
                    '<a href="https://github.com/stritzinger/trellang/blob/v1.2.0/src/trello.erl" aria-label="View Source">source</a>')
        self.assertEqual(["https://github.com/stritzinger/trellang/blob/v1.2.0/src/trello.erl"], parser.links)

    def test_missing_docs_and_known_secret_patterns(self):
        checker = load("check_artifacts")
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaises(ValueError):
                checker.docs(Path(temporary), "1.2.0")
        for data in (b"-----BEGIN PRIVATE KEY-----", b"ghp_" + b"a" * 40):
            with self.assertRaises(ValueError):
                checker.scan(data)


if __name__ == "__main__":
    unittest.main()
