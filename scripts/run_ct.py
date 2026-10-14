#!/usr/bin/env python3
"""Run full Common Test without releasing raw output or failure details to CI."""
import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
FIXTURES = {"init_per_suite", "end_per_suite", "init_per_group", "end_per_group"}


def suites():
    """Only identifiers present in reviewed suite source may appear in output."""
    return {p.stem: FIXTURES | set(re.findall(r"^([a-z][A-Za-z0-9_]*)\s*\(", p.read_text(), re.M))
            for p in (ROOT / "test").glob("*_SUITE.erl")}


def summarize(logs, allowed):
    found = set()
    counts = {"passed": 0, "failed": 0, "skipped": 0}
    valid = bool(allowed)
    reports = list(logs.rglob("junit_report.xml"))
    if len(reports) != 1:
        print("Common Test report missing or ambiguous.")
        return False
    try:
        data = reports[0].read_bytes()
        if len(data) > 10_000_000 or b"<!DOCTYPE" in data or b"<!ENTITY" in data:
            raise ValueError("invalid report")
        report = ET.fromstring(data)
        for suite in report.iter("testsuite"):
            name = suite.get("name")
            if name not in allowed or name in found:
                valid = False
                continue
            found.add(name)
            observed_cases = 0
            for case in suite.findall("testcase"):
                case_name = case.get("name")
                if case_name not in allowed[name]:
                    valid = False
                    continue
                status = "failed" if case.find("failure") is not None or case.find("error") is not None else (
                    "skipped" if case.find("skipped") is not None else "passed")
                print(f"{name}.{case_name}: {status}")
                if status != "passed":
                    valid = False
                if case_name not in FIXTURES:
                    observed_cases += 1
                    counts[status] += 1
            if observed_cases == 0 or int(suite.get("tests", "0")) != observed_cases:
                valid = False
            if any(int(suite.get(key, "-1")) != 0 for key in ("failures", "errors", "skipped")):
                valid = False
    except (ET.ParseError, OSError, ValueError, TypeError):
        print("Common Test report invalid.")
        return False
    if found != set(allowed):
        print("Common Test suite coverage incomplete.")
        valid = False
    print(f"Common Test: suites={len(found)}/{len(allowed)}, passed={counts['passed']}, "
          f"failed={counts['failed']}, skipped={counts['skipped']}.")
    return valid


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, help="local Erlang application config; cleanup is always enabled")
    parser.add_argument("--keep-logs", type=Path, help="new private local directory for failure diagnostics")
    args = parser.parse_args()
    ci = bool(os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"))
    if ci and (args.config or args.keep_logs):
        print("CI accepts environment configuration only and never retains raw logs.")
        return 1
    if args.keep_logs and args.keep_logs.exists():
        print("The private diagnostics destination must not exist.")
        return 1
    os.umask(0o077)
    env = dict(os.environ)
    # Avoid inherited configuration and crash dumps outside the private directory.
    for key in ("ERL_FLAGS", "ERL_AFLAGS", "ERL_ZFLAGS"):
        env.pop(key, None)
    env["ERL_CRASH_DUMP"] = os.devnull
    successful = False
    with tempfile.TemporaryDirectory(prefix="trellang-ct-") as private:
        work = Path(private)
        config = work / "private.config"
        logs = work / "logs"
        logs.mkdir(mode=0o700)
        try:
            with (logs / "runner.log").open("wb") as output:
                command = ["escript", str(ROOT / "scripts/private_ct_config.escript"), str(config)]
                if args.config:
                    command.append(str(args.config.resolve()))
                prepared = subprocess.run(command, cwd=ROOT, env=env, stdout=output, stderr=subprocess.STDOUT,
                                          timeout=60)
                if prepared.returncode != 0:
                    print("Private test configuration unavailable or invalid; tests did not run.")
                    return 1
                for key in ("TRELLO_KEY", "TRELLO_TOKEN", "TRELLO_BOARD_ID", "TRELLO_LIST_ID"):
                    env.pop(key, None)
                result = subprocess.run(["rebar3", "ct", "--sys_config", str(config), "--logdir", str(logs),
                                         "--abort_if_missing_suites", "true"],
                                        cwd=ROOT, env=env, stdout=output, stderr=subprocess.STDOUT, timeout=3600)
            successful = summarize(logs, suites()) and result.returncode == 0
            if not successful:
                print("Common Test release gate failed. Raw diagnostics are private.")
            return 0 if successful else 1
        except (OSError, subprocess.SubprocessError, KeyboardInterrupt):
            print("Common Test runner failed or was interrupted. Raw diagnostics are private.")
            return 1
        finally:
            config.unlink(missing_ok=True)
            if not successful and args.keep_logs:
                try:
                    # Fail rather than reuse a directory that could contain unsafe links.
                    args.keep_logs.mkdir(mode=0o700)
                    shutil.copytree(logs, args.keep_logs / "logs", symlinks=False,
                                    ignore=lambda directory, names: [name for name in names
                                        if (Path(directory) / name).is_symlink()])
                    for path in args.keep_logs.rglob("*"):
                        path.chmod(0o700 if path.is_dir() else 0o600)
                    print("Private diagnostics retained in the requested local directory.")
                except OSError:
                    print("Private diagnostics could not be retained.")


if __name__ == "__main__":
    raise SystemExit(main())
