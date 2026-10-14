#!/usr/bin/env python3
"""Validate the built Hex package and ExDoc output without publishing anything."""
import argparse
import hashlib
from html.parser import HTMLParser
import io
from pathlib import Path, PurePosixPath
import re
import subprocess
import tarfile
import tempfile

ROOT = Path(__file__).resolve().parents[1]
PACKAGE_FILES = {"rebar.config", "rebar.lock", "LICENSE.md", "README.md", "CHANGELOG.md",
                 "AGENTS.md", "docs/dev.config.example", "docs/releases.md"}
DOC_EXTENSIONS = {".html", ".js", ".css", ".woff2", ".woff", ".ttf", ".png", ".svg", ".ico", ".epub", ".txt"}
SECRET_PATTERNS = [rb"-----BEGIN (?:[A-Z]+ )?PRIVATE KEY-----",
                   rb"\bgh[pousr]_[A-Za-z0-9]{30,}\b", rb"\bgithub_pat_[A-Za-z0-9_]{40,}\b"]


class SourceLinks(HTMLParser):
    def __init__(self):
        super().__init__()
        self.links = []

    def handle_starttag(self, tag, attrs):
        attributes = dict(attrs)
        if tag == "a" and (attributes.get("title") == "View Source" or
                           attributes.get("aria-label") == "View Source"):
            self.links.append(attributes.get("href", ""))


def require(condition):
    if not condition:
        raise ValueError("artifact check failed")


def scan(data):
    require(not any(re.search(pattern, data) for pattern in SECRET_PATTERNS))


def metadata(*args):
    result = subprocess.run(["escript", str(ROOT / "scripts/check_release.escript"), *args],
                            cwd=ROOT, capture_output=True, text=True, check=True, timeout=60)
    return result.stdout.strip()


def package(path, version):
    require(path.name == f"trellang-{version}.tar")
    allowed = PACKAGE_FILES | {p.relative_to(ROOT).as_posix() for p in (ROOT / "src").glob("*.erl")}
    allowed.add("src/trellang.app.src")
    with tarfile.open(path, "r:") as outer:
        members = outer.getmembers()
        require(len(members) == 4 and {m.name for m in members} == {"VERSION", "CHECKSUM", "metadata.config", "contents.tar.gz"})
        require(all(m.isfile() and m.size < 50_000_000 for m in members))
        archive_version = outer.extractfile("VERSION").read()
        config_bytes = outer.extractfile("metadata.config").read()
        compressed = outer.extractfile("contents.tar.gz").read()
        require(archive_version == b"3")
        require(hashlib.sha256(archive_version + config_bytes + compressed).hexdigest().upper().encode()
                == outer.extractfile("CHECKSUM").read())
        with tempfile.TemporaryDirectory(prefix="trellang-package-") as temporary:
            config = Path(temporary) / "metadata.config"
            config.write_bytes(config_bytes)
            metadata("--package-metadata", str(config))
        with tarfile.open(fileobj=io.BytesIO(compressed), mode="r:gz") as content:
            seen = set()
            for member in content.getmembers():
                name = member.name.rstrip("/")
                require(name not in seen and not PurePosixPath(name).is_absolute() and ".." not in PurePosixPath(name).parts)
                seen.add(name)
                if member.isdir():
                    require(name in {"src", "docs"})
                    continue
                require(member.isfile() and name in allowed and member.size < 10_000_000)
                data = content.extractfile(member).read()
                require(not (ROOT / name).is_symlink())
                if name == "src/trellang.app.src":
                    # rebar3_hex serializes this consulted term with canonical formatting.
                    with tempfile.TemporaryDirectory(prefix="trellang-app-") as temporary:
                        app_source = Path(temporary) / "trellang.app.src"
                        app_source.write_bytes(data)
                        metadata("--app-source", str(app_source))
                else:
                    require(data == (ROOT / name).read_bytes())
                scan(data)
            require(allowed.issubset(seen))


def docs(path, version):
    required = {"index.html", "api-reference.html", "readme.html", "changelog.html", "agents.html",
                "releases.html", "trello.html", "trello_board_server.html"}
    require(path.is_dir() and all((path / name).is_file() for name in required))
    expected_link = f"https://github.com/stritzinger/trellang/blob/v{version}/"
    module_source_pages = set()
    for file in path.rglob("*"):
        require(not file.is_symlink())
        if file.is_dir():
            continue
        require((file.suffix in DOC_EXTENSIONS or file == path / ".build") and file.stat().st_size < 50_000_000)
        data = file.read_bytes()
        scan(data)
        if file.suffix == ".html":
            parser = SourceLinks()
            parser.feed(data.decode("utf-8"))
            for link in parser.links:
                # ExDoc's aggregate API page points to source_url, not a source file.
                require(link.startswith(expected_link) or
                        (file == path / "api-reference.html" and link == "https://github.com/stritzinger/trellang"))
            if parser.links:
                module_source_pages.add(file.name)
    for name in ("api-reference.html", "trello.html", "trello_board_server.html"):
        require(f"trellang v{version}".encode() in (path / name).read_bytes())
    for name in ("trello.html", "trello_board_server.html"):
        require(name in module_source_pages)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--package", type=Path, required=True)
    parser.add_argument("--docs", type=Path, required=True)
    args = parser.parse_args()
    try:
        version = metadata("--version")
        package(args.package, version)
        docs(args.docs, version)
    except (OSError, ValueError, KeyError, tarfile.TarError, subprocess.SubprocessError):
        print("Artifact check failed: inspect package allowlist/source/version and complete versioned ExDoc output locally.")
        return 1
    print(f"Package and documentation valid: trellang {version}.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
