# Releases

Publish Trellang packages and documentation from a development machine. GitHub
Actions validates releases and runs approved Trello integration tests; it never
publishes to Hex and must never receive Hex credentials.

## Version history and source of truth

The version in `src/trellang.app.src` is authoritative. A published version must
have matching application metadata, a dated [changelog](../CHANGELOG.md) entry,
an immutable annotated `vX.Y.Z` Git tag, a Hex package and versioned HexDocs.
An application version or changelog entry on GitHub alone does not mean that
version has been published.

The historical releases were reconciled as follows:

| Version | Release date | Source |
| --- | --- | --- |
| 1.0.0 | 2025-09-08 | Existing `v1.0.0` tag |
| 1.1.0 | 2025-09-18 | Restored annotated `v1.1.0` tag at `facc038f37d8422ffe93ef236a8d50cfa9ad8c5b`; every packaged file matches this commit |
| 1.2.0 | 2026-10-14 | `v1.2.0`; credentials-aware APIs and board mirror server |

The missing `v1.1.0` tag was restored and pushed after comparing every file in
the published archive with the verified commit above. The published package was
unchanged; the tag annotation records its recovery after publication. Release
dates follow the changelog; confirm published versions on
[Hex](https://hex.pm/packages/trellang) and [HexDocs](https://hexdocs.pm/trellang/).

## Toolchain and credential protection

Trellang supports OTP 27 or later. Use OTP **28.3.1** and Rebar3 **3.27.0** for
release validation. Maintainer plugins are pinned in `project_plugins`:
`rebar3_hex` **7.0.10** and `rebar3_ex_doc` **0.2.30**. The Hex plugin pins its
production dependencies to `hex_core` **0.10.3** and `verl` **1.1.1**, neither of
which has additional production dependencies. The ExDoc plugin has no Erlang
dependencies and bundles its ExDoc executable. Application `rebar.lock` does not
lock project plugins; review the complete plugin dependency resolution when
updating these pins.

Keep local Trello configuration outside Git, with permissions `0600`. The test
wrapper creates private temporary configuration and logs and forces cleanup of
cards owned by that run. Do not attach raw test logs to issues or CI artifacts.
Keep Hex authentication in the development machine's Rebar3/Hex credential store.

GitHub's `trello-tests` environment contains only individual `TRELLO_KEY` and
`TRELLO_TOKEN` secrets, with board/list IDs in `TRELLO_BOARD_ID` and
`TRELLO_LIST_ID` environment variables. The environment requires
`@stritzinger/developers` approval, prevents self-review, disables administrator
bypass, and is restricted to release tags. Tags must point to `main` commits
with successful release validation. Repository protections require pull requests
and passing CI on `main`; independent PR review is optional. Code owners identify
reviewers for sensitive CI/test/dependency files. Release tags are immutable. Secret scanning and
push protection are enabled. Actions use verified full commit SHA pins and a
selected-action allowlist; workflow dependency lockfiles
require GitHub Enterprise and are not available on this organization's Team plan.

## Prepare and validate

Work in a clean repository checkout without `_checkouts` overrides. Submit
release changes through the protected `main` pull-request process; required checks
must pass, and maintainers may authorize their own merges. The separate approval
for credentialed integration jobs remains mandatory. Inspect the package allowlist
in the application metadata whenever adding files.

1. Set the intended version in `src/trellang.app.src` and record observable changes
   under `## vX.Y.Z — Unreleased` in the changelog. Restore or update public docs.
2. Run the following from the repository root, using a test board/list and local
   credentials. Missing suites, skipped tests or failures block release readiness.

   ```sh
   scripts/check_release.escript
   python3 -m unittest discover -s scripts/tests
   rebar3 compile
   rebar3 as test compile
   python3 scripts/run_ct.py --config ./dev.config
   rebar3 dialyzer
   rebar3 hex build package
   rebar3 hex build docs
   python3 scripts/check_artifacts.py \
     --package _build/default/lib/trellang/hex/trellang-1.2.0.tar --docs doc
   ```

   The artifact path above uses 1.2.0; substitute the intended version for later
   releases. Build package and docs separately: a combined build can report some
   documentation errors as warnings. Inspect generated docs for warnings, restored
   examples, changelog links and `vX.Y.Z` source links. Artifact checks reject
   unexpected files and missing documentation.
3. Once these checks pass, replace **Unreleased** with the intended publication
   date (`YYYY-MM-DD`), change the changelog comparison link from `HEAD` to
   `vX.Y.Z`, and merge the release preparation into `main` after required CI passes.
4. At that exact clean `main` commit, create an annotated `vX.Y.Z` tag. Run
   `scripts/check_release.escript --tag vX.Y.Z`, then push the tag. Await successful
   CI, including the separately approved full integration test job and Dialyzer,
   for the tag's exact commit. Never move a release tag after a failed check.

## Publish and verify locally

On the development machine, fetch and check out the verified release tag in a
clean checkout. Re-run the tag metadata check and separate package/docs builds
and inspect the artifacts as above. Confirm that the successful GitHub release
validation run is for this exact commit and that the version is not already on
Hex. Then run:

```sh
rebar3 hex publish
```

Review the package/version/file list at the interactive prompt. Do not use
`--replace`. Verify the Hex release, `https://hexdocs.pm/trellang/X.Y.Z/`, the
latest HexDocs redirect and source links to the release tag. In a fresh consumer
project, resolve `{trellang, "X.Y.Z"}` from Hex and compile it without local
overrides. Record completion in the release history table through a normal PR.

If publication times out or otherwise has an uncertain result, inspect Hex before
retrying. If the package exists but docs are missing, first download and compare
the published package files with the tagged source and inspected local package.
Only after an exact match, run `rebar3 hex publish docs` from that tag. If package
contents differ, stop and prepare a corrected version through the normal release
process. Do not overwrite published packages, move tags or claim an incomplete
publication succeeded.
