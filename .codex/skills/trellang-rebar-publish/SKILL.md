---
name: trellang-rebar-publish
description: Trellang Rebar3, ExDoc, Hex, relx, and coverage configuration rules. Use when editing rebar.config, release metadata, docs publishing config, Hex publishing workflow, or coverage expectations for *_app.erl and *_sup.erl modules.
---

# Trellang Rebar Publish

## Overview

Use this skill when changing Trellang build, docs, release, or publishing configuration. It captures the former Cursor rule for `rebar.config` and related release metadata.

## rebar.config Rules

- Use `{project_plugins, [rebar3_hex, rebar3_ex_doc]}` rather than `plugins`, so downstream library users do not need those plugins installed to build their applications.
- Configure ExDoc with `{ex_doc, [...]}` including `{extras, ["README.md"]}`, `{main, "README.md"}`, and `{source_url, "https://github.com/stritzinger/trellang"}`.
- Keep `source_ref` omitted from ExDoc config. The plugin infers it from the tag or version, and setting it can cause warnings.
- Enable HexDocs publishing with `{hex, [{doc, ex_doc}]}` so `rebar3 hex publish` builds and uploads ExDoc output.
- Provide a minimal relx release definition for local runs: `{relx, [{release, {trellang, semver}, [trellang]}]}`.

## Publishing Checklist

- Ensure `src/trellang.app.src` `vsn` matches the release version before publishing.
- Tag releases as `vX.Y.Z` in git.
- Publish with `rebar3 hex publish`; docs should build through ExDoc and upload to HexDocs because of the `{hex, [{doc, ex_doc}]}` config.
- Generate local docs with `rebar3 ex_doc` when validating documentation changes.

## Coverage

- Exclude modules matching `*_app.erl` and `*_sup.erl` from coverage expectations and summaries.
- Rationale: these modules are straightforward application and supervisor wiring and can skew test-time coverage metrics.
