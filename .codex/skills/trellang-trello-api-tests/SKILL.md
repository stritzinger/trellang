---
name: trellang-trello-api-tests
description: Trellang Trello API scoping and Common Test safety rules. Use when editing src/trello.erl or test/**/*.erl that read application env, call Trello APIs, create/update/delete cards, use board/list/member/label ids, or clean up integration-test resources.
---

# Trellang Trello API Tests

## Overview

Use this skill to keep Trello production code board-agnostic while keeping integration tests constrained to the configured development board. These rules cover the former Cursor rules for application env usage, board scope, and Common Test conventions.

## Production Code

- In `src/trello.erl`, read only `trellang.trello_key` and `trellang.trello_token` from the application environment.
- Do not read `board_id`, `list_id`, `member_id`, `label_id`, or other resource identifiers from application env in production/library code.
- Keep the public client board-agnostic and safe for concurrent use across multiple boards.
- Pass identifiers such as `BoardId`, `ListId`, `MemberId`, and `LabelId` as explicit function parameters in the public API.
- Keep legacy arities thin when they read credentials: read env credentials, construct the canonical credentials map, and delegate to the credentials-first arity.

## Integration Test Scope

- Tests may read fixed values such as `trellang.board_id`, `trellang.list_id`, member ids, label ids, and custom-field ids from non-committed `dev.config`.
- Keep fixed board or list ids confined to tests; do not copy test configuration into production modules.
- During testing, Trello API interactions must target only the board id configured as `trellang.board_id`.
- Access to lists on the configured board is allowed, including lists other than the primary `trellang.list_id`.
- For endpoints that accept list ids, ensure the list belongs to the configured board in setup or fixture code.
- When useful, assert returned resources contain `<<"idBoard">>` equal to `trellang.board_id` so misconfiguration fails early.
- Treat the key/token in `dev.config` as account-scoped credentials; do not assume broader permissions.

## Common Test Structure

- Tests may create ephemeral Trello resources such as cards on the configured test list; clean them up when configured.
- Start long-running dependencies in `init_per_suite/1` and stop them in `end_per_suite/1`.
- Do not change directories inside test cases. Common Test already gives each case a suitable working directory under the suite `priv_dir` for generated artifacts.
- Prefer cleanup in `end_per_testcase/2`, such as deleting generated files or Trello resources, instead of wrapping each test in local `try ... after` cleanup.
- Use pattern matching for assertions where appropriate so tests fail fast, for example `{ok, Card} = trello:create_card(ListId, #{name => Name})`.

## Coverage Expectations

- Exclude modules matching `*_app.erl` and `*_sup.erl` from coverage expectations and summaries.
- Rationale: these modules are usually release/supervision wiring and can skew test-time coverage metrics.
