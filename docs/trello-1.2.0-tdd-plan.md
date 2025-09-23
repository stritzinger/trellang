## Trellang v1.2.0 — TDD Plan: Credentials‑Aware API and Board Mirror GenServer

### Objectives
- Add credentials-aware variants of the Trello API so each call can be made with explicit credentials.
- Introduce a `gen_server` that mirrors a Trello board in memory, started with per-server credentials and a `board_id`.
- Maintain board-agnostic API design; only tests may bind to a specific board via config.
- Preserve backward compatibility: keep existing arities that read key/token from the application env. New arities accept `Credentials` explicitly. Plan deprecation for 2.0.

### Constraints & Conventions
- OTP 27 only; use built-in `httpc` and `json`.
- `trello.erl` may only read `trellang.trello_key` and `trellang.trello_token` from application env; no board/list/member/label ids.
- Tests may read `trellang.board_id` and `trellang.list_id` from `dev.config` and pass them into API functions.
- Fail-fast style for `gen_server` callbacks: no catch-all for `handle_call/3` and `handle_cast/2`; unhandled messages in `handle_info/2` are logged and dropped.

### Credential Model
- Define credentials as an Erlang map: `{trello_key := binary(), trello_token := binary()}`.
- New API arities take `Credentials` as the leading argument, e.g. `create_card(Creds, ListId, Fields)`.
- Existing arities remain and delegate to the new ones by internally calling `get_credentials/0`.

### High-Level Work Plan
1) Add credentials-aware HTTP helpers (non-breaking)
2) Add credentials-aware public API variants per feature area
3) Update docs and examples to show both modes (env-based and explicit credentials)
4) Implement `trello_board_server` GenServer that mirrors a board
5) Final docs, version bump to 1.2.0, and CHANGELOG

---

## Phase A — Credentials-Aware HTTP Helpers

Goal: Extend internal HTTP helper functions to accept credentials explicitly while keeping current behavior intact.

Tests (Common Test): `test/trello_credentials_SUITE.erl`
- setup: read key/token from app env to build `Creds` map; also keep `dev.config` flow.
- tc1: health with explicit creds — calling `trello:me(Creds)` returns `{ok, Map}`.
- tc2: health with invalid creds — returns `{error, {http_status, 401, _}}` (or consistent error tuple).
- tc3: parity — `trello:me()` and `trello:me(Creds)` yield equivalent success results with valid credentials.

Implementation steps:
- Introduce internal `http_*` helper variants that take `Creds` as first parameter (e.g. `http_get_json(Creds, Path, QS, Opts)`).
- Keep existing helpers delegating to the `Creds` variants via `get_credentials/0`.
- Thread `Creds` into retry/backoff logic unchanged.

Deliverable: Passing suite `trello_credentials_SUITE` proving explicit-credentials HTTP path works.

---

## Phase B — Credentials-Aware Public API Variants

Goal: Add new arities that accept `Creds` for all existing features; keep old arities delegating to new.

Coverage Areas and Representative Tests (CT suites may group logically):
1. Cards
   - `create_card(Creds, ListId, Fields)` creates a card; compare to `create_card(ListId, Fields)`.
   - `get_card(Creds, CardId)` vs legacy.
   - `update_card(Creds, CardId, Fields)` vs legacy for name/desc/due/pos/labels/members.
2. Labels & Members
   - `get_label_id_by_color(Creds, BoardId, Color)`; `add_label(Creds, {CardId, LabelId})` and color helper.
   - `list_board_usernames(Creds, BoardId)` and `get_member_id_by_username(Creds, BoardId, Username)`; `add_member(Creds, {CardId, MemberId})`.
3. Custom Fields
   - `list_custom_fields(Creds, BoardId)`; setters for text/date/checkbox on card.
   - `get_card_with_custom_fields(Creds, CardId)`.
4. Lists & Board dump
   - `list_lists(Creds, BoardId)`; `list_cards_for_list(Creds, ListId)`; `dump_board(Creds, BoardId, Opts)`.
5. Checklists & Check Items
   - `list_checklists(Creds, CardId)`; `create_checklist(Creds, CardId, Name)`; `rename_checklist(Creds, {ChecklistId, Name})`; `set_checklist_pos(Creds, {ChecklistId, Pos})`.
   - `add_check_item(Creds, {ChecklistId, Name, Pos})`; `rename_check_item(Creds, {ChecklistId, CheckItemId, Name})`; `set_check_item_state(Creds, {ChecklistId, CheckItemId, State})`; `set_check_item_pos(Creds, {ChecklistId, CheckItemId, Pos})`; `delete_check_item(Creds, {ChecklistId, CheckItemId})`; `set_check_item_due(Creds, {CardId, CheckItemId, Due})`; `assign_check_item_member(Creds, {CardId, CheckItemId, MemberId})`.

Testing approach:
- For each area add CT cases that call both the new `Creds` arity and the legacy arity to ensure functional parity on success and error shapes.
- Reuse existing suites where sensible by parameterizing on a function arity variant.

Implementation steps:
- For each exported function, add a `Creds`-first arity.
- Legacy arity delegates to the new one by constructing `Creds` from `get_credentials/0`.
- Update `-doc` examples to include both styles.

Deliverable: All existing CT suites green plus new tests covering the `Creds` arities.

---

## Phase C — Board Mirror GenServer

Module: `trello_board_server`

Purpose: Maintain an in-memory mirror of a board, fetch-on-demand and optionally periodic refresh.

Public API (initial cut):
- `start_link(Creds, BoardId, Options) -> {ok, Pid}`
  - Options: `[{refresh_interval, non_neg_integer() | disabled}]` default disabled
- `stop(Pid) -> ok`
- `refresh(Pid) -> ok`  — fetch lists, cards, checklists & items; atomically replace state
- `get_state(Pid) -> #{ board_id := binary(), lists := [List], cards := [Card], checklists := [Checklist] }`
- Convenience operations (delegating to `trello` with embedded `Creds` and updating state on success):
  - `create_card(Pid, ListId, Fields) -> {ok, Card}`
  - `update_card(Pid, CardId, Fields) -> {ok, Card}`
  - `add_check_item(Pid, ChecklistId, Name, Pos) -> {ok, CheckItem}`
  - ... (extend as needed after core mirror works)

Server behavior:
- State includes `Creds`, `BoardId`, `RefreshTimerRef | undefined`, last sync timestamp, and mirrored entities.
- `init/1` performs optional initial `refresh` unless disabled via option.
- `handle_info/2` handles refresh timer; logs and drops unknown messages.
- Unknown calls/casts are not matched (fail fast).

Tests (CT): `test/trello_board_server_SUITE.erl`
- suite init: build `Creds` from env key/token; read `board_id` from config (tests only).
- tc1: start/stop server without refresh; `get_state/1` returns empty structures with correct `board_id`.
- tc2: `refresh/1` populates lists, cards, checklists to match direct `trello:dump_board(Creds, BoardId, #{include_checklists => true})`.
- tc3: `create_card/4` via server returns card and subsequent `get_state/1` includes it (with eventual consistency wait if needed).
- tc4: update card name via server, then refresh and assert change present.
- tc5: optional periodic refresh — start with `refresh_interval => 1000`, mutate board externally, assert state eventually reflects changes.
- tc6: error propagation — simulate bad credentials (start with invalid token); operations return clear error tuples and state remains unchanged.

Implementation steps:
- Create module `trello_board_server` with proper `-moduledoc` and `-doc` for public API.
- Add supervisor child spec example in docs, but do not add app-level supervision wiring (library remains opt-in).
- Use existing `trello` functions with `Creds` arities for all fetches and writes.

Deliverable: Passing `trello_board_server_SUITE` demonstrating mirror correctness and basic CRUD delegation.

---

## Phase D — Documentation & Migration

- Update `README.md` with a concise section:
  - How to construct `Creds` maps
  - Using `trello` with explicit credentials vs env-based
  - Starting `trello_board_server` and reading mirrored state
- Add `-doc` examples for all new `Creds` arities and for `trello_board_server` APIs.
- Add `CHANGELOG.md` entry for v1.2.0 featuring credentials-aware API and board mirror server.
- Keep ExDoc config unchanged (no `source_ref`).

---

## Phase E — Versioning & Release Checklist

- Bump `src/trellang.app.src` `vsn` to "1.2.0".
- Add `CHANGELOG.md` section for 1.2.0.
- Tag `v1.2.0` and publish to Hex (docs via ExDoc).

---

## Risks & Mitigations
- Trello eventual consistency: employ wait/retry helpers in CT where necessary.
- Rate limits (429): rely on existing retry/backoff in HTTP helpers.
- Backward compatibility: keep legacy arities until 2.0; mark deprecation in docs but not in API yet.

---

## Deliverables
- New CT suites: `trello_credentials_SUITE.erl`, `trello_board_server_SUITE.erl`.
- Refactored `trello.erl` with `Creds`-first arities for all public functions.
- New module `trello_board_server` with full docs.
- Updated README and CHANGELOG; version bumped to 1.2.0.
