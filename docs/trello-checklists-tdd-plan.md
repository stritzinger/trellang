Trello Checklists & Check Items – TDD Plan (Erlang)
===================================================

Goal
----
Add first-class support for Trello checklists and check items on existing cards. All actions are initiated from Erlang; boards/lists are managed manually in Trello. The API remains board-agnostic: only `trello_key`/`trello_token` are read from the application environment; all identifiers (card, checklist, check item) are explicit parameters.

Scope
-----
- In-scope:
  - List a card’s checklists and their items
  - Create, rename, reorder, and delete checklists
  - Create, rename, reorder, toggle complete/incomplete, and delete check items
  - Optional (phase 2): set check item due date and assignee
- Out-of-scope:
  - Webhooks, power-ups, global search, cross-board ops

Configuration & Test Conventions
--------------------------------
- Credentials via non-committed `dev.config` (loaded using `ERL_FLAGS="-config ./dev.config"`).
- Tests read `trellang.board_id`/`trellang.list_id` for setup, but the library API is board-agnostic.
- Integration tests create ephemeral cards on the configured list and clean them up when `trellang.test_cleanup=true`.
- JSON: Erlang/OTP built-in `json` only.

Proposed API Additions (module: `trello.erl`)
---------------------------------------------
- Checklists (for a card):
  - `list_checklists(CardId) -> {ok, [ChecklistMap]}`
  - `create_checklist(CardId, Name) -> {ok, ChecklistMap}`
  - `rename_checklist(ChecklistId, NewName) -> {ok, ChecklistMap}`
  - `set_checklist_pos(ChecklistId, Pos) -> {ok, ChecklistMap}`
  - `delete_checklist(ChecklistId) -> {ok, ok}`
- Check items:
  - `add_check_item(ChecklistId, Name, Opts=#{}) -> {ok, CheckItemMap}`  %% Opts: `pos`, `checked`
  - `set_check_item_state(CardId, CheckItemId, State) -> {ok, CheckItemMap}` %% State: `complete`|`incomplete`
  - `rename_check_item(CardId, CheckItemId, NewName) -> {ok, CheckItemMap}`
  - `set_check_item_pos(CardId, CheckItemId, Pos) -> {ok, CheckItemMap}`
  - `delete_check_item(ChecklistId, CheckItemId) -> {ok, ok}`
  - (Phase 2) `set_check_item_due(CardId, CheckItemId, DueISO8601|null) -> {ok, CheckItemMap}`
  - (Phase 2) `assign_check_item_member(CardId, CheckItemId, MemberId|null) -> {ok, CheckItemMap}`

Reference Endpoints (OpenAPI names may vary)
-------------------------------------------
- List checklists for a card: `GET /1/cards/{cardId}/checklists`
- Create checklist on a card: `POST /1/cards/{cardId}/checklists?name=...&pos=...`
- Update checklist name/pos: `PUT /1/checklists/{id}?name=...&pos=...`
- Delete checklist: `DELETE /1/checklists/{id}`
- Add check item: `POST /1/checklists/{id}/checkItems?name=...&pos=...&checked=...`
- Update check item (rename/pos/state/due/assignee): `PUT /1/cards/{cardId}/checkItem/{idCheckItem}?name=...&pos=...&state=...&due=...&idMember=...`
- Delete check item: `DELETE /1/checklists/{idChecklist}/checkItems/{idCheckItem}`

TDD Steps
---------
1) Suite scaffolding
   - Create `test/trello_checklists_SUITE.erl` with `init_per_suite/1` starting deps if needed, and `end_per_testcase/2` cleanup (archive/delete created cards when enabled).
   - Add helpers to create a temporary card on `trellang.list_id` for each test.

2) List checklists for a card
   - Unit: build `GET /1/cards/{cardId}/checklists` request and decode JSON to maps.
   - Integration: for a fresh card, expect `[]` initially.

3) Create a checklist on a card
   - Unit: build `POST /1/cards/{cardId}/checklists?name=...&pos=...`.
   - Integration: create with a name, assert 200 and fields (`id`, `name`, `idCard`). Then re-list and assert presence.

4) Rename a checklist
   - Unit: build `PUT /1/checklists/{id}?name=...`.
   - Integration: rename, then re-list or `GET /1/cards/{cardId}/checklists` to verify.

5) Reorder a checklist
   - Unit: build `PUT /1/checklists/{id}?pos=top|bottom|number`.
   - Integration: set to `top`, re-list and verify relative order.

6) Add check items
   - Unit: build `POST /1/checklists/{id}/checkItems?name=...&pos=...&checked=...`.
   - Integration: add two items, verify they appear under the checklist via list endpoint; account for eventual consistency by polling with a short backoff when needed.

7) Toggle check item state (complete/incomplete)
   - Unit: build `PUT /1/cards/{cardId}/checkItem/{idCheckItem}?state=complete|incomplete`.
   - Integration: mark complete, read back and assert `state` or `completed` flag; then mark incomplete and verify.

8) Rename a check item
   - Unit: build `PUT /1/cards/{cardId}/checkItem/{idCheckItem}?name=...`.
   - Integration: rename and verify via re-fetch.

9) Reorder a check item
   - Unit: build `PUT /1/cards/{cardId}/checkItem/{idCheckItem}?pos=top|bottom|number`.
   - Integration: reorder relative to another item; verify order via list.

10) Delete a check item
   - Unit: build `DELETE /1/checklists/{idChecklist}/checkItems/{idCheckItem}`.
   - Integration: delete and verify it disappears from listing.

11) Delete a checklist
   - Unit: build `DELETE /1/checklists/{id}`.
   - Integration: delete the checklist and verify removal from card listing.

12) Optional phase 2 (dates and assignments)
   - Unit: extend check item update with `due` and `idMember` fields.
   - Integration: set a due date and assign/unassign a visible member; verify through re-fetch.

Ergonomics
----------
- Provide thin helpers with clear types and binaries for ids and names.
- Consider `trello:ensure_checklist(CardId, Name)` for idempotent create-or-get (optional).
- Integrate checklist data into `trello:dump_board/2` behind an opt flag: e.g., `#{include_checklists => true}`.

Error handling & backoff
------------------------
- Reuse existing retry/backoff with jitter for 429/5xx.
- Surface 4xx errors as `{error, {status, Code, Body}}` consistently.

Acceptance Criteria
-------------------
- Fresh card initially lists zero checklists.
- Can create, rename, reorder, and delete a checklist on a card.
- Can add, rename, reorder, toggle, and delete check items.
- Optional: can set/unset check item due date and member assignment.
- Tests clean up cards when `trellang.test_cleanup=true`.

References
----------
- Trello API introduction: `https://developer.atlassian.com/cloud/trello/guides/rest-api/api-introduction/`
- Nested resources: `https://developer.atlassian.com/cloud/trello/guides/rest-api/nested-resources/`
- Object definitions: `https://developer.atlassian.com/cloud/trello/guides/rest-api/object-definitions/`
- Limits: `https://developer.atlassian.com/cloud/trello/guides/rest-api/limits/`
- Rate limits: `https://developer.atlassian.com/cloud/trello/guides/rest-api/rate-limits/`
- Authorization: `https://developer.atlassian.com/cloud/trello/guides/rest-api/authorization/`
- Status codes: `https://developer.atlassian.com/cloud/trello/guides/rest-api/status-codes/`
- OpenAPI: `https://dac-static.atlassian.com/cloud/trello/swagger.v3.json?_v=1.688.0`
- Postman collection: `https://developer.atlassian.com/cloud/trello/trello.postman.json`
- Erlang/OTP json: `[json (stdlib v7.0.2)](https://www.erlang.org/doc/apps/stdlib/json.html)`


