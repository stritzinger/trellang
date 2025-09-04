Trello Cards – TDD Implementation Plan (Erlang)
===============================================

Goal
----
Deliver a focused Trello client that operates on existing boards/lists to create, read, and update cards including standard fields and custom fields. All actions are initiated from Erlang; board/list management and deletions/archives are manual in Trello.

Scope
-----
- In-scope: create card, read card, update card standard fields (name, desc, due, pos, labels, members), read board custom field definitions, set card custom field values, list cards for a list.
- Out-of-scope: webhooks, board/list CRUD, card delete/archive (except optional test cleanup), power-ups.

Test Infrastructure
-------------------
- Common Test suites; follow local suite conventions.
- Layers:
  - Unit tests: request building, auth param injection, JSON encode/decode, error mapping (mock HTTP).
  - Integration tests: real Trello REST calls, guarded by env vars.
- Required environment:
  - `TRELLO_KEY`, `TRELLO_TOKEN`
  - `TRELLO_BOARD_ID` (existing)
  - `TRELLO_LIST_ID` (existing target list)
  - Optional for custom fields: `TRELLO_CF_ID_TEXT`, `TRELLO_CF_ID_NUMBER`, `TRELLO_CF_ID_CHECKBOX`, `TRELLO_CF_ID_DATE`, `TRELLO_CF_ID_LIST`
- Cleanup: archive or delete test-created cards in `end_per_testcase/2` when enabled by `TRELLO_TEST_CLEANUP=1`.

Tooling and Conventions
-----------------------
- HTTP client with connection reuse and timeouts.
- JSON via Erlang/OTP `json` module (encode/decode to maps).
- Central request builder: base URL `https://api.trello.com/1`, auth query params, default headers, query encoding helpers.
- Retry/backoff middleware for 429/5xx with jitter and limits; prefer low request volume.

OpenAPI / Postman / Docs Usage
------------------------------
- Use OpenAPI as canonical reference for endpoints, params, and status codes.
- Use Postman collection for examples and payload shapes (not as authoritative spec).
- Follow Trello docs for nested resource expansions, limits, rate limits, status codes, and custom fields.

Proposed Modules
----------------
- `trello_client.erl`: low-level HTTP + auth + retry + JSON helpers.
- `trello_cards.erl`: create/get/update card operations; list cards for a list.
- `trello_custom_fields.erl`: fetch board custom field definitions; set card custom field values.

TDD Steps
---------
1) Health check and bootstrap
   - Unit: compose auth query params; validate error mapping skeleton.
   - Integration: `GET /1/members/me` returns 200 with provided credentials; skip if env missing.

2) Read card (existing)
   - Unit: build `GET /1/cards/{id}` with `fields` filter; decode JSON into maps.
   - Integration: fetch a known card (env-provided id); assert fields presence.

3) Create card (minimal)
   - Unit: `POST /1/cards` with required `idList`, `name`.
   - Integration: create on `TRELLO_LIST_ID`; assert 200/201, check `id`, `idList`, `name`; cleanup.

4) Update standard fields
   - Unit: `PUT /1/cards/{id}` for `name`, `desc`, `due`, `pos`, `idLabels`, `idMembers`.
   - Integration: update created card; assert persistence; test partial updates; cleanup.

5) Retrieve board custom field definitions
   - Unit: `GET /1/boards/{boardId}/customFields`.
   - Integration: fetch for `TRELLO_BOARD_ID`; assert shape; cache id/type mapping for tests.

6) Set card custom field values
   - Unit: `PUT /1/card/{cardId}/customField/{customFieldId}/item` payloads by type:
     - text: `{ "value": { "text": "..." } }`
     - number: `{ "value": { "number": "123" } }`
     - checkbox: `{ "value": { "checked": "true" } }`
     - date: `{ "value": { "date": "ISO-8601" } }`
     - list (dropdown): `{ "idValue": "optionId" }`
   - Integration: set then read back via `GET /1/cards/{id}?customFieldItems=true`.

7) List cards for list (filters)
   - Unit: `GET /1/lists/{id}/cards?fields=...&limit=...`.
   - Integration: list cards on `TRELLO_LIST_ID`; assert pagination behavior with `limit`.

8) Error handling and limits
   - Unit: surface 400/401/403/404/429; map to error tuples; retry on 429/5xx with backoff.
   - Integration: negative test (invalid id => 404) to verify decode and error surface.

9) Refine ergonomics
   - Opaque id type as binaries, thin option maps for updates, small pagination helper.

10) Documentation and examples
   - Add cookbook snippets to README/docs for create/update/list and custom field write/read.

Acceptance Criteria
-------------------
- Health: `/members/me` succeeds when creds provided.
- Cards: create minimal card; update name/desc/due/pos/labels/members; read back changes.
- Custom fields: list definitions; set value on a card; read back via `customFieldItems=true`.
- Integration-created cards are cleaned up when cleanup is enabled.
- Unit tests pass offline; integration tests pass with env.

References
----------
- Trello API introduction: `https://developer.atlassian.com/cloud/trello/guides/rest-api/api-introduction/`
- Nested resources: `https://developer.atlassian.com/cloud/trello/guides/rest-api/nested-resources/`
- Object definitions: `https://developer.atlassian.com/cloud/trello/guides/rest-api/object-definitions/`
- Limits: `https://developer.atlassian.com/cloud/trello/guides/rest-api/limits/`
- Rate limits: `https://developer.atlassian.com/cloud/trello/guides/rest-api/rate-limits/`
- Custom fields: `https://developer.atlassian.com/cloud/trello/guides/rest-api/getting-started-with-custom-fields/`
- Authorization: `https://developer.atlassian.com/cloud/trello/guides/rest-api/authorization/`
- Status codes: `https://developer.atlassian.com/cloud/trello/guides/rest-api/status-codes/`
- OpenAPI: `https://dac-static.atlassian.com/cloud/trello/swagger.v3.json?_v=1.688.0`
- Postman collection: `https://developer.atlassian.com/cloud/trello/trello.postman.json`
- Erlang/OTP json: `[json (stdlib v7.0.2)](https://www.erlang.org/doc/apps/stdlib/json.html)`


