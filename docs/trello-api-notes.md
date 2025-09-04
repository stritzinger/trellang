Trello REST API – Implementation Notes (Erlang)
==============================================

Sources
-------
- API intro: `https://developer.atlassian.com/cloud/trello/guides/rest-api/api-introduction/`
- Nested resources: `https://developer.atlassian.com/cloud/trello/guides/rest-api/nested-resources/`
- Object definitions: `https://developer.atlassian.com/cloud/trello/guides/rest-api/object-definitions/`
- Limits: `https://developer.atlassian.com/cloud/trello/guides/rest-api/limits/`
- Rate limits: `https://developer.atlassian.com/cloud/trello/guides/rest-api/rate-limits/`
- Custom fields: `https://developer.atlassian.com/cloud/trello/guides/rest-api/getting-started-with-custom-fields/`
- Authorization: `https://developer.atlassian.com/cloud/trello/guides/rest-api/authorization/`
- Webhooks: `https://developer.atlassian.com/cloud/trello/guides/rest-api/webhooks/`
- Automating exports: `https://developer.atlassian.com/cloud/trello/guides/rest-api/automating-exports/`
- Action types: `https://developer.atlassian.com/cloud/trello/guides/rest-api/action-types/`
- Status codes: `https://developer.atlassian.com/cloud/trello/guides/rest-api/status-codes/`
- Erlang/OTP json docs: `https://www.erlang.org/doc/apps/stdlib/json.html`

Artifacts
---------
- OpenAPI (as published by Atlassian): `docs/trello-openapi.json`
- Postman collection: `docs/trello.postman.json`

Base API and Authentication
---------------------------
- Base URL: `https://api.trello.com/1`
- Auth: use API key and token.
  - Query params: `?key=YOUR_KEY&token=YOUR_TOKEN`
  - Or HTTP headers where supported; query params are simplest and documented.
- Tokens are user-scoped; treat as secrets. Prefer a non-committed Erlang `dev.config` loaded into the application environment for local and CI runs.

Nested Resources and Expansions
-------------------------------
- Many endpoints support including child resources via query flags to avoid N+1 calls, e.g.:
  - Boards: `GET /1/boards/{id}?lists=open&cards=open&members=all`
  - Lists on a board: `GET /1/boards/{id}/lists`
  - Cards on a list: `GET /1/lists/{id}/cards`
- Prefer expansions for read-heavy flows; keep responses bounded with filters like `cards=open`, `members=none/all`.

Pagination and Filtering
------------------------
- Large collections (e.g., actions, cards) require paging.
- Common patterns: `limit`, `before`, `since`, and specific filters per endpoint.
- Use `before`/`since` (creation timestamps/ids) to walk timelines reliably.

Key Objects (high level)
------------------------
- Board, List, Card, Action, Member, Organization(Workspace), Label, Checklist, Attachment
- CustomField (definition) and CustomFieldItem (value on a card)
- Prefer treating IDs as opaque strings. Do not parse.

Limits (operational)
--------------------
- Trello enforces size/count limits across names, descriptions, attachments, checklists, etc.
- Consult the limits guide when designing bulk operations and validators.

Rate Limits
-----------
- API enforces rate limits per key/token. Exceeding returns HTTP 429.
- Guidance:
  - Implement exponential backoff with jitter on 429/5xx.
  - Prefer webhooks over polling to reduce request volume.
  - Inspect response headers for retry hints when present.

Custom Fields
-------------
- Custom Fields are managed via dedicated endpoints and must be enabled on a board.
- Values live on cards as `customFieldItems`; definitions live as board-level `customFields`.
- Read: expand via `customFieldItems=true` on card requests when needed.

Webhooks
--------
- Create with `POST /1/webhooks` using `callbackURL` and `idModel` (board, card, etc.).
- Your endpoint must respond quickly (200) to verification and delivery requests.
- Validate signatures/headers per guide where applicable; handle retries idempotently.

Automating Exports
------------------
- Use REST endpoints to export board JSON on schedules (external scheduler/cron).
- Keep exports bounded with filters and include action history only as required.

Action Types
------------
- Actions represent activity events and support filtering by `filter` and `since/before`.
- Useful for sync/incremental processing pipelines.

Status Codes (common)
---------------------
- 200/201/204 success; 400 bad request; 401/403 auth issues; 404 not found; 409 conflict; 429 rate limited; 5xx server errors.

Erlang Implementation Notes
---------------------------
- HTTP client: prefer a robust client (e.g., `hackney`) with connection reuse and timeouts.
- JSON: use Erlang/OTP built-in `json` and decode to maps; keep raw maps for flexibility.
- Credentials: load `trellang.trello_key` and `trellang.trello_token` from the application environment (e.g., via `application:get_env/2`); compose query params centrally.
- Request builder: central function to add base URL, auth params, standard headers, and encode query.
- Pagination helpers: utilities to iterate with `before/since/limit` and accumulate results safely.
- Rate-limit handling: middleware that detects 429/5xx and retries with exponential backoff + jitter.
- Webhooks: lightweight HTTP handler that validates headers, decodes JSON, and enqueues work; respond 200 fast.
- Idempotency: Trello lacks idempotency keys; dedupe by action id or application keys when retrying writes.
- Testing: use the Postman collection as reference; mock HTTP in unit tests; integration tests driven by non-committed `dev.config`.

Examples
--------
```
GET /1/members/me/boards?key=KEY&token=TOKEN&fields=name,url&lists=none

GET /1/boards/{boardId}?key=KEY&token=TOKEN&lists=open&cards=open&members=all

POST /1/cards?key=KEY&token=TOKEN&name=Hello&due=null&idList={listId}
```


