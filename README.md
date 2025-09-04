trellang
=====

An OTP application

Build
-----

    $ rebar3 compile

Environment
-----------
To run integration tests against Trello you must provide these environment variables:

- `TRELLO_KEY`: Trello API key
- `TRELLO_TOKEN`: Trello API token (user‑scoped)
- `TRELLO_BOARD_ID`: ID of an existing Trello board used for tests
- `TRELLO_LIST_ID`: ID of an existing list on that board used for creating cards
- Optional custom fields (if present on the board):
  - `TRELLO_CF_ID_TEXT`, `TRELLO_CF_ID_NUMBER`, `TRELLO_CF_ID_CHECKBOX`, `TRELLO_CF_ID_DATE`, `TRELLO_CF_ID_LIST`
- Optional: `TRELLO_TEST_CLEANUP=1` to archive/delete test cards at the end of each test

Obtaining Trello credentials
----------------------------
- API key: visit `https://trello.com/app-key` while logged into Trello to retrieve your key.
- Token: from the same page, generate a token for your account (read/write as needed).
- Reference docs: `https://developer.atlassian.com/cloud/trello/guides/rest-api/authorization/`

Local setup (keep secrets out of git)
-------------------------------------
Option A: direnv (recommended)

1. Install direnv and hook it into your shell.
2. Create a `.envrc` (not committed) in the project root:

        export TRELLO_KEY=... 
        export TRELLO_TOKEN=...
        export TRELLO_BOARD_ID=...
        export TRELLO_LIST_ID=...
        # optional
        # export TRELLO_TEST_CLEANUP=1

3. Run `direnv allow` once.

Option B: ad‑hoc shell exports

    $ export TRELLO_KEY=...
    $ export TRELLO_TOKEN=...
    $ export TRELLO_BOARD_ID=...
    $ export TRELLO_LIST_ID=...

Running tests
-------------
- Unit tests (no network):

        $ rebar3 eunit

- Integration tests (requires env vars above):

        $ rebar3 ct

CI setup (GitHub Actions example)
---------------------------------
Store credentials as repository secrets (Settings → Secrets and variables → Actions), then reference them in your workflow:

```yaml
name: CI
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '27'
          rebar3-version: '3.23.0'
      - name: Unit tests
        run: rebar3 eunit
      - name: Common Test (integration)
        env:
          TRELLO_KEY: ${{ secrets.TRELLO_KEY }}
          TRELLO_TOKEN: ${{ secrets.TRELLO_TOKEN }}
          TRELLO_BOARD_ID: ${{ secrets.TRELLO_BOARD_ID }}
          TRELLO_LIST_ID: ${{ secrets.TRELLO_LIST_ID }}
          TRELLO_TEST_CLEANUP: '1'
        run: rebar3 ct
```

Notes
-----
- Do not commit credentials. Keep them in your shell, a `.envrc` managed by direnv, or your CI secrets store.
- IDs can be obtained from the Trello UI (URL path contains board/list IDs) or via the API using your key/token.
