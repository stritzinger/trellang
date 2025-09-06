Introduction to Trello's REST API resoruces:

https://developer.atlassian.com/cloud/trello/guides/rest-api/api-introduction/
https://developer.atlassian.com/cloud/trello/guides/rest-api/nested-resources/
https://developer.atlassian.com/cloud/trello/guides/rest-api/object-definitions/
https://developer.atlassian.com/cloud/trello/guides/rest-api/limits/
https://developer.atlassian.com/cloud/trello/guides/rest-api/rate-limits/
https://developer.atlassian.com/cloud/trello/guides/rest-api/getting-started-with-custom-fields/
https://developer.atlassian.com/cloud/trello/guides/rest-api/authorization/
https://developer.atlassian.com/cloud/trello/guides/rest-api/webhooks/
https://developer.atlassian.com/cloud/trello/guides/rest-api/automating-exports/
https://developer.atlassian.com/cloud/trello/guides/rest-api/action-types/
https://developer.atlassian.com/cloud/trello/guides/rest-api/status-codes/

OpenAPI 
https://dac-static.atlassian.com/cloud/trello/swagger.v3.json?_v=1.688.0

Postman Collection
https://developer.atlassian.com/cloud/trello/trello.postman.json


Project notes (dev/test/CI)
---------------------------

Local configuration (dev.config):
- Provide `trellang.trello_key` and `trellang.trello_token` in a non-committed `dev.config`. See `docs/dev.config.example`.
- Run with `ERL_FLAGS="-config ./dev.config"`.

Test conventions:
- tests create ephemeral cards on `list_id` and clean them up when configured
- avoid global board coupling in library code; pass ids in tests

CI (GitHub Actions) example:
```
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
      - name: Write dev.config
        run: |
          printf "%s" "${{ secrets.TRELLO_DEV_CONFIG }}" > dev.config
      - name: Unit tests
        run: ERL_FLAGS="-config ./dev.config" rebar3 eunit
      - name: Common Test (integration)
        run: ERL_FLAGS="-config ./dev.config" rebar3 ct
```

Examples and cookbook:
- See `src/trello.erl` `-moduledoc` and `-doc` sections.