Introduction to Trello's REST API resources:

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

Toolchain:
- Support Erlang/OTP 27 or later; release validation uses OTP 28.3.1 and Rebar3 3.27.0.
- Use Common Test for unit and integration tests. Run Dialyzer after all required tests pass.
- Keep documentation and publishing tools in `project_plugins`; see [release procedure](docs/releases.md).

Local configuration:
- Keep `trellang.trello_key` and `trellang.trello_token` in a non-committed `dev.config` with permissions `0600`. See [the example](https://github.com/stritzinger/trellang/blob/main/docs/dev.config.example).
- Run the complete integration suite with `python3 scripts/run_ct.py --config ./dev.config`. The wrapper captures raw output privately and fails on missing configuration, failed tests or skipped tests.
- To diagnose a local failure, use `--keep-logs /path/to/new-private-directory`. Never upload that directory; test errors can contain credentials.
- Test cards belong to the current run and are cleaned up after tests. Never clean up unrelated board cards. Pass board/list IDs explicitly in library calls.

GitHub Actions:
- Pull requests and trusted pushes compile sources/tests and validate release metadata, package and docs without secrets.
- Complete integration tests run only for release tags through the protected `trello-tests` environment. Require approval by `@stritzinger/developers`, prevent self-review and disable administrator bypass.
- Store `TRELLO_KEY` and `TRELLO_TOKEN` as separate environment secrets. Store `TRELLO_BOARD_ID` and `TRELLO_LIST_ID` as environment variables. Never store a whole Erlang config as a secret or interpolate a secret into shell source.
- Protect release tags and `main`; require passing CI on pull requests. Independent PR review is optional; code owners identify reviewers for workflow, test and dependency changes. Enable secret scanning and push protection.
- Keep workflow permissions read-only. Pin actions by full commit SHA, use GitHub-hosted runners, and set checkout `persist-credentials: false`.
- Credentialed jobs publish only allowlisted test names, results and counts. Do not upload raw test logs, configuration, crash dumps or caches.
- Hex publication happens only on development machines. Never put Hex credentials in GitHub secrets or publish to Hex from CI.

After checkout and toolchain setup, the protected integration job uses this step:

```yaml
- name: Common Test integration
  env:
    TRELLO_KEY: ${{ secrets.TRELLO_KEY }}
    TRELLO_TOKEN: ${{ secrets.TRELLO_TOKEN }}
    TRELLO_BOARD_ID: ${{ vars.TRELLO_BOARD_ID }}
    TRELLO_LIST_ID: ${{ vars.TRELLO_LIST_ID }}
  run: python3 scripts/run_ct.py
```

Run Dialyzer in a subsequent step without credentials. Keep package/documentation builds in a separate job without access to the environment.

Examples and cookbook:
- See `src/trello.erl` `-moduledoc` and `-doc` sections and the [README examples](README.md#examples).
