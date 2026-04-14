---
name: trellang-erlang-code-style
description: Repo-specific Erlang coding conventions for Trellang. Use when editing Erlang modules in src/**/*.erl or test/**/*.erl, especially HTTP helpers, gen_server callbacks, documentation attributes, JSON handling, pg usage, or boundary validation and pattern matching.
---

# Trellang Erlang Code Style

## Overview

Follow these conventions when changing Erlang code in the Trellang repository. They encode the former Cursor rules for Erlang structure, documentation, JSON, `gen_server`, and `pg` usage.

## Boundary And Data Shape

- Build definitive input once at the boundary, such as config or environment readers returning a fully shaped map, then pass that map through internal helpers without repeated validation.
- Pattern-match required shapes in function heads instead of branching with `case` or `if` inside hot-path bodies.
- Normalize types at the consumer, such as calling `to_bin/1` immediately before building HTTP query parameters rather than in producers.
- Let internal helpers fail fast on invalid input with `function_clause` or `badmatch`; public API edges should return `{ok, Value} | {error, Reason}`.
- Keep legacy arities thin: read environment or credentials, construct the canonical map, and delegate to the credentials-first arity.
- Avoid catch-all clauses that hide programmer errors, especially in `handle_call/3` and `handle_cast/2`.

Prefer this shape:

```erlang
do_get(#{trello_key := Key0, trello_token := Token0}, Path, QueryKVs) ->
    Url = build_url(Path, [{<<"key">>, to_bin(Key0)}, {<<"token">>, to_bin(Token0)} | QueryKVs]),
    http_get_json(Url).

do_get(Path, QueryKVs) ->
    do_get(get_credentials(), Path, QueryKVs).
```

## Documentation

- Use `-moduledoc` at the top of every public module to describe purpose and usage. Prefer a concise overview with examples.
- Document every exported function with `-doc` immediately before the function, or via external doc files. Include brief purpose and a short example when useful.
- Prefer Markdown documentation. Use reserved metadata such as `since` or `deprecated` when applicable.
- Hide non-API/internal modules with `-moduledoc false.` when needed. Do not mark exported API functions with `-doc false`.
- Document types and callbacks with `-doc`, and provide `-spec` or `-type` where appropriate.
- Erlang documentation attributes reference: `https://erlang.org/documentation/doc-15.0-rc2/doc/system/documentation.html`.

## JSON

- Use Erlang/OTP built-in `json` for all JSON encoding and decoding.
- Do not add or use third-party JSON libraries such as `jiffy`, `jsx`, or `jsone`.
- Prefer maps as decoded representation and binaries for serialized payloads.
- Centralize JSON encode/decode helpers so options and response handling stay consistent.
- Erlang/OTP JSON reference: `https://www.erlang.org/doc/apps/stdlib/json.html`.

## gen_server

- For `handle_call/3` and `handle_cast/2`, match only implemented request patterns.
- Do not add catch-all `handle_call/3` or `handle_cast/2` clauses. Unknown calls or casts should crash with `function_clause` so missing branches are visible during testing.
- For `handle_info/2`, explicitly match known messages such as timers and monitors first.
- Add a generic `handle_info/2` only to log unexpected external or system messages and drop them with `{noreply, State}`.
- Prefer `logger:debug("Unhandled info: ~p", [Info])`, or warn only when the message may indicate an issue.
- Keep logging lightweight; throttle or avoid warning in hot loops.

## pg Scopes

- Do not call `pg:start_link/0` as if `pg` had a singleton server.
- Start each explicit scope with `pg:start_link(Scope)`, usually under a supervisor.
- Interact directly with the intended scope using calls such as `pg:join(Scope, Group, Pid)` and `pg:get_members(Scope, Group)`.
- Remember that calls without a scope use the default scope.
- If preserving code that follows the existing Skylark convention, use scope `skylark` for groups such as `{DronePid, Group}` and ensure that scope server is started.
