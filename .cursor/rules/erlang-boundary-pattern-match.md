### Erlang: Validate/Normalize at the Boundary; Pattern‑Match and Fail Fast Internally

Applies to: Erlang modules in this repo (helpers, HTTP wrappers, `gen_server` callbacks).

Rule
- Build definitive input once at the boundary (e.g., config/env readers return a fully‑shaped map) and pass it through without re‑validating.
- Pattern‑match required shapes in function heads instead of branching with `case` inside bodies.
- Normalize types at the consumer (e.g., call `to_bin/1` right before building HTTP queries), not in producers.
- Internal helpers can fail fast on invalid input (let `function_clause`/`badmatch` crash); public API edges should return `{ok, Value} | {error, Reason}`.
- Keep legacy arities thin: read env → construct map → delegate to the Creds‑first arity.
- Do not add catch‑all clauses for `handle_call/3` and `handle_cast/2`; unknown messages should crash. For `handle_info/2`, log and drop.

Do
- Pattern‑match maps in heads: `fun(#{trello_key := Key0, trello_token := Tok0}, Path, QS) -> ... end`.
- Convert types where consumed: `[{<<"key">>, to_bin(Key0)} | ...]`.
- Delegate: `legacy() -> new(get_credentials(), ...)`.

Avoid
- Nested `case`/`if` to validate known shapes inside hot paths.
- Repeating the same validation at every layer.
- Catch‑alls that hide programmer errors.

Before (defensive branching inside)
```erlang
do_get(Creds, Path, QS) ->
    case Creds of
        #{trello_key := K0, trello_token := T0} ->
            Url = build_url(Path, [{<<"key">>, to_bin(K0)}, {<<"token">>, to_bin(T0)} | QS]),
            http_get_json(Url);
        _ -> {error, invalid_credentials}
    end.
```

After (pattern‑match head, simpler body)
```erlang
do_get(#{trello_key := K0, trello_token := T0}, Path, QS) ->
    Url = build_url(Path, [{<<"key">>, to_bin(K0)}, {<<"token">>, to_bin(T0)} | QS]),
    http_get_json(Url).

do_get(Path, QS) ->
    do_get(get_credentials(), Path, QS).
```

Why
- Reduces branching/cognitive load; single source of truth for input construction.
- Surfaces programmer errors early during testing.
- Keeps public surfaces predictable while letting internals stay linear and fast.


