trellang
=====

Trello REST client for Erlang/OTP

Build
-----

    $ rebar3 compile

Configuration
-------------
trellang reads your Trello credentials from the Erlang application environment.

- Required keys:
  - `trellang.trello_key`
  - `trellang.trello_token`

Recommended: keep credentials in a non‑committed `dev.config` and run with `ERL_FLAGS`. See `docs/dev.config.example`.

Minimal `dev.config` example:
```
[
  {trellang, [
    {trello_key, "..."},
    {trello_token, "..."}
  ]}
].
```

Run with the config file:
```
ERL_FLAGS="-config ./dev.config" rebar3 ct
```

You can also set the values at runtime before the first API call, even after the application is started:
```
application:set_env(trellang, trello_key, "...").
application:set_env(trellang, trello_token, "...").
{ok, Me} = trello:me().
```

Run
---
- Compile: `rebar3 compile`
- Generate docs: `rebar3 ex_doc` (output in `doc/`, git-ignored)

Getting started
---------------
See the module docs for `trello` (ExDoc HTML) for API reference and examples. For CI setup, extended examples, and contributor/test notes, see `AGENTS.md`.

Credentials usage
-----------------
You can call APIs in two ways:

- Env-based (legacy): existing arities read `trellang.trello_key` and `trellang.trello_token` from the application env.
- Explicit credentials: new arities accept a `Creds` map as the first argument: `#{trello_key => <<"KEY">>, trello_token => <<"TOKEN">>}`.

Example (explicit credentials):
```
Creds = #{trello_key => <<"KEY">>, trello_token => <<"TOKEN">>},
{ok, Me} = trello:me(Creds),
{ok, Card} = trello:create_card(Creds, ListId, #{name => <<"Hello">>}).
```

Board mirror server
-------------------
Start a board mirror `gen_server` with explicit credentials and a `board_id`:
```
Creds = #{trello_key => <<"KEY">>, trello_token => <<"TOKEN">>},
{ok, Pid} = trello_board_server:start_link(Creds, BoardId, #{initial_refresh => true}),
ok = trello_board_server:refresh(Pid),
State = trello_board_server:get_state(Pid),
{ok, Card} = trello_board_server:create_card(Pid, ListId, #{name => <<"from-server">>}),
ok = trello_board_server:stop(Pid).
```

Examples
--------
Assumes your key/token are provided via `dev.config`.

How to get your board id:
- Open the board in your browser and copy the 8‑character short link after `/b/` in the URL (e.g., `