trellang
=====

Trello REST client for Erlang/OTP 27 or later.

Published versions and dates are listed in the [changelog](CHANGELOG.md).
GitHub may contain an unreleased version; [Hex](https://hex.pm/packages/trellang)
and [HexDocs](https://hexdocs.pm/trellang/) describe published releases.

Build
-----

    $ rebar3 compile

Configuration
-------------
trellang reads your Trello credentials from the Erlang application environment.

- Required keys:
  - `trellang.trello_key`
  - `trellang.trello_token`

Recommended: keep credentials in a non‑committed `dev.config` and run with `ERL_FLAGS`. See the [configuration example](https://github.com/stritzinger/trellang/blob/main/docs/dev.config.example).

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
See the module docs for `trello` (ExDoc HTML) for API reference and examples. For CI setup and contributor/test notes, see [AGENTS.md](AGENTS.md).
Maintainers publish from development machines using the [release procedure](docs/releases.md).

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
- Open the board in your browser and copy the 8‑character short link after `/b/` in the URL (e.g., `abcd1234`). Trello accepts this short link wherever a board id is required, or you can resolve the full id via `GET /1/boards/{shortLink}?fields=id`.

Create and update a card
```
%% board id (short link or full id)
BoardId = <<"abcd1234">>,

%% pick a list on that board
{ok, Lists} = trello:list_lists(BoardId),
ListId = maps:get(<<"id">>, hd(Lists)),

%% create
{ok, Card} = trello:create_card(ListId, #{name => <<"Hello Trello">>}),
CardId = maps:get(<<"id">>, Card),

%% set desc and due
{ok, _} = trello:set_desc(CardId, <<"Created from Erlang">>),
{ok, _} = trello:set_due(CardId, <<"2031-01-02T03:04:05.000Z">>),

%% add a green label by color
{ok, _} = trello:add_label_by_color(BoardId, CardId, <<"green">>),

%% fetch card
{ok, Card2} = trello:get_card(CardId).
```

Custom fields (text/date/checkbox)
```
BoardId = <<"abcd1234">>,
{ok, Lists} = trello:list_lists(BoardId),
ListId = maps:get(<<"id">>, hd(Lists)),
{ok, Fields} = trello:list_custom_fields(BoardId),
TextId = maps:get(<<"id">>, hd([F || F <- Fields, maps:get(<<"type">>, F, <<>>) =:= <<"text">>])),
{ok, Card} = trello:create_card(ListId, #{name => <<"CF demo">>}),
CardId = maps:get(<<"id">>, Card),
{ok, _} = trello:set_custom_field_text(CardId, TextId, <<"hello-cf">>),
{ok, CardCF} = trello:get_card_with_custom_fields(CardId).
```

Discover lists and dump a board
```
BoardId = <<"abcd1234">>,
{ok, Lists} = trello:list_lists(BoardId),
{ok, Dump} = trello:dump_board(BoardId, #{}).
%% Dump = #{ board => #{<<"id">> := _, <<"name">> := _},
%%           lists => [ #{<<"id">> := _, <<"name">> := _, <<"cards">> := [#{<<"id">> := _} | _]} | _ ] }
```
