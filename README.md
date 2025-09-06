trellang
=====

Trello REST client for Erlang/OTP

Build
-----

    $ rebar3 compile

Configuration
-------------
Use a non-committed `dev.config` to provide your Trello API key and token. See `docs/dev.config.example`.

Run
---
- Compile: `rebar3 compile`
- Generate docs: `rebar3 ex_doc` (output in `doc/`, git-ignored)

Getting started
---------------
See the module docs for `trello` (ExDoc HTML) for API reference and examples. For CI setup, extended examples, and contributor/test notes, see `AGENTS.md`.

Examples
--------
Assuming you run with `ERL_FLAGS="-config ./dev.config"` and your `dev.config` contains `board_id` and `list_id`.

Create and update a card
```
%% create
{ok, ListId} = application:get_env(trellang, list_id),
{ok, BoardId} = application:get_env(trellang, board_id),
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
{ok, BoardId} = application:get_env(trellang, board_id),
{ok, ListId} = application:get_env(trellang, list_id),
{ok, Fields} = trello:list_custom_fields(BoardId),
TextId = maps:get(<<"id">>, hd([F || F <- Fields, maps:get(<<"type">>, F, <<>>) =:= <<"text">>])),
{ok, Card} = trello:create_card(ListId, #{name => <<"CF demo">>}),
CardId = maps:get(<<"id">>, Card),
{ok, _} = trello:set_custom_field_text(CardId, TextId, <<"hello-cf">>),
{ok, CardCF} = trello:get_card_with_custom_fields(CardId).
```

Discover lists and dump a board
```
{ok, BoardId} = application:get_env(trellang, board_id),
{ok, Lists} = trello:list_lists(BoardId),
{ok, Dump} = trello:dump_board(BoardId, #{}).
%% Dump = #{ board => #{<<"id">> := _, <<"name">> := _},
%%           lists => [ #{<<"id">> := _, <<"name">> := _, <<"cards">> := [#{<<"id">> := _} | _]} | _ ] }
```
