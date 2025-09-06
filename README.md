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
