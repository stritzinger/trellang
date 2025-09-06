trellang
=====

An OTP application

Build
-----

    $ rebar3 compile

Configuration (non-committed dev.config)
----------------------------------------
Place Trello credentials and IDs in an Erlang config file that loads into the application environment.

1) Create `dev.config` (do not commit) based on the example in `docs/dev.config.example`:

```
[
  {trellang,
    [
      {trello_key, "..."},
      {trello_token, "..."},
      {board_id, "..."},
      {list_id, "..."},
      % optional custom field ids on your test board
      {cf_id_text, undefined},
      {cf_id_number, undefined},
      {cf_id_checkbox, undefined},
      {cf_id_date, undefined},
      {cf_id_list, undefined},
      % archive/delete test cards after each test when true
      {test_cleanup, false}
    ]}
].
```

2) Run with this config (rebar3 honors ERL_FLAGS to pass VM args):

```
$ ERL_FLAGS="-config ./dev.config" rebar3 eunit
$ ERL_FLAGS="-config ./dev.config" rebar3 ct
```

Obtaining Trello credentials
----------------------------
- API key: visit `https://trello.com/app-key` while logged into Trello to retrieve your key.
- Token: from the same page, generate a token for your account (read/write as needed).
- Reference docs: `https://developer.atlassian.com/cloud/trello/guides/rest-api/authorization/`

CI setup (GitHub Actions example)
---------------------------------
Store the contents of `dev.config` in a repository secret (e.g., `TRELLO_DEV_CONFIG`) and write it to a file at runtime, then pass it via `ERL_FLAGS`:

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
      - name: Write dev.config
        run: |
          printf "%s" "${{ secrets.TRELLO_DEV_CONFIG }}" > dev.config
      - name: Unit tests
        run: ERL_FLAGS="-config ./dev.config" rebar3 eunit
      - name: Common Test (integration)
        run: ERL_FLAGS="-config ./dev.config" rebar3 ct
```

Notes
-----
- Do not commit `dev.config`. Add it to your local `.gitignore`.
- Board/list and custom field IDs can be obtained from the Trello UI or via the API using your key/token.

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
%% pick a text/date/checkbox field ids as present on your board
TextId = maps:get(<<"id">>, hd([F || F <- Fields, maps:get(<<"type">>, F, <<>>) =:= <<"text">>])),
DateId = maps:get(<<"id">>, hd([F || F <- Fields, maps:get(<<"type">>, F, <<>>) =:= <<"date">>])),
CheckboxId = maps:get(<<"id">>, hd([F || F <- Fields, maps:get(<<"type">>, F, <<>>) =:= <<"checkbox">>])),

{ok, Card} = trello:create_card(ListId, #{name => <<"CF demo">>}),
CardId = maps:get(<<"id">>, Card),

{ok, _} = trello:set_custom_field_text(CardId, TextId, <<"hello-cf">>),
{ok, _} = trello:set_custom_field_date(CardId, DateId, <<"2031-12-24T12:34:56.000Z">>),
{ok, _} = trello:set_custom_field_checkbox(CardId, CheckboxId, true),

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
