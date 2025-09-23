-module(trello_board_server).

-behaviour(gen_server).

-moduledoc """
GenServer that mirrors a Trello board in memory.

Start the server with explicit credentials and a board id. You can request an on-demand
refresh to fetch lists, cards, and checklists (flattened into state). Optionally enable
periodic refresh via `refresh_interval` option (milliseconds).

State shape returned by `get_state/1`:

```
#{ board_id := binary()
 , lists := [ListMap]
 , cards := [CardMap]
 , checklists := [ChecklistMap]
 , last_sync := integer() | undefined
 }
```
""".

-export([
    start_link/3,
    stop/1,
    refresh/1,
    get_state/1,
    create_card/3,
    update_card/3,
    add_check_item/4
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    creds :: map(),
    board_id :: binary(),
    refresh_interval :: non_neg_integer() | disabled,
    refresh_timer_ref :: reference() | undefined,
    last_sync :: integer() | undefined,
    lists :: [map()],
    cards :: [map()],
    checklists :: [map()]
}).

-type options() :: #{ refresh_interval => non_neg_integer() | disabled } | list().

-doc """
Start the board server.

Options:
- `refresh_interval` (ms | `disabled`, default `disabled`)
- If you want an initial refresh at init, pass `{initial_refresh, true}` in map options.
""".
start_link(Creds, BoardId0, Options) ->
    BoardId = to_bin(BoardId0),
    gen_server:start_link(?MODULE, {Creds, BoardId, normalize_opts(Options)}, []).

-doc """
Stop the server.
""".
stop(Pid) ->
    gen_server:stop(Pid).

-doc """
Trigger a synchronous refresh.
""".
refresh(Pid) ->
    gen_server:call(Pid, refresh, 60000).

-doc """
Get the mirrored state (board id, lists, cards, checklists, last_sync).
""".
get_state(Pid) ->
    gen_server:call(Pid, get_state).

-doc """
Create a card on a list via the server; updates mirror on success.
Returns `{ok, Card}`.
""".
create_card(Pid, ListId, Fields) when is_map(Fields) ->
    gen_server:call(Pid, {create_card, ListId, Fields}, 60000).

-doc """
Update a card via the server; updates mirror on success.
Returns `{ok, Card}`.
""".
update_card(Pid, CardId, Fields) when is_map(Fields) ->
    gen_server:call(Pid, {update_card, CardId, Fields}, 60000).

-doc """
Add a check item to a checklist. `Pos` can be a number or `top`/`bottom`.
Returns `{ok, CheckItem}`.
""".
add_check_item(Pid, ChecklistId, Name, Pos) ->
    gen_server:call(Pid, {add_check_item, ChecklistId, Name, Pos}, 60000).

%% gen_server

init({Creds, BoardId, Opts}) ->
    RefreshInterval = maps:get(refresh_interval, Opts, disabled),
    InitialState = #state{ creds = Creds
                         , board_id = BoardId
                         , refresh_interval = RefreshInterval
                         , refresh_timer_ref = undefined
                         , last_sync = undefined
                         , lists = []
                         , cards = []
                         , checklists = [] },
    State1 = maybe_initial_refresh(InitialState, Opts),
    State2 = ensure_timer(State1),
    {ok, State2}.

handle_call(get_state, _From, S = #state{}) ->
    {reply, state_to_map(S), S};
handle_call(refresh, _From, S0) ->
    case do_refresh(S0) of
        {ok, S1} -> {reply, ok, S1};
        {error, _}=E -> {reply, E, S0}
    end;
handle_call({create_card, ListId0, Fields}, _From, S0 = #state{creds = Creds}) ->
    ListId = to_bin(ListId0),
    case trello:create_card(Creds, ListId, Fields) of
        {ok, Card} ->
            {ok, S1} = do_refresh(S0),
            {reply, {ok, Card}, S1};
        {error, _}=E -> {reply, E, S0}
    end;
handle_call({update_card, CardId0, Fields}, _From, S0 = #state{creds = Creds}) ->
    CardId = to_bin(CardId0),
    case trello:update_card(Creds, CardId, Fields) of
        {ok, Card} ->
            {ok, S1} = do_refresh(S0),
            {reply, {ok, Card}, S1};
        {error, _}=E -> {reply, E, S0}
    end;
handle_call({add_check_item, ChecklistId0, Name0, Pos0}, _From, S0 = #state{creds = Creds}) ->
    ChecklistId = to_bin(ChecklistId0),
    Name = to_bin(Name0),
    Opts = case Pos0 of undefined -> #{}; P -> #{pos => P} end,
    case trello:add_check_item(Creds, ChecklistId, Name, Opts) of
        {ok, Item} ->
            {ok, S1} = do_refresh(S0),
            {reply, {ok, Item}, S1};
        {error, _}=E -> {reply, E, S0}
    end.

handle_cast(_Msg, _State) ->
    %% No casts are implemented yet; fail fast on accidental usage
    erlang:error(badcast).

handle_info(refresh_tick, S0) ->
    case do_refresh(S0) of
        {ok, S1} -> {noreply, ensure_timer(S1)};
        {error, _} -> {noreply, ensure_timer(S0)}
    end;
handle_info(Info, S) ->
    logger:debug("Unhandled info: ~p", [Info]),
    {noreply, S}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal

maybe_initial_refresh(S0, Opts) ->
    case maps:get(initial_refresh, Opts, false) of
        true ->
            case do_refresh(S0) of
                {ok, S1} -> S1;
                {error, _} -> S0
            end;
        _ -> S0
    end.

ensure_timer(S = #state{refresh_interval = disabled, refresh_timer_ref = TRef}) ->
    _ = maybe_cancel_timer(TRef),
    S#state{refresh_timer_ref = undefined};
ensure_timer(S = #state{refresh_interval = Interval}) when is_integer(Interval), Interval > 0 ->
    _ = maybe_cancel_timer(S#state.refresh_timer_ref),
    Ref = erlang:send_after(Interval, self(), refresh_tick),
    S#state{refresh_timer_ref = Ref}.

maybe_cancel_timer(undefined) -> ok;
maybe_cancel_timer(Ref) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

do_refresh(S0 = #state{creds = Creds, board_id = BoardId}) ->
    case trello:dump_board(Creds, BoardId, #{include_checklists => true}) of
        {ok, Dump} ->
            Lists = maps:get(lists, Dump, []),
            Cards = lists:flatmap(fun(L) -> maps:get(<<"cards">>, L, []) end, Lists),
            Checklists = lists:flatmap(
                fun(C) -> maps:get(<<"checklists">>, C, []) end, Cards),
            TS = erlang:system_time(millisecond),
            {ok, S0#state{ lists = Lists, cards = Cards, checklists = Checklists, last_sync = TS }};
        {error, _}=E -> E
    end.

state_to_map(#state{board_id = B, lists = Ls, cards = Cs, checklists = CLs, last_sync = TS}) ->
    #{ board_id => B, lists => Ls, cards => Cs, checklists => CLs, last_sync => TS }.

normalize_opts(Opts) when is_map(Opts) ->
    Opts;
normalize_opts(Opts) when is_list(Opts) ->
    maps:from_list(Opts);
normalize_opts(_) ->
    #{}.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(I) when is_integer(I) -> integer_to_binary(I).


