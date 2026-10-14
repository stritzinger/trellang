-module(trello_board_server_offline_SUITE).
-moduledoc false.

-include_lib("stdlib/include/assert.hrl").

-export([suite/0, all/0, initial_refresh_failure_propagates/1,
         requested_refresh_failure_propagates/1, periodic_refresh_failure_propagates/1]).

suite() -> [{timetrap, {seconds, 10}}].

all() -> [initial_refresh_failure_propagates, requested_refresh_failure_propagates,
          periodic_refresh_failure_propagates].

%% Invalid explicit credentials fail in trello's boundary pattern match, before
%% any HTTP request. Refresh already propagates these failures; it does not
%% return an error reply or retain a usable stale state.
initial_refresh_failure_propagates(_Config) ->
    ?assertException(error, function_clause,
        trello_board_server:init({#{}, <<"unused-board">>, #{initial_refresh => true}})).

requested_refresh_failure_propagates(_Config) ->
    {ok, State} = trello_board_server:init({#{}, <<"unused-board">>, #{}}),
    ?assertException(error, function_clause,
        trello_board_server:handle_call(refresh, undefined, State)).

periodic_refresh_failure_propagates(_Config) ->
    {ok, State} = trello_board_server:init({#{}, <<"unused-board">>, #{}}),
    ?assertException(error, function_clause,
        trello_board_server:handle_info(refresh_tick, State)).
