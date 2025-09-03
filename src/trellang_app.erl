%%%-------------------------------------------------------------------
%% @doc trellang public API
%% @end
%%%-------------------------------------------------------------------

-module(trellang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    trellang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
