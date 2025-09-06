%% Internal application module; hidden from generated docs
-module(trellang_app).
-moduledoc false.

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    trellang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
