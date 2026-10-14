#!/usr/bin/env escript
%%! -noshell
%% This helper deliberately never prints consulted data or exception reasons.
-mode(compile).

main(Args) ->
    try
        [Destination | Source] = Args,
        Props = read_props(Source),
        Keys = [trello_key, trello_token, board_id, list_id],
        true = lists:all(fun(Key) -> nonempty(proplists:get_value(Key, Props)) end, Keys),
        Allowed = Keys ++ [cf_id_text, cf_id_number, cf_id_checkbox, cf_id_date, cf_id_list, member_id],
        Selected = [{K, proplists:get_value(K, Props)} || K <- Allowed,
                    proplists:is_defined(K, Props)],
        Config = [{trellang, [{test_cleanup, true} | Selected]}],
        {ok, File} = file:open(Destination, [write, exclusive, {encoding, utf8}]),
        ok = file:change_mode(Destination, 8#600),
        ok = io:format(File, "~tp.~n", [Config]),
        ok = file:close(File)
    catch
        _:_ -> io:format(standard_error, "Private test configuration unavailable or invalid.~n", []), halt(1)
    end.

read_props([]) ->
    [{trello_key, os:getenv("TRELLO_KEY")}, {trello_token, os:getenv("TRELLO_TOKEN")},
     {board_id, os:getenv("TRELLO_BOARD_ID")}, {list_id, os:getenv("TRELLO_LIST_ID")}];
read_props([Path]) ->
    {ok, [Config]} = file:consult(Path),
    Props = proplists:get_value(trellang, Config),
    true = is_list(Props),
    Props.

nonempty(Value) when is_list(Value), Value =/= [] -> lists:all(fun is_integer/1, Value);
nonempty(Value) when is_binary(Value), byte_size(Value) > 0 -> true;
nonempty(_) -> false.
