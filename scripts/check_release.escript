#!/usr/bin/env escript
%%! -noshell
%% Read the version as Erlang data, never by evaluating source text.
-mode(compile).

main(Args) ->
    try
        Root = filename:dirname(filename:dirname(filename:absname(escript:script_name()))),
        ok = file:set_cwd(Root),
        {ok, [{application, trellang, Props}]} = file:consult("src/trellang.app.src"),
        Version = proplists:get_value(vsn, Props),
        match = re:run(Version, "^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)$", [{capture, none}]),
        case Args of
            ["--version"] -> io:format("~s~n", [Version]);
            ["--app-source", Path] ->
                {ok, [{application, trellang, Props}]} = file:consult(Path),
                io:format("Package application source valid.~n", []);
            ["--package-metadata", Path] ->
                {ok, Metadata} = file:consult(Path),
                Expected = list_to_binary(Version),
                Expected = proplists:get_value(<<"version">>, Metadata),
                <<"trellang">> = proplists:get_value(<<"name">>, Metadata),
                <<"trellang">> = proplists:get_value(<<"app">>, Metadata),
                io:format("Package metadata valid.~n", []);
            [] -> check_changelog(Version, false), io:format("Release metadata valid: ~s~n", [Version]);
            ["--tag", Tag] ->
                true = Tag =:= "v" ++ Version,
                check_changelog(Version, true),
                <<"tag\n">> = git(["cat-file", "-t", "refs/tags/" ++ Tag]),
                Head = git(["rev-parse", "--verify", "HEAD"]),
                Head = git(["rev-parse", "--verify", "refs/tags/" ++ Tag ++ "^{commit}"]),
                io:format("Release metadata and tag valid: ~s~n", [Tag]);
            _ -> throw(invalid_arguments)
        end
    catch
        _:_ ->
            io:format(standard_error,
                "Release check failed: require a numeric X.Y.Z app version, matching first changelog heading, unique dated/Unreleased entries, and (with --tag) a dated annotated release tag at HEAD.~n", []),
            halt(1)
    end.

check_changelog(Version, RequireDate) ->
    {ok, Bin} = file:read_file("CHANGELOG.md"),
    {match, Matches} = re:run(Bin, "^## v([^\\r\\n]+)$", [global, multiline, {capture, [1], list}]),
    Entries = [parse_entry(Line) || [Line] <- Matches],
    [{Version, Status} | _] = Entries,
    Versions = [V || {V, _} <- Entries],
    true = length(Versions) =:= length(lists:usort(Versions)),
    true = not RequireDate orelse Status =/= "Unreleased",
    ok.

parse_entry(Line) ->
    %% Both a plain hyphen and an em dash are accepted in Markdown headings.
    {match, [Version, Status]} = re:run(Line,
        "^((?:0|[1-9][0-9]*)\\.(?:0|[1-9][0-9]*)\\.(?:0|[1-9][0-9]*)) (?:-|\xE2\x80\x94) (Unreleased|[0-9]{4}-[0-9]{2}-[0-9]{2})$",
        [{capture, [1, 2], list}]),
    case Status of
        "Unreleased" -> ok;
        _ ->
            [Year, Month, Day] = [list_to_integer(N) || N <- string:split(Status, "-", all)],
            true = calendar:valid_date(Year, Month, Day)
    end,
    {Version, Status}.

git(Args) ->
    Port = open_port({spawn_executable, os:find_executable("git")},
                     [binary, exit_status, use_stdio, stderr_to_stdout, {args, Args}]),
    git_result(Port, <<>>).

git_result(Port, Acc) ->
    receive
        {Port, {data, Data}} -> git_result(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, 0}} -> Acc;
        {Port, {exit_status, _}} -> throw(git_failed)
    after 10000 -> port_close(Port), throw(git_timeout)
    end.
