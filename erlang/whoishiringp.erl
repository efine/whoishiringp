-module(whoishiringp).

-export([main/0]).
-export([main/1]).

-define(URL_BASE, "https://hacker-news.firebaseio.com/v0").
-define(USER_ID, <<"whoishiring">>).
-define(MAX_CONCURRENCY, (8 * num_cpus())).


-export([get_job/2, num_cpus/0]).

main() ->
    main(?MAX_CONCURRENCY).

main(MaxConcurrency) ->
    inets:start(),
    ssl:start(),
    User = get_user(?URL_BASE, ?USER_ID),
    case maps:get(<<"submitted">>, User) of
        undefined ->
            io:format("No submissions for user ~p\n", [User]);
        Submissions ->
            print_jobs(?URL_BASE, Submissions, MaxConcurrency)
    end.

get_user(UrlBase, UserId) ->
    User = get_url_json(UrlBase, "user", UserId),
    Id = maps:get(<<"id">>, User),
    case Id =:= UserId of
        false ->
            io:format("Expected user id ~p, got ~p, aborting\n", [UserId, Id]),
            erlang:halt(1);
        true ->
            User
    end.

print_jobs(UrlBase, Submissions, MaxConcurrency) ->
    case get_first_matching_submission(UrlBase, Submissions) of
        {ok, Map} ->
            Title = maps:get(<<"title">>, Map, <<"">>),
            Kids = maps:get(<<"kids">>, Map, []),
            io:format("~s\n", [Title]),
            Jobs = pget_jobs(UrlBase, Kids, MaxConcurrency),
            print_jobs(Jobs);
        _Else ->
            ok
    end.


get_first_matching_submission(UrlBase, [Id|Submitted]) ->
    Submission = get_url_json(UrlBase, "item", Id),
    case is_matching_submission(Submission) of
        true->
            {ok, Submission};
        false ->
            get_first_matching_submission(UrlBase, Submitted)
    end;
get_first_matching_submission(_UrlBase, []) ->
    undefined.

is_matching_submission(Submission) ->
    Title = maps:get(<<"title">>, Submission, <<"">>),
    Type = maps:get(<<"type">>, Submission, <<"">>),
    Opts = [{capture, none}, caseless],
    case re:run(to_s(Title), "Ask\\s+HN:\\s+Who\\s+is\\s+hiring", Opts) of
        match ->
            Type =:= <<"story">>;
        _Else ->
            false
    end.

pget_jobs(UrlBase, Kids, MaxConcurrency) ->
    Njobs = length(Kids),
    io:format("Found ~p jobs to search\n", [Njobs]),
    ChunkSize = MaxConcurrency,
    pmap_n({?MODULE, get_job}, [UrlBase], Kids, ChunkSize).

print_jobs(Jobs) ->
    NumberedJobs = lists:enumerate(Jobs),
    [io:format("<h3>#~B:</h3><p>~s</p>\n", [N, Job]) || {N, Job} <- NumberedJobs],
    ok.

make_url(Base, Resource, Id) ->
    to_s(iolist_to_binary([Base, $/, Resource, $/, to_s(Id), ".json"])).

%% Synchronous http call
get_url_json(Base, Resource, Id) ->
    {ok, RequestId} = get_url_json_async(Base, Resource, Id),
    {ok, Response} = receive_response(RequestId, 30000),
    Response.

%% Asynchronous http call
get_url_json_async(Base, Resource, Id) ->
    Url = make_url(Base, Resource, Id),
    Opts = [{body_format, binary}, {sync, false}],
    httpc:request(get, {Url, []}, [], Opts).


receive_response(RequestId, Timeout) ->
    receive
        {http, {RequestId, Result}} ->
            {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
            {ok, json:decode(Body)}
    after
        Timeout ->
            {error, timeout}
    end.


num_cpus() ->
    case erlang:system_info(logical_processors_online) of
        unknown -> 1;
        N -> N
    end.


pmap_n(FuncSpec, ExtraArgs, List, MaxChunk) ->
    pmap_n(FuncSpec, ExtraArgs, List, MaxChunk, [], 0).

pmap_n(_, _, [], _, Results, N) ->
    io:format(standard_error, "\r~8..0B\n", [N]),
    lists:reverse([R || R <- Results, R /= undefined]);
pmap_n(FuncSpec, ExtraArgs, List, MaxChunk, Results, N) ->
    {ToDo, Rest} = safe_split(MaxChunk, List),
    %io:format("~p\n", [{FuncSpec, ExtraArgs, ToDo}]),
    case ToDo of
        [_|_] ->
            Result = safe_rpc_pmap(FuncSpec, ExtraArgs, ToDo),
            NewN = N + length(Result),
            io:format(standard_error, "~8..0B\r", [NewN]),
            pmap_n(FuncSpec, ExtraArgs, Rest, MaxChunk,
                   lists:reverse(Result) ++ Results, NewN);
        [] ->
            pmap_n(FuncSpec, ExtraArgs, Rest, MaxChunk, Results, N)
    end.


safe_rpc_pmap(_FuncSpec, _ExtraArgs, []) ->
    [];
safe_rpc_pmap(FuncSpec, ExtraArgs, L) ->
    try
        rpc:pmap(FuncSpec, ExtraArgs, L)
    catch
        _:badrpc ->
            io:format(standard_error, "\nWarning: pmap failed for ExtraArgs=~p, L=~s\n",
                      [ExtraArgs, [integer_to_list(X) ++ "," || X <- lists:sublist(L, 5)] ++ (if length(L) > 5 -> ["..."]; true -> [] end)]),
            %% Drop one item from list and retry until ok or empty
            safe_rpc_pmap(FuncSpec, ExtraArgs, tl(L))
    end.

safe_split(N, L) when N =< length(L) ->
    lists:split(N, L);
safe_split(_N, L) ->
    {L, []}.


-spec get_job(Id, UrlBase) -> Result when
    Id :: integer(), UrlBase :: binary(), Result :: undefined | string().
get_job(Id, UrlBase) when is_integer(Id) ->
    Kid = get_url_json(UrlBase, "item", Id),
    check_remote_job(get_text(Kid)).


get_text(null) ->
    undefined;
get_text(Kid) ->
    case {maps:get(<<"text">>, Kid, undefined),
          maps:get(<<"deleted">>, Kid, false)} of
        {_, true} ->
            undefined;
        {undefined, _} ->
            io:format("No text in ~p\n", [Kid]),
            undefined;
        {Text, _} ->
            Text
    end.

check_remote_job(undefined) ->
    undefined;
check_remote_job(Text) ->
    Opts = [{capture, none}, caseless],
    case re:run(Text, "remote", Opts) of
        match -> Text;
        _Else -> undefined
    end.


to_s(<<B/bytes>>) ->
    binary_to_list(B);
to_s(I) when is_integer(I) ->
    integer_to_list(I);
to_s(S) when is_list(S) ->
    S.
