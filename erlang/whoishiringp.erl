-module(whoishiringp).

-export([main/0]).
-export([main/1]).

-define(URL_BASE, "https://hacker-news.firebaseio.com/v0").
-define(USER_ID, <<"whoishiring">>).
-define(MAX_CONCURRENCY, (8 * num_cpus())).


-export([get_job/2, num_cpus/0]).

main() ->
    main(?MAX_CONCURRENCY).

%% Entry point
-spec main(MaxConcurrency :: pos_integer()) -> ok.

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

%% Fetch user and verify id
-spec get_user(UrlBase :: binary(), UserId :: binary()) -> map().

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

%% Print jobs from first matching submission
-spec print_jobs(UrlBase :: binary(), Submissions :: [integer()], MaxConcurrency :: pos_integer()) -> ok.

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


%% Get first submission matching "Ask HN: Who is hiring"
-spec get_first_matching_submission(UrlBase :: binary(), Submitted :: [integer()]) -> {ok, map()} | undefined.

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

%% Check if submission title matches "Ask HN: Who is hiring" and type is "story"
-spec is_matching_submission(Submission :: map()) -> boolean().

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

%% Get jobs in parallel with max concurrency
-spec pget_jobs(UrlBase :: binary(), Kids :: [integer()], MaxConcurrency :: pos_integer()) -> [string()].

pget_jobs(UrlBase, Kids, MaxConcurrency) ->
    Njobs = length(Kids),
    io:format(standard_error, "Found ~p jobs to search\n", [Njobs]),
    ChunkSize = MaxConcurrency,
    pmap_n({?MODULE, get_job}, [UrlBase], Kids, ChunkSize).

%% Print jobs as numbered list of Unicode strings. Output is HTML.
-spec print_jobs(Jobs :: [string()]) -> ok.

print_jobs(Jobs) ->
    NumberedJobs = lists:enumerate(Jobs),
    [io:format("<h3>#~B:</h3><p>~ts</p>\n", [N, Job]) || {N, Job} <- NumberedJobs],
    ok.

%% Construct URL.json for given base, resource and id
-spec make_url(Base, Resource, Id) -> binary() when
              Base :: binary(),
              Resource :: binary(),
              Id :: integer() | binary().

make_url(Base, Resource, Id) ->
    to_s(iolist_to_binary([Base, $/, Resource, $/, to_s(Id), ".json"])).

%% Synchronous http call
%% Wrapper around asynchronous call with receive and timeout
%% Returns {ok, Map}
-spec get_url_json(Base, Resource, Id) -> map() when
              Base :: binary(),
              Resource :: binary(),
              Id :: integer() | binary().

get_url_json(Base, Resource, Id) ->
    {ok, RequestId} = get_url_json_async(Base, Resource, Id),
    {ok, Response} = receive_response(RequestId, 30000),
    Response.

%% Asynchronous http call
%% Returns {ok, RequestId}
-spec get_url_json_async(Base, Resource, Id) -> {ok, reference()} when
              Base :: binary(),
              Resource :: binary(),
              Id :: integer() | binary().

get_url_json_async(Base, Resource, Id) ->
    Url = make_url(Base, Resource, Id),
    Opts = [{body_format, binary}, {sync, false}],
    httpc:request(get, {Url, []}, [], Opts).

%% Wait for the asynchronous JSON response with a timeout
-spec receive_response(RequestId, Timeout) -> {ok, map()} | {error, timeout} when
              RequestId :: reference(),
              Timeout :: non_neg_integer().

receive_response(RequestId, Timeout) ->
    receive
        {http, {RequestId, Result}} ->
            {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
            {ok, json:decode(Body)}
    after
        Timeout ->
            {error, timeout}
    end.

%% Get number of CPUs, default to 1 if unknown
-spec num_cpus() -> pos_integer().

num_cpus() ->
    case erlang:system_info(logical_processors_online) of
        unknown -> 1;
        N -> N
    end.

%%  Parallel map with a maximum chunk size to avoid overloading the rpc server
-spec pmap_n(FuncSpec, ExtraArgs, List, MaxChunk) -> List2 when
    FuncSpec :: {Module, Function},
    Module :: module(),
    Function :: atom(),
    ExtraArgs :: [term()],
    List :: [Elem :: term()],
    MaxChunk :: pos_integer(),
    List2 :: [term()].

pmap_n(FuncSpec, ExtraArgs, List, MaxChunk) ->
    pmap_n(FuncSpec, ExtraArgs, List, MaxChunk, [], 0).

%% Accumulator version with results and count
-spec pmap_n(FuncSpec, ExtraArgs, List, MaxChunk, Results, N) -> List2 when
    FuncSpec :: {Module, Function},
    Module :: module(),
    Function :: atom(),
    ExtraArgs :: [term()],
    List :: [Elem :: term()],
    MaxChunk :: pos_integer(),
    Results :: [term()],
    N :: non_neg_integer(),
    List2 :: [term()].

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

%%  Wrapper around rpc:pmap/3 that retries on badrpc by dropping one item from the list
%%  until it succeeds or the list is empty.
-spec safe_rpc_pmap(FuncSpec, ExtraArgs, List1) -> List2
              when
                  FuncSpec :: {Module, Function},
                  Module :: module(),
                  Function :: atom(),
                  ExtraArgs :: [term()],
                  List1 :: [Elem :: term()],
                  List2 :: [term()].

safe_rpc_pmap(_FuncSpec, _ExtraArgs, []) ->
    [];
safe_rpc_pmap(FuncSpec, ExtraArgs, L) ->
    try
        rpc:pmap(FuncSpec, ExtraArgs, L)
    catch
        _:badrpc ->
            io:format(standard_error, "\nWarning: pmap failed for ExtraArgs=~p, L=~s\n",
                      [ExtraArgs, [integer_to_list(X) ++ "," || X <- lists:sublist(L, 5)] 
                      ++ (if length(L) > 5 -> ["..."]; true -> [] end)]),
            %% Drop one item from list and retry until ok or empty
            safe_rpc_pmap(FuncSpec, ExtraArgs, tl(L))
    end.

%% Safe split that doesn't fail if N > length(L)
-spec safe_split(N, L) -> {Front, Back} when
    N :: non_neg_integer(),
    L :: [term()],
    Front :: [term()],
    Back :: [term()].
safe_split(N, L) when N =< length(L) ->
    lists:split(N, L);
safe_split(_N, L) ->
    {L, []}.


-spec get_job(Id, UrlBase) -> Result when
    Id :: integer(), UrlBase :: binary(), Result :: undefined | string().
get_job(Id, UrlBase) when is_integer(Id) ->
    Kid = get_url_json(UrlBase, "item", Id),
    check_remote_job(get_text(Kid)).


%% Extract text from item, return undefined if deleted or no text
-spec get_text(Kid) -> undefined | string() when
    Kid :: map().

get_text(null) ->
    undefined;
get_text(Kid) ->
    case {maps:get(<<"text">>, Kid, undefined),
          maps:get(<<"deleted">>, Kid, false)} of
        {_, true} ->
            undefined;
        {undefined, _} ->
            io:format(standard_error, "No text in ~p\n", [Kid]),
            undefined;
        {Text, _} ->
            Text
    end.

%% Check if text contains "remote" (case insensitive), return text or undefined
-spec check_remote_job(Text) -> undefined | string() when
    Text :: undefined | string().

check_remote_job(undefined) ->
    undefined;
check_remote_job(Text) ->
    Opts = [{capture, none}, caseless],
    case re:run(Text, "remote", Opts) of
        match -> Text;
        _Else -> undefined
    end.

%% Convert binary or integer to string (list of characters)
-spec to_s(B | I | S) -> string() when
    B :: binary(),
    I :: integer(),
    S :: string().

to_s(<<B/bytes>>) ->
    binary_to_list(B);
to_s(I) when is_integer(I) ->
    integer_to_list(I);
to_s(S) when is_list(S) ->
    S.
