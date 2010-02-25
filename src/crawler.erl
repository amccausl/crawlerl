%%% @author Alex McCausland <alex.mccausland@gmail.com>

%% Data Types:
%%
%% Type			= crawl | save | rewrite
%% Translation	= {CompiledRegex, Atoms, CompiledTemplates}
%% Rule			= {Type, Name, Regex, Templates}
%%				= {Type, Name, Regex, Templates, Params}
%% RuleProcesses = [{Translation, PID}]
%% CompiledRule	= {Type, Name, Translation}
%% MatchedRule	= {Type, Name, Strings}
%% Request		= {[Id], Url, Translations}
%% Id			= {RuleName, PID, Url}
%% Response 	= {Url, [Filename]}
%%				= {Url, [Response]}

%% Actors:
%%
%% router:
%%	Incoming:	rule_actor			=>	{url, Url}, {request, Request}
%%	Outgoing:	{request, Request}	=>	rule_manager
%%
%% rule_actor:
%%	Incoming:	rule_actor						=> {callback, Request, Response}
%%	Outgoing:	{callback, Request, Response}	=> rule_actor

%% TODO
% TODO: finalize message signatures
% TODO: add index file handling
% TODO: should store each processed url in each thread
% TODO: on launch, should check process cache, then translated filename (if local) to determine if fetch has already occured
% TODO: add caching in rule_manager
% TODO: rewrite directory requests to use index.html (should happen on rule end)
% TODO: add parameters to rules
% TODO: examine http methods to find a way to abort a download from examining the headers
% TODO: consider changing methods around to allow $ext$ tag harvested from headers
% TODO: add parameter for allowed depth, cycling detection or url regex param on crawl rule (use template) 

-module( crawler ).
-export( [ start/0, stop/0, router/1, uri_parse/1, html_extract_urls/2, url_makeAbsolute/2 ] ).
% expose methods for testing
-export( [rule_manager/4, rule_actor/2, find_rule/2, templates_run/2] ).

% start with url on cmd line, maybe some preprocessing
start() ->
	init().
	%{ok, URLs} = init:get_arguments( url ),
	%route(router, URLs).

stop() ->
	router ! {stop}.

init() ->
	{ok, Rules} = crawler_config:load_rules(),
	RuleProcesses = rule_manager_spawn( Rules, [] ),
	PID = spawn( crawler, router, [RuleProcesses] ),
	register(router, PID),
	% TODO: configure http client
	ibrowse:start().

router(RuleProcesses) ->
	io:format("router process '~w' waiting...~n", [erlang:self()]),
% All requests go to the router first
% The router looks up the rule, runs the translation, generates the Rule request and sends it to the rule manager.
	receive
		{request, {Ids, Url}} ->
			case find_rule( RuleProcesses, Url ) of
				nomatch ->
					io:format("No rule for url: ~s/n", [Url]),
					router(RuleProcesses);
				{ok, PID, Translations} ->
					io:format("Matched '~s' to rule~n", [Url]),
					PID ! {request, {Ids, Url, Translations}},
					router(RuleProcesses)
			end;
		{status} ->
			io:format("query router for status~n"),
			lists:map(fun(X) -> PID = element(2, X), PID ! {status} end, RuleProcesses),
			router( RuleProcesses );
		{stop} ->
			lists:map(fun(X) -> element(2, X) ! {stop} end, RuleProcesses),
			exit({ok})
	end.

rule_manager_spawn( [], AccRules ) ->
	lists:reverse(AccRules);
rule_manager_spawn( [Rule|Rules], AccRules ) ->
	{Type, Name, Translation} = Rule,
	PID = spawn( crawler, rule_manager, [{Type, Name}, [], [], 5]),
	rule_manager_spawn( Rules, [{Translation, PID}|AccRules] ).

rule_manager( Rule, [Request|Queue], Threads, ThreadCount) when ThreadCount > 0 ->
	io:format("rule_manager: ~s, with ~B threads left~n", [element(2, Rule), ThreadCount]),
	{Ids, Url, Translation} = Request,
	Id = {element(2, Rule), erlang:self(), Url},
	PID = spawn( crawler, rule_actor, [Rule, {[Id|Ids], Url, Translation}] ),
	erlang:monitor(process, PID),
	rule_manager( Rule, Queue, [{PID, Request}|Threads], ThreadCount - 1 );
rule_manager( Rule, Queue, Threads, ThreadCount) ->
	io:format("rule_manager: ~s, waiting with ~B threads left~n", [element(2, Rule), ThreadCount]),
	receive
		{request, Request} ->
			io:format("rule_manager '~w' called with {request, ~w}~n", [element(2, Rule), Request]),  
			rule_manager( Rule, [Request|Queue], Threads, ThreadCount);
		{'DOWN', _, process, Pid, Reason} ->
			case Reason of
				{ok, {[], _, _}, Response} ->
					io:format("Thread '~s:~w' finished~n~w~n", [element(2, Rule), erlang:self(), Response]);
				{ok, {[Id|Ids], Url, Translations}, Response} ->
					PID = erlang:element(2, Id),
					PID ! {ok, {Ids, Url, Translations}, Response};
				_ ->
					io:format("rule_actor '~s:~w' failed: ~w~n", [element(2, Rule), Pid, Reason])
			end,
			rule_manager( Rule, Queue, lists:keydelete(Pid, 1, Threads), ThreadCount + 1 );
		{status} ->
			io:format("Rule: ~w~n~w~n", [Rule, Threads]),
			rule_manager( Rule, Queue, Threads, ThreadCount );
		{stop} ->
			io:format("stopping rule_manager '~s:~w' ~B threads~n", [element(2, Rule), erlang:self(), ThreadCount]),
			lists:map(fun(X) -> exit(element(1, X), kill) end, Threads)
	end.

rule_actor( {crawl, Name}, Request ) ->
	io:format("running rule_actor '~s:~w'~n", [Name, erlang:self()]),
	{_, Url, Filenames} = Request,
	%{Protocol, } = uri_parse( Uri ),
	case fetch(Url) of
		{ok, Body} ->
			RelativeUrls = html_extract_urls(Body, []),
			Urls		 = lists:map(fun(X) -> url_makeAbsolute(Url, X) end, RelativeUrls),
			%io:format( "following urls: ~w~n", [Urls] ),
			Result 		 = rule_spawn_and_collect( Request, Urls ),
			ResultString = response_to_string(Result),
			lists:map(fun(X) -> file:write_file(X, ResultString) end, Filenames),
			exit(Result);
		{fail, Reason} ->
			io:format("fetch failed for ~s with reason: ~w~n", [Url, Reason]),
			exit({ok, Request, {Url, []}})
	end;
rule_actor( {save, Name}, Request ) ->
	{_, Url, Filenames} = Request,
	io:format("running rule_actor '~s:~w' on url ~s~n", [Name, erlang:self(), Url]),
	case fetch_file( Url ) of
		{ok, FetchedFilename} ->
			lists:map(fun(Filename) ->
				io:format("creating file ~s~n", [Filename]),
				{_, RevDirectory} = partition($/, lists:reverse(Filename)),
				filelib:ensure_dir(lists:reverse(RevDirectory)),
				file:copy(FetchedFilename, Filename) end, Filenames),
			file:delete(FetchedFilename),
			io:format("rule returning result: {~s, ~s}~n", [Url, Filenames]),
			exit({ok, Request, {Url, Filenames}});
		{fail, Reason} ->
			io:format("fetch failed for url '~s': ~s~n", [Url, Reason]),
			exit({ok, Request, {Url, []}})
	end;
rule_actor( {rewrite, Name}, Request ) ->
	io:format("running rule_actor '~s:~w'~n", [Name, erlang:self()]),
	Translations = element(3, Request),
	exit(rule_spawn_and_collect( Request, Translations )).

rule_spawn_and_collect( Request, Urls ) ->
	{ok, Responses, Count}	= rule_spawner( Request, Urls, [], 0 ),
	PopulatedResponses		= rule_response_collector( Responses, Count ),
	{ok, Request, PopulatedResponses}.
rule_spawner( _, [], Responses, Count ) ->
	io:format( "finished swaping requests, waiting for responses ~w~n", [Responses] ),
	{ok, Responses, Count};
rule_spawner( Request, [Url|Urls], Responses, Count ) ->
	io:format("rule_spawner: creating request for ~s~n", [Url]),
	{[{Name, _, _}|_], _, _} = Request,
	NewId = [{Name, erlang:self(), Url}|Request],
	% TODO: add in cycling detection for crawlers (examine id for occurrences of the same rule, if exists, spawn thread to resolve)
	router ! { request, {NewId, Url} },
	rule_spawner( Request, Urls, [{Url}|Responses], Count + 1 ).
rule_response_collector( Responses, 0 ) ->
	Responses;
rule_response_collector( Responses, Outstanding ) ->
%% Collect the result of requests for Urls
	io:format( "waiting for responses~n" ),
	receive
		{ok, Request, Response} ->
			io:format( "response received ~w~n", [Response] ),
			{_, Url, _} = Request, 
			NewResponses = lists:keyreplace(Url, 1, Responses, Response),
			rule_response_collector( NewResponses, Outstanding - 1 )
	end.

% Crawl helpers
fetch(Url) ->
	io:format("fetching ~s~n", [Url]),
	case ibrowse:send_req(Url, [], get) of
		{ok, "200", _, Body} ->
		%{ok, "200", Headers, Body} ->
			%io:format("fetch '~s' => '~w'~n", [Url, Headers]),
			{ok, Body};
		{ok, "301", Headers, _} ->
			%io:format("fetch '~s' => '~w'~n", [Url, Headers]),
			case lists:keyfind("Location", 1, Headers) of
				false ->
					io:format("missing Location header on 301 response~n"),
					{fail, "301 response, no location"};
				{_, Location} ->
					io:format("redirecting fetch to ~s~n", [Location]),
					fetch(Location);
				A ->
					io:format("match failed for ~w~n", [A]),
					{fail, "Match failed"}
			end;
		Msg ->
			io:format("failed with '~w'~n", [Msg]),
			{fail, Msg}
	end.

fetch_file(Url) ->
	io:format("fetch: ~s to file~n", [Url]),
	case ibrowse:send_req(Url, [], get, [], [{save_response_to_file, true}]) of
		{ok, "200", _, {file, SavedFile}} ->
		%{ok, "200", Headers, Body} ->
			%io:format("fetch '~s' => '~w'~n", [Url, Headers]),
			{ok, SavedFile};
		{ok, "301", Headers, {file, SavedFile}} ->
			file:delete(SavedFile),
			%io:format("fetch '~s' => '~w'~n", [Url, Headers]),
			case lists:keyfind("Location", 1, Headers) of
				false ->
					io:format("missing Location header on 301 response~n"),
					{fail, "301 response, no location"};
				{_, Location} ->
					io:format("redirecting fetch to ~s~n", [Location]),
					fetch_file(Location);
				A ->
					io:format("match failed for ~w~n", [A]),
					{fail, "Match failed"}
			end;
		Msg ->
			io:format("failed with '~w'~n", [Msg]),
			{fail, Msg}
	end.

%process_page( { ok, {_Status, _Headers, Body }} ) ->
%	lists:subtract(lists:subtract(Body, "// [ "), "] ");
%process_page( {error,no_scheme} ) ->
%	io:format( "No scheme error" ).

parse_quoted(Input) ->
	[Quote|Tail] = Input,
	Quoted = lists:takewhile(fun(X) -> X =/= Quote end, Tail),
	{Quoted, lists:nthtail(length(Quoted), Tail)}.
html_extract_urls( [], Urls ) ->
	lists:reverse(Urls);
html_extract_urls( HTML, Urls ) ->
	% TODO: should be case insensitive
	case lists:prefix("src=", HTML) of
		true ->
			{Url, Remaining} = parse_quoted(lists:nthtail(4, HTML)),
			html_extract_urls(Remaining, [Url|Urls]);
		false ->
			case lists:prefix("href=", HTML) of
				true ->
					{Url, Remaining} = parse_quoted(lists:nthtail(5, HTML)),
					html_extract_urls(Remaining, [Url|Urls]);
				false ->
					[_|T] = HTML,
					html_extract_urls(T, Urls)
			end
	end.

-spec(url_makeAbsolute/2 :: (string(), string()) -> string()).
url_makeAbsolute( {Protocol, Host, Path, Params}, Link ) ->
	case hd(Link) of
		$. ->
			% Remove "./" and "../" from urls
			{First, Second} = partition($/, Link),
			case First of
				".." ->
					{_, NewPath} = partition($/, lists:reverse(Path)),
					url_makeAbsolute( {Protocol, Host, lists:reverse(NewPath), Params}, Second );
				"." ->
					url_makeAbsolute( {Protocol, Host, Path, Params}, Second );
				_ ->
					Protocol ++ "://" ++ Host ++ filename:join(["/", Path, Link])
			end;
		$/ -> Protocol ++ "://" ++ Host ++ filename:join(["/", Link]); % '/'
		$? -> Protocol ++ "://" ++ Host ++ filename:join(["/", Path]) ++ Link; % '?'
		$# -> Protocol ++ "://" ++ Host ++ filename:join(["/", Path]) ++ "?" ++ Params; % '#'
		_ ->
			case lists:member($:, Link) of
				true ->
					Link;
				false ->
					Protocol ++ "://" ++ Host ++ filename:join(["/", Path, Link])
			end
	end;
url_makeAbsolute( SourceUrl, Url ) ->
	url_makeAbsolute( uri_parse(SourceUrl), Url ).

-spec(uri_parse/1 :: ({string(), string(), string(), string()}) -> string()).
uri_parse( Uri ) ->
	% TODO: fix when port is specified, ':'
	{Protocol, [_|[_|Url]]} = partition($:, Uri),
	{Request, Params} = partition($?, Url),
	{Host, Path} = partition($/, Request),
	{string:to_lower(Protocol), Host, Path, Params}.

partition(Delim, Str) ->
	{Str1, Str2} = lists:splitwith(fun(A) -> A =/= Delim end, Str),
	if hd(Str2) =:= Delim -> {Str1, tl(Str2)};
		true -> {Str1, Str2}
	end.

% Rule helpers
templates_run( Map, Templates ) ->
	% Run a list of templates with the variable definitions to return a list of strings
	io:format("Rendering templates with map: ~w~n", Map),
	lists:map(fun(X) -> sgte:render_str(X, Map) end, Templates).
find_rule( [], _ ) ->
	nomatch;
find_rule( [{Translation, PID}|RuleProcesses], Url ) ->
	{Regex, Atoms, Templates} = Translation,
	case re:run(Url, Regex, [global, {capture, Atoms, list}]) of
		{match, Subpatterns} ->
			io:format("router: matched rule patterns = ~w~natoms = ~w~n", [Subpatterns, Atoms]),
			{ok, PID, templates_run(lists:zip(Atoms, Subpatterns), Templates)};
		nomatch ->
			find_rule( RuleProcesses, Url )
	end.

response_to_string(Value) ->
	response_to_string( Value, 0 ).
response_to_string( [], _ ) ->
	"";
response_to_string( [{Url, Response}|Rest], Depth ) ->
	lists:duplicate(Depth, $ ) ++ Url ++ "\n" ++ response_to_string( Response, Depth + 2 ) ++ response_to_string(Rest, Depth);
response_to_string( [Filename|Rest], Depth ) ->
	lists:duplicate(Depth, $ ) ++ Filename ++ "\n" ++ response_to_string( Rest, Depth ).
