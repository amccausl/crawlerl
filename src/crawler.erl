 %%% @author Alex McCausland <alex.mccausland@gmail.com>

%% Data Types:
%%
%% Type			= crawl | save | rewrite
%% Rule			= {Type, Name, Regex, Templates}
%%				= {Type, Name, Regex, Templates, Params}
%% RuleProcesses = [{Translation, PID}]
%% CompiledRule	= {Type, Name, Translation}
%% Translation	= {CompiledRegex, Atoms, CompiledTemplates}
%% MatchedRule	= {Type, Name, MappedTranslation}
%% MappedTranslation = {AtomsMap, CompiledTemplates}
%% Request		= {[Id], Url, Translations}
%% Id			= {RuleName, PID, Url}
%% Response 	= {Url, [Filename]}
%%				= {Url, [Response]}
%% Params		= [Param]
%% Param		= {thread, int()} | {url_filter, [UrlFilter]} | {depth, int()} | {header_filter, [HeaderFilter]}
%% UrlFilter	= {regex, Regex} | {local_only} | {sub_only}
%% HeaderFilter	= {content_type, string()} | {filesize, {Lower, Upper}}

%% Actors:
%%
%% router:
%%	Incoming:	rule_actor			=>	{request, Request}
%%	Outgoing:	{request, Request}	=>	rule_manager
%%
%% rule_manager:
%%	Incoming:	router					=> {ok, Request, Response}
%%	Outgoing:	{ok, Request, Response}	=> rule_actor

%% TODO
% TODO: add index file handling
% TODO: should store each processed url in each thread
% TODO: on launch, should check process cache, then translated filename (if local) to determine if fetch has already occured
% TODO: add caching in rule_manager
% TODO: rewrite directory requests to use index.html (should happen on rule end)
% TODO: add parameter for allowed depth, cycling detection or url regex param on crawl rule (use template) and thread count
% TODO: add crawl parameters to filter captured urls on regex, header (size, content type)
% TODO: examine http methods to find a way to abort a download from examining the headers
% TODO: consider changing methods around to allow $ext$ tag harvested from headers
% TODO: rework to have router populate variables, actor run translation
% TODO: populate variables in translation from fetch (test template if variable is defined multiple times)
% TODO: look into using as a download handler for uzbl (maybe just as a proxy)

-module( crawler ).
-export( [ start/0, stop/0, run/1 ] ).
% utility methods
-export( [ url_to_uri/1, html_extract_urls/2 ] ).
% expose methods for testing
-export( [ router/1, rule_manager/4, rule_actor/2, find_rule/2, templates_run/2, url_filter/3 ] ).

% start with url on cmd line, maybe some preprocessing
start() ->
	case lists:member(router, erlang:registered()) of
		true ->
			{ok};
		false ->
			{ok, Rules} = crawler_config:load_rules(),
			RuleProcesses = rule_manager_spawn( Rules, [] ),
			PID = spawn( crawler, router, [RuleProcesses] ),
			register(router, PID),
			% TODO: configure http client
			ibrowse:start()
	end.
stop() ->
	router ! {stop}.

run(Url) ->
	Timeout = 30,
	router ! {request, {[{run, erlang:self(), []}], Url}},
	receive
		{ok, _, Response} ->
			io:format( "run: response received~n~w~n", [Response] );
		Err ->
			io:format( "run: error ~w~n", [Err] )
	after Timeout * 1000 ->
		io:format("run: ~B timeout expired~n", [Timeout])
	end.

router(RuleProcesses) ->
	io:format("router process '~w' waiting...~n", [erlang:self()]),
% All requests go to the router first
% The router looks up the rule, runs the translation, generates the Rule request and sends it to the rule manager.
	receive
		{request, {Ids, Url}} ->
			case find_rule( RuleProcesses, Url ) of
				nomatch ->
					io:format("router: No rule for url '~s'/n", [Url]),
					router(RuleProcesses);
				{ok, PID, MappedTranslations} ->
					io:format("router: matched '~s' to rule~n", [Url]),
					PID ! {request, {Ids, Url, MappedTranslations}},
					router(RuleProcesses)
			end;
		{status} ->
			io:format("router: status~n"),
			lists:map(fun(X) -> PID = element(2, X), PID ! {status} end, RuleProcesses),
			router( RuleProcesses );
		{stop} ->
			lists:map(fun(X) -> element(2, X) ! {stop} end, RuleProcesses),
			exit({ok})
	end.

rule_manager_spawn( [], AccRules ) ->
	lists:reverse(AccRules);
rule_manager_spawn( [{Type, Name, Translation}|Rules], AccRules ) ->
	rule_manager_spawn( [{Type, Name, Translation, []}|Rules], AccRules );
rule_manager_spawn( [{Type, Name, Translation, Params}|Rules], AccRules ) ->
	PID = spawn( crawler, rule_manager, [{Type, Name, Params}, [], [], 5]),
	rule_manager_spawn( Rules, [{Translation, PID}|AccRules] ).

rule_manager( Rule, [Request|Queue], Threads, ThreadCount) when ThreadCount > 0 ->
	io:format("rule_manager: ~s, with ~B threads left~n", [element(2, Rule), ThreadCount]),
	%{Ids, Url, MappedTranslation} = Request,
	%Id = {element(2, Rule), erlang:self(), Url},
	%PID = spawn( crawler, rule_actor, [Rule, {[Id|Ids], Url, MappedTranslation}] ),
	PID = spawn( crawler, rule_actor, [Rule, Request] ),
	erlang:monitor(process, PID),
	rule_manager( Rule, Queue, [{PID, Request}|Threads], ThreadCount - 1 );
rule_manager( Rule, Queue, Threads, ThreadCount) ->
	Name = element(2, Rule),
	io:format("rule_manager: ~s, waiting with ~B threads left~n", [Name, ThreadCount]),
	receive
		{request, Request} ->
			io:format("rule_manager '~s' called with {request, ~s}~n", [Name, request_to_string(Request)]),  
			rule_manager( Rule, [Request|Queue], Threads, ThreadCount);
		{'DOWN', _, process, Pid, Reason} ->
			io:format("rule_manager '~s': received death from ~w~n", [Name, Pid]),
			case Reason of
				{ok, {[], _, _}, Response} ->
					io:format("Thread '~s:~w' finished~n~w~n", [Name, erlang:self(), Response]);
				{ok, {[Id|Ids], Url, Translations}, Response} ->
					% NOTE: Id removed
					PID = erlang:element(2, Id),
					io:format("rule_manager ~s: received result from ~w, passing to ~w. id = ~w~n", [Name, Pid, PID, [Id|Ids]]),
					PID ! {ok, {Ids, Url, Translations}, Response},
					io:format("rule_manager ~s: message to ~w: id = ~w~n", [Name, PID, {ok, {Ids, Url, Translations}, Response}]);
				_ ->
					io:format("rule_actor '~s:~w' failed: ~w~n", [Name, Pid, Reason])
			end,
			rule_manager( Rule, Queue, lists:keydelete(Pid, 1, Threads), ThreadCount + 1 );
		{status} ->
			io:format("Rule: ~w~n~w~n", [Rule, Threads]),
			rule_manager( Rule, Queue, Threads, ThreadCount );
		{stop} ->
			io:format("stopping rule_manager '~s:~w' ~B threads~n", [Name, erlang:self(), ThreadCount]),
			lists:map(fun(X) -> exit(element(1, X), kill) end, Threads)
	end.

rule_actor( {crawl, Name, Params}, Request ) ->
	io:format("running rule_actor '~s:~w'~n", [Name, erlang:self()]),
	{_, URL, {Map, Translations}} = Request,
	Filenames = templates_run(Map, Translations),  
	UrlFilters = keyfind2( url_filter, 1, Params, [] ),
	%{Protocol, } = url_to_uri( Uri ),
	case fetch(URL) of
		{ok, Body} ->
			RelativeUrls = html_extract_urls(Body, []),
			io:format( "rule_actor: parsed relative urls from '~s'~n~w~n", [URL, RelativeUrls] ),
			URI			 = url_to_uri( URL ),
			FullURLs	 = lists:map(fun(X) -> uri_to_url(relativeurl_to_uri(X, URI)) end, RelativeUrls),
			io:format( "rule_actor: FullURLs = ~w~n", [FullURLs] ),
			URLs		 = lists:filter(fun(X) -> url_filter( URL, X, UrlFilters ) end, FullURLs ),
			io:format( "rule_actor: following urls: ~w~n", [URLs] ),
			Result 		 = rule_spawn_and_collect( Request, URLs ),
			io:format( "rule_actor '~s:~w': finished crawling urls~n", [Name, erlang:self()] ),
			ResultString = response_to_string(element(3, Result)),
			lists:map(fun(X) -> file:write_file(X, ResultString) end, Filenames),
			exit(Result);
		{fail, Reason} ->
			io:format("fetch failed for ~s with reason: ~w~n", [URL, Reason]),
			exit({ok, Request, {URL, []}})
	end;
rule_actor( {save, Name, Params}, Request ) ->
	{_, Url, {Map, Translations}} = Request,
	Filenames = templates_run(Map, Translations),
	HeaderFilters = keyfind2( header_filter, 1, Params, [] ),
	io:format("running rule_actor '~s:~w' on url ~s~n", [Name, erlang:self(), Url]),
	case fetch_file( Url ) of
		{ok, FetchedFilename} ->
			lists:map(fun(Filename) ->
				%io:format("creating file ~s~n", [Filename]),
				{_, RevDirectory} = partition($/, lists:reverse(Filename)),
				filelib:ensure_dir(lists:reverse(RevDirectory)),
				file:copy(FetchedFilename, Filename) end, Filenames),
			file:delete(FetchedFilename),
			io:format("rule returning result: {~s, ~s}~n", [Url, Filenames]),
			exit({ok, Request, {Url, Filenames}});
		{fail, Reason} ->
			io:format("fetch failed for url '~s': ~w~n", [Url, Reason]),
			exit({ok, Request, {Url, []}})
	end;
rule_actor( {rewrite, Name, Params}, Request ) ->
	io:format("running rule_actor '~s:~w'~n", [Name, erlang:self()]),
	Translations = element(3, Request),
	exit(rule_spawn_and_collect( Request, Translations )).

rule_spawn_and_collect( Request, Urls ) ->
	{ok, Responses, Count}	= rule_spawner( Request, Urls, [], 0 ),
	PopulatedResponses		= rule_response_collector( Responses, Count ),
	{ok, Request, PopulatedResponses}.
rule_spawner( _, [], Responses, Count ) ->
	io:format( "rule_spawner: finished spawning requests, waiting for responses ~w~n", [Responses] ),
	{ok, Responses, Count};
rule_spawner( Request, [Url|Urls], Responses, Count ) ->
	io:format("rule_spawner: creating request for '~s'~n", [Url]),
	IDs = element(1, Request),
	NewId = [{rule_spawner, erlang:self(), Url}|IDs],
	% TODO: add in cycling detection for crawlers (examine id for occurrences of the same rule, if exists, spawn thread to resolve)
	router ! { request, {NewId, Url} },
	rule_spawner( Request, Urls, [{Url}|Responses], Count + 1 ).
rule_response_collector( Responses, 0 ) ->
	io:format("rule_response_collector finished~n"),
	Responses;
rule_response_collector( Responses, Outstanding ) ->
%% Collect the result of requests for Urls
	io:format( "rule_response_collector: waiting for responses (~B outstanding)~n", [Outstanding] ),
	receive
		{ok, Request, Response} ->
			io:format( "rule_response_collector: response received ~w~n", [Response] ),
			{_, Url, _} = Request, 
			NewResponses = lists:keyreplace(Url, 1, Responses, Response),
			rule_response_collector( NewResponses, Outstanding - 1 );
		Msg ->
			io:format( "rule_response_collector: error, unknown message ~w~n", [Msg] ),
			rule_response_collector( Responses, Outstanding )
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
keyfind2(Key, N, TupleList, Default) ->
	case lists:keyfind(Key, N, TupleList) of
		{Key, Value} -> Value;
		false -> Default
	end.
partition(Delim, Str) ->
	{Str1, Str2} = lists:splitwith(fun(A) -> A =/= Delim end, Str),
	if hd(Str2) =:= Delim -> {Str1, tl(Str2)};
		true -> {Str1, Str2}
	end.

% Rule helpers
templates_run( Map, Templates ) ->
	% Run a list of templates with the variable definitions to return a list of strings
	%io:format("Rendering templates with map: ~w~n", Map),
	lists:map(fun(X) -> sgte:render_str(X, Map) end, Templates).
find_rule( [], _ ) ->
	nomatch;
find_rule( [{Translation, PID}|RuleProcesses], Url ) ->
	{Regex, Atoms, Templates} = Translation,
	case re:run(Url, Regex, [global, {capture, Atoms, list}]) of
		{match, Subpatterns} ->
			%{ok, PID, templates_run(lists:zip(Atoms, Subpatterns), Templates)};
			{ok, PID, {lists:zip(Atoms, Subpatterns), Templates}};
		nomatch ->
			find_rule( RuleProcesses, Url )
	end.

%% Apply filters to URLs 
url_filter( _, DestinationURL, {regex, Regex} ) ->
	case re:run( uri_to_url(DestinationURL), Regex ) of
		{match, _} -> true;
		nomatch -> false
	end;
url_filter( SourceURL, DestinationURL, {sub_only} ) ->
	{_, SHost, SPath, _} = url_to_uri(SourceURL),
	{_, DHost, DPath, _} = url_to_uri(DestinationURL),
	lists:prefix( SHost ++ SPath, DHost ++ DPath );
url_filter( SourceURL, DestinationURL, {local_only} ) ->
	{_, SHost, _, _} = url_to_uri(SourceURL),
	{_, DHost, _, _} = url_to_uri(DestinationURL), 
	SHost == DHost;
url_filter( SourceURL, DestinationURL, {and_filters, [Filter|Filters]} ) ->
	case url_filter( SourceURL, DestinationURL, Filter ) of
		true -> url_filter( SourceURL, DestinationURL, {and_filters, Filters} );
		false -> false
	end;
url_filter( _, _, {and_filters, []} ) ->
	true;
url_filter( SourceURL, DestinationURL, [Filter|Filters] ) ->
	case url_filter( SourceURL, DestinationURL, Filter ) of
		true -> true;
		false -> url_filter( SourceURL, DestinationURL, Filters )
	end;
url_filter( _, _, [] ) ->
	false.

url_to_uri( URL ) ->
	% TODO: fix when port is specified, ':'
	{Protocol, [_|[_|Url]]} = partition($:, URL),
	{Request, Params} = partition($?, Url),
	{Host, Path} = partition($/, Request),
	{string:to_lower(Protocol), Host, Path, Params}.
relativeurl_to_uri( Link, {Protocol, Host, Path, Params} ) ->
	case hd(Link) of
		$. ->
			% Remove "./" and "../" from urls
			{First, Second} = partition($/, Link),
			case First of
				".." ->
					{_, NewPath} = partition($/, lists:reverse(Path)),
					relativeurl_to_uri( Second, {Protocol, Host, lists:reverse(NewPath), Params} );
				"." ->
					relativeurl_to_uri( Second, {Protocol, Host, Path, Params} );
				_ ->
					{Protocol, Host, filename:join(["/", Path, Link]), ""}
			end;
		$/ -> {Protocol, Host, filename:join(["/", Link]), ""};
		$? -> {Protocol, Host, filename:join(["/", Path]) ++ Link, ""};
		$# -> {Protocol, Host, filename:join(["/", Path]), Params};
		_ ->
			case lists:member($:, Link) of
				true ->
					url_to_uri( Link );
				false ->
					{Protocol, Host, filename:join(["/", Path, Link]), ""}
			end
	end.
uri_to_url( {Protocol, Host, Path, ""} ) ->
	Protocol ++ "://" ++ Host ++ Path;
uri_to_url( {Protocol, Host, Path, Params} ) ->
	Protocol ++ "://" ++ Host ++ Path ++ "?" ++ Params.

request_to_string({IDs, URL, Translation}) ->
	Info = lists:flatten(io_lib:format("~s:~w", [URL, Translation])),
	lists:foldl(fun(X, Acc) -> lists:append([id_to_string(X), " => ", Acc]) end, Info, IDs).
id_to_string( {RuleName, PID, URL} ) ->
	lists:flatten(io_lib:format("'~s:~w' ~s", [RuleName, PID, URL])).
response_to_string(Value) ->
	response_to_string( Value, 0 ).
response_to_string( [], _ ) ->
	"";
response_to_string( [{Url, Response}|Rest], Depth ) ->
	lists:duplicate(Depth, $ ) ++ Url ++ "\n" ++ response_to_string( Response, Depth + 2 ) ++ response_to_string(Rest, Depth);
response_to_string( [Filename|Rest], Depth ) ->
	lists:duplicate(Depth, $ ) ++ Filename ++ "\n" ++ response_to_string( Rest, Depth );
response_to_string( Response, _ ) ->
	io:format("response_to_string: unknown response ~w~n", [Response]),
	"".
