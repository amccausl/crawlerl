 %%% @author Alex McCausland <alex.mccausland@gmail.com>
-module( crawler ).
-export( [ start/0, stop/0, run/1 ] ).
% utility methods
-export( [ url_to_uri/1, uri_to_url/1, html_extract_urls/2 ] ).
% expose methods for testing
-export( [ router/1, rule_manager/4, rule_actor/2, find_rule/2, templates_run/2, url_filter/3 ] ).

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
%% Param		= {thread, int()} | {url_filter, [UrlFilter]} | {depth, int()} | {header_filter, [HeaderFilter]} | {headers, [{header, value}]}
%% UrlFilter	= {regex, Regex} | {local_only} | {sub_only}
%% HeaderFilter	= {content_type, string()} | {filesize, {Lower, Upper}}

%-record( request, {ids, url, translations} ).

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
% TODO: clean up logging
% TODO: use same format for index files that feh supports
% TODO: rewrite to use records for messages
% TODO: fix handling of empty url lists
% TODO: fix rewrite rule bug
% TODO: determine cause of robot flagging in the wild
% TODO: add index file handling
% TODO: should store each processed url in each thread
% TODO: on launch, should check process cache, then translated filename (if local) to determine if fetch has already occured
% TODO: add caching in rule_manager
% TODO: rewrite directory requests to use index.html (should happen on rule end)
% TODO: add parameter for allowed depth, cycling detection or url regex param on crawl rule (use template) and thread count
% TODO: add crawl parameters to filter captured urls on header (size, content type)
% TODO: examine http methods to find a way to abort a download from examining the headers
% TODO: consider changing methods around to allow $ext$ tag harvested from headers
% TODO: rework to have router populate variables, actor run translation
% TODO: populate variables in translation from fetch (test template if variable is defined multiple times)
% TODO: look into using as a download handler for uzbl (maybe just as a proxy)

-define(USER_AGENT, "Mozilla/5.0 (X11; U; Linux i686; en-US) AppleWebKit/533.1 (KHTML, like Gecko) Chrome/5.0.335.0 Safari/533.1").
 
% start with url on cmd line, maybe some preprocessing
start() ->
	application:start(log4erl),
	log4erl:conf("../config/log4erl.conf"),
	ibrowse:start(),
	% TODO: configure http client
	ets:new(cookie_jar, [duplicate_bag, public, named_table]),
	case lists:member(router, erlang:registered()) of
		true ->
			{ok};
		false ->
			{ok, Rules} = crawler_config:load_rules(),
			RuleProcesses = rule_manager_spawn( Rules, [] ),
			PID = spawn( crawler, router, [RuleProcesses] ),
			register(router, PID)
	end.
stop() ->
	router ! {stop}.

run(Url) ->
	router ! {request, {[{run, erlang:self(), Url}], Url}},
	receive
		{ok, _, Response} ->
			log4erl:info( crawler, "run: response received: ~p", [Response] );
		Err ->
			log4erl:error( crawler, "run: error ~p", [Err] )
	after infinity ->
		log4erl:error(crawler, "run: 30 second timeout expired")
	end.

router(RuleProcesses) ->
	log4erl:debug( crawler, "router process '~p' waiting...", [erlang:self()]),
% All requests go to the router first
% The router looks up the rule, runs the translation, generates the Rule request and sends it to the rule manager.
	receive
		{request, {Ids, Url}} ->
			case find_rule( RuleProcesses, Url ) of
				nomatch ->
					log4erl:warn( crawler, "router: No rule for url '~s'", [Url] ),
					router(RuleProcesses);
				{ok, PID, MappedTranslations} ->
					log4erl:info( crawler, "router: matched url '~s' to rule", [Url] ),
					PID ! {request, {Ids, Url, MappedTranslations}},
					router(RuleProcesses)
			end;
		{status} ->
			log4erl:info( crawler, "router: status" ),
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
	Threads = keyfind(threads, 2, Params, 5),
	PID = spawn( crawler, rule_manager, [{Type, Name, Params}, [], [], Threads]),
	rule_manager_spawn( Rules, [{Translation, PID}|AccRules] ).

rule_manager( {Type, Name, Params} = Rule, [{IDs, URL, _} = Request|Queue], Threads, ThreadCount) when ThreadCount > 0 ->
	log4erl:debug( crawler, "rule_manager: ~s, with ~B threads left", [element(2, Rule), ThreadCount] ),
	log4erl:debug( crawler, "rule_manager: extracting referer for ~s from id ~p", [URL, IDs] ),
	RequestHeaders = get_cookies(URL, [{'REFERER', extract_referer(IDs, URL)}, {'USER_AGENT', ?USER_AGENT}]), 
	PID = spawn( crawler, rule_actor, [{Type, Name, [{headers, RequestHeaders}|Params]}, Request] ),
	erlang:monitor(process, PID),
	rule_manager( Rule, Queue, [{PID, Request}|Threads], ThreadCount - 1 );
rule_manager( Rule, Queue, Threads, ThreadCount) ->
	Name = element(2, Rule),
	log4erl:debug( crawler, "rule_manager: ~s, waiting with ~B threads left", [Name, ThreadCount] ),
	receive
		{request, Request} ->
			log4erl:debug( crawler, "rule_manager '~s' called with {request, ~s}", [Name, request_to_string(Request)] ),  
			rule_manager( Rule, [Request|Queue], Threads, ThreadCount);
		{'DOWN', _, process, Pid, Reason} ->
			log4erl:debug( crawler, "rule_manager '~s': received death from ~p", [Name, Pid] ),
			case Reason of
				{ok, {[], _, _}, Response} ->
					log4erl:debug( crawler, "Thread '~s:~p' finished: ~p", [Name, erlang:self(), Response] );
				{ok, {[Id|Ids], Url, Translations}, Response} ->
					PID = erlang:element(2, Id),
					log4erl:debug( crawler, "rule_manager ~s: received result from ~p, passing to ~p.", [Name, Pid, PID] ),
					PID ! {ok, {Ids, Url, Translations}, Response},
					log4erl:debug( crawler, "rule_manager ~s: message to ~p: id = ~s", [Name, PID, lists:foldl(fun(X, Acc) -> lists:append([id_to_string(X), " => ", Acc]) end, "", Ids)] );
				_ ->
					log4erl:error( crawler, "rule_actor '~s:~p' failed: ~p", [Name, Pid, Reason] )
			end,
			rule_manager( Rule, Queue, lists:keydelete(Pid, 1, Threads), ThreadCount + 1 );
		{status} ->
			io:format( "Rule: ~p: ~p", [Rule, Threads] ),
			rule_manager( Rule, Queue, Threads, ThreadCount );
		{stop} ->
			log4erl:info( crawler, "stopping rule_manager '~s:~p' ~B threads", [Name, erlang:self(), ThreadCount] ),
			lists:map(fun(X) -> exit(element(1, X), kill) end, Threads)
	end.

rule_actor( {crawl, Name, Params}, Request ) ->
	log4erl:debug( crawler, "running rule_actor '~s:~p'", [Name, erlang:self()] ),
	{_, URL, {Map, Translations}} = Request,
	RequestHeaders = keyfind( headers, 2, Params, [] ),
	UrlFilters = keyfind( url_filter, 2, Params, [] ),
	case fetch(URL, RequestHeaders) of
		{ok, Headers, Body} ->
			RelativeUrls = lists:usort(html_extract_urls(Body, [])),
			URI			 = url_to_uri( URL ),
			%log4erl:debug( crawler, "rule_actor: parsed relative urls from '~s' (~p): ~p", [URL, URI, RelativeUrls] ),
			FullURLs	 = lists:usort(lists:map(fun(X) -> uri_to_url(relativeurl_to_uri(X, URI)) end, RelativeUrls)),
			%log4erl:debug( crawler, "rule_actor: FullURLs = ~p", [FullURLs] ),
			URLs		 = lists:filter(fun(X) -> url_filter( URL, X, UrlFilters ) end, FullURLs ),
			log4erl:debug( crawler, "rule_actor: following urls: ~p", [URLs] ),
			Result 		 = rule_spawn_and_collect( Request, URLs ),
			log4erl:debug( crawler, "rule_actor '~s:~p': finished crawling urls", [Name, erlang:self()] ),
			ResultString = response_to_string(element(3, Result)),
			Filenames = templates_run(Map, Translations),
			lists:map(fun(X) -> file:write_file(X, ResultString) end, Filenames),
			exit(Result);
		{fail, Reason} ->
			log4erl:error( crawler, "fetch failed for ~s with reason: ~p", [URL, Reason] ),
			exit({ok, Request, {URL, []}})
	end;
rule_actor( {save, Name, Params}, Request ) ->
	_HeaderFilters = keyfind( header_filter, 2, Params, [] ),
	{_, Url, {Map, Translations}} = Request,
	RequestHeaders = keyfind( headers, 2, Params, [] ),
	log4erl:info( crawler, "running rule_actor '~s:~p' on url ~s", [Name, erlang:self(), Url] ),
	case fetch_file( Url, RequestHeaders ) of
		{ok, Headers, FetchedFilename} ->
			log4erl:info( crawler, "fetch: ~s successful", [FetchedFilename] ),
			%TODO: add header to map 
			Filenames = templates_run(Map, Translations),
			lists:map(fun(Filename) ->
				ok = filelib:ensure_dir(Filename),
				file:copy(FetchedFilename, Filename) end, Filenames),
			file:delete(FetchedFilename),
			log4erl:debug( crawler, "rule returning result: {~s, ~s}", [Url, Filenames] ),
			exit({ok, Request, {Url, Filenames}});
		{fail, Reason} ->
			log4erl:error( crawler, "fetch failed for url '~s': ~p", [Url, Reason] ),
			exit({ok, Request, {Url, []}})
	end;
rule_actor( {rewrite, Name, _}, Request ) ->
	log4erl:debug( crawler, "running rule_actor '~s:~p'", [Name, erlang:self()] ),
	Translations = element(3, Request),
	exit(rule_spawn_and_collect( Request, Translations )).

rule_spawn_and_collect( Request, Urls ) ->
	{ok, Responses, Count}	= rule_spawner( Request, Urls, [], 0 ),
	PopulatedResponses		= rule_response_collector( Responses, Count ),
	{ok, Request, PopulatedResponses}.
rule_spawner( _, [], Responses, Count ) ->
	log4erl:info( crawler, "rule_spawner: finished spawning requests, waiting for responses ~p", [Responses] ),
	{ok, Responses, Count};
rule_spawner( Request, [Url|Urls], Responses, Count ) ->
	log4erl:debug( crawler, "rule_spawner: creating request for '~s'", [Url] ),
	timer:sleep(4000),
	IDs = element(1, Request),
	NewId = [{rule_spawner, erlang:self(), Url}|IDs],
	% TODO: add in cycling detection for crawlers (examine id for occurrences of the same rule, if exists, spawn thread to resolve)
	router ! { request, {NewId, Url} },
	rule_spawner( Request, Urls, [{Url}|Responses], Count + 1 ).
rule_response_collector( Responses, 0 ) ->
	log4erl:info( crawler, "rule_response_collector finished" ),
	Responses;
rule_response_collector( Responses, Outstanding ) ->
%% Collect the result of requests for Urls
	log4erl:debug( crawler, "rule_response_collector: waiting for responses (~B outstanding)", [Outstanding] ),
	receive
		{ok, Request, Response} ->
			log4erl:debug( crawler, "rule_response_collector: response received ~p", [Response] ),
			{_, Url, _} = Request, 
			NewResponses = lists:keyreplace(Url, 1, Responses, Response),
			rule_response_collector( NewResponses, Outstanding - 1 );
		Msg ->
			log4erl:error( crawler, "rule_response_collector: error, unknown message ~p", [Msg] ),
			rule_response_collector( Responses, Outstanding )
	end.

% Crawl helpers
fetch_file(Url, RequestHeaders) ->
	fetch(Url, RequestHeaders, get, [], [{save_response_to_file, true}]).
fetch(Url, RequestHeaders) ->
	fetch(Url, RequestHeaders, get, [], []).
fetch(Url, RequestHeaders, Type, [], Params) ->
	log4erl:info( crawler, "fetching ~s", [Url] ),
	case ibrowse:send_req(Url, get_cookies(Url, RequestHeaders), Type, [], Params) of
		{ok, "200", Headers, {file, SavedFile}} ->
			set_cookie(url_to_uri(Url), Headers),
			log4erl:debug( crawler, "fetch: headers = ~p", [Headers] ),
			{ok, Headers, SavedFile};
		{ok, "200", Headers, Body} ->
			set_cookie(url_to_uri(Url), Headers),
			log4erl:debug( crawler, "fetch: headers = ~p", [Headers] ),
			{ok, Headers, Body};
		{ok, "301", Headers, _} ->
			log4erl:debug( crawler, "fetch: redirect headers = ~p", [Headers] ),
			case lists:keyfind("Location", 1, Headers) of
				false ->
					log4erl:error( crawler, "missing Location header on 301 response" ),
					{fail, "301 response, no location"};
				{_, Location} ->
					log4erl:warn( crawler, "redirecting fetch to ~s", [Location] ),
					fetch(Location, RequestHeaders, Type, [], Params);
				A ->
					log4erl:error( crawler, "match failed for ~p", [A] ),
					{fail, "Match failed"}
			end;
		Msg ->
			log4erl:error( crawler, "failed with '~p'", [Msg] ),
			{fail, Msg}
	end.

set_cookie(_, []) ->
	true;
set_cookie({_, Host, _, _} = URI, [{"Set-Cookie", CookieRequest}|Headers]) ->
	log4erl:info( crawler, "request from '~s', saving cookie: ~s", [Host, CookieRequest] ),
	{Cookie, _Path} = partition( $ , CookieRequest ),
	ets:insert(cookie_jar, {Host, Cookie}),
	set_cookie(URI, Headers);
set_cookie(URI, [_|Headers]) ->
	set_cookie(URI, Headers).
get_cookies(URL, RequestHeaders) ->
	log4erl:info( crawler, "retrieving cookies for ~s", [URL] ),
	{_Protocol, Host, _Path, _Params} = url_to_uri( URL ),
	case ets:lookup(cookie_jar, Host) of
		false ->
			RequestHeaders;
		Cookies ->
			CookieMap = lists:foldl(
				fun(X, Acc) ->
						{Key, Value} = partition($=, element(2, X)),
						lists:keystore(Key, 1, Acc, {Key, Value})
					end,
				[], Cookies),
			CookieStr = lists:foldl(
				fun({Key, Value}, Acc) ->
						lists:concat([Key, "=", Value, " ", Acc]) end, "", CookieMap),
			log4erl:debug( crawler, "using cookies: ~s", [CookieStr] ),
			[{cookie, CookieStr}|RequestHeaders]
	end.
extract_referer([], _) ->
	"";
extract_referer([ID|IDs], URL) ->
	{_, _, Url} = ID,
	if
		Url == URL -> extract_referer(IDs, URL);
		true -> Url
	end.
parse_quoted(Input) ->
	Terminators = "'\" ",
	[Quote|Tail] = Input,
	case lists:member(Quote, Terminators) of
		true ->
			Quoted = lists:takewhile(fun(X) -> not (lists:member(X, Terminators)) end, Tail),
			{Quoted, lists:nthtail(length(Quoted), Tail)};
		false ->
			Quoted = lists:takewhile(fun(X) -> not (lists:member(X, Terminators)) end, Input),
			{Quoted, lists:nthtail(length(Quoted), Input)}
	end.
html_extract_urls( [], Urls ) ->
	lists:reverse(Urls);
html_extract_urls( HTML, Urls ) ->
	case lists:prefix("src=", HTML) or lists:prefix("SRC=", HTML) of
		true ->
			{Url, Remaining} = parse_quoted(lists:nthtail(4, HTML)),
			html_extract_urls(Remaining, [Url|Urls]);
		false ->
			case lists:prefix("href=", HTML) or lists:prefix("HREF=", HTML) of
				true ->
					{Url, Remaining} = parse_quoted(lists:nthtail(5, HTML)),
					html_extract_urls(Remaining, [Url|Urls]);
				false ->
					[_|T] = HTML,
					html_extract_urls(T, Urls)
			end
	end.
keyfind(Key, N, TupleList, Default) ->
	case lists:keyfind(Key, 1, TupleList) of
		false -> Default;
		Tuple -> element(N, Tuple)
	end.
partition(Delim, Str) ->
	{Str1, Str2} = lists:splitwith(fun(A) -> A =/= Delim end, Str),
	if hd(Str2) =:= Delim -> {Str1, tl(Str2)};
		true -> {Str1, Str2}
	end.

% Rule helpers
templates_run( Map, Templates ) ->
	% Run a list of templates with the variable definitions to return a list of strings
	%io:format("Rendering templates with map: ~p", Map),
	lists:map(fun(X) -> sgte:render_str(X, Map) end, Templates).
find_rule( [], _ ) ->
	nomatch;
find_rule( [{Translation, PID}|RuleProcesses], Url ) ->
	{Regex, Atoms, Templates} = Translation,
	case re:run(Url, Regex, [global, {capture, Atoms, list}]) of
		{match, [Subpatterns]} ->
			%{ok, PID, templates_run(lists:zip(Atoms, Subpatterns), Templates)};
			log4erl:debug( crawler, "Atoms = ~p, Subpatterns = ~p", [Atoms, Subpatterns] ),
			{ok, PID, {lists:zip(Atoms, Subpatterns), Templates}};
		nomatch ->
			find_rule( RuleProcesses, Url )
	end.

%% Apply filters to URLs 
url_filter( _, DestinationURL, {regex, Regex} ) ->
	case re:run( DestinationURL, Regex ) of
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
	url_filter( SourceURL, DestinationURL, Filter )
		andalso url_filter( SourceURL, DestinationURL, {and_filters, Filters} );
url_filter( _, _, {and_filters, []} ) ->
	true;
url_filter( SourceURL, DestinationURL, [Filter|Filters] ) ->
	url_filter( SourceURL, DestinationURL, Filter )
		orelse url_filter( SourceURL, DestinationURL, Filters );
url_filter( _, _, [] ) ->
	false.

url_to_uri( URL ) ->
	% TODO: fix when port is specified, ':'
	{Protocol, [_|[_|Url]]} = partition($:, URL),
	{Request, Params} = partition($?, Url),
	{Host, Path} = partition($/, Request),
	{string:to_lower(Protocol), Host, Path, Params}.
relativeurl_to_uri( [], URI ) ->
	URI;
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
					case partition($/, lists:reverse(Path)) of
						{"", _} -> {Protocol, Host, filename:join(["/", Path, Link]), ""};
						{_, PathRev} -> {Protocol, Host, filename:join(["/", lists:reverse(PathRev), Link]), ""}
					end
			end
	end.
uri_to_url( {Protocol, Host, Path, ""} ) ->
	Protocol ++ "://" ++ Host ++ filename:join(["/", Path]);
uri_to_url( {Protocol, Host, Path, Params} ) ->
	Protocol ++ "://" ++ Host ++ filename:join(["/", Path]) ++ "?" ++ Params.

request_to_string({IDs, URL, Translation}) ->
	Info = lists:flatten(io_lib:format("~s:~p", [URL, Translation])),
	lists:foldl(fun(X, Acc) -> lists:append([id_to_string(X), " => ", Acc]) end, Info, IDs).
id_to_string( {RuleName, PID, URL} ) ->
	lists:flatten(io_lib:format("'~s:~p' ~s", [RuleName, PID, URL])).
response_to_string(Value) ->
	response_to_string( Value, 0 ).
response_to_string( [], _ ) ->
	"";
response_to_string( [{Url, Response}|Rest], Depth ) ->
	lists:duplicate(Depth, $ ) ++ Url ++ "\n" ++ response_to_string( Response, Depth + 2 ) ++ response_to_string(Rest, Depth);
response_to_string( [Filename|Rest], Depth ) ->
	lists:duplicate(Depth, $ ) ++ Filename ++ "\n" ++ response_to_string( Rest, Depth );
response_to_string( Response, _ ) ->
	log4erl:error(crawler, "response_to_string: unknown response ~p", [Response]),
	"".
