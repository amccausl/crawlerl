%%% @author Alex McCausland <alex.mccausland@gmail.com>

%% Data Types:
%%
%% Type			= crawl | save | rewrite
%% Translation	= {Regex, Atoms, Templates}
%% Rule			= {Type, Name, Regex, Templates}
%%				= {Type, Name, Regex, Templates, Params}
%% CompiledRule	= {Type, Name, Translation}
%% MatchedRule	= {Type, Name, Strings}
%% Request		= [Id|Ids]
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
% TODO: add debugging stats calls
% TODO: finish and test matcher
% TODO: spawn manager threads for each rule
% TODO: router should prepend ID information
% TODO: write tests for rewrite functionality
% TODO: add index file handling
% TODO: should store each processed url in each thread
% TODO: on launch, should check process cache, then translated filename (if local) to determine if fetch has already occured
% TODO: add in cycling detection for crawlers (examine id for occurrences of the same rule, if exists, spawn thread to resolve)
% TODO: add caching in rule_manager

-module( crawler ).
-export( [ start/0, router/1, rule_actor/2, uri_parse/1, html_extract_urls/2, url_makeAbsolute/2 ] ).

% start with url on cmd line, maybe some preprocessing
start() ->
	init(),
	{ok, URLs} = init:get_arguments( url ),
	{ok, Rules} = crawler_config:load_rules(),
	PID = spawn( crawler, router, [Rules] ),
	register(router, PID),
	route(PID, URLs).

route( _, [] ) ->
	{ok};
route( PID, [Url|Urls] ) ->
	PID ! {url, Url},
	route( PID, Urls ).

init() ->
  % configure http client
  %inets:start().
	ibrowse:start().

router(Rules) ->
% All requests go to the router first
% The router looks up the rule, runs the translation, generates the Rule request and sends it to the rule manager.
	receive
		{url, Url} ->
			case find_rule( Rules, Url ) of
				{undef} ->
					io:format("No rule for url: ~s/n", [Url]),
					router(Rules);
				{ok, MatchedRule} ->
					{_, Name, Filenames} = MatchedRule,
					[{Name, RuleMgrPID}] = ets:lookup(?MODULE, Name),
					io:format("Matched '~s' to rule '~s'", [Url, Name]),
					Request = [{Name, RuleMgrPID, Url}],
					RuleMgrPID ! {request, blah},
					router(Rules)
			end;
		{request, Request} ->
			io:format("blah"),
			router(Rules)
	end.

rule_manager( Rule, [Request|Queue], Threads, ThreadCount) when ThreadCount > 0 ->
	PID = spawn( crawler, rule_actor, [Rule, Request] ),
	erlang:monitor(process, PID),
	rule_manager( Rule, Queue, [{PID, Request}|Threads], ThreadCount - 1 );
rule_manager( Rule, Queue, Threads, ThreadCount) ->
	receive
		{request, Request} ->
			rule_manager( Rule, [Request|Queue], Threads, ThreadCount);
		{'DOWN', _, process, Pid, Reason} ->
			% pass message to parent, Reason = {Id, [Filenames]}
			rule_manager( Rule, Queue, lists:keydelete(Pid, 1, Threads), ThreadCount + 1 )
	end.

rule_actor( {crawl, Name, Filenames}, [{Rulename, PID, Url}|Rest] ) ->
	Request		 = [{Rulename, PID, Url}|Rest],
	%{Protocol, } = uri_parse( Uri ),
	Body		 = get_page(Url),
	RelativeUrls = html_extract_urls(Body, []),
	Urls		 = lists:map(fun(X) -> url_makeAbsolute(Url, X) end, RelativeUrls),
	Result 		 = rule_spawn_and_collect( Request, Urls ),
	ResultString = response_to_string(Result),
	lists:map(fun(X) -> file:write_file(X, ResultString) end, Filenames),
	Result;
rule_actor( {save, Name, Filenames}, [{Rulename, PID, Url}|Request] ) ->
	{ok, "200", Headers, Body} = ibrowse:send_req(Url, [], get),
	lists:map(fun(X) -> file:write_file(X, Body) end, Filenames),
	{Url, Filenames};
rule_actor( {rewrite, Name, Translations}, Request ) ->
	rule_spawn_and_collect( Request, Translations ).

rule_spawn_and_collect( Request, Urls ) ->
	{ok, Responses, Count}	= rule_spawner( Request, Urls, [], 0 ),
	PopulatedResponses		= rule_response_collector( Responses, Count ),
	{ok, Request, PopulatedResponses}.

rule_spawner( Request, [], Responses, Count ) ->
	{ok, Responses, Count};
rule_spawner( Request, [Url|Urls], Responses, Count ) ->
	[{Name, _, OldUrl}|_] = Request,
	NewId = [{Name, erlang:self(), Url}|Request],
%% TODO: Check that there is no cycling
	router ! { request, NewId },
	rule_spawner( Request, Urls, [{Url}|Responses], Count + 1 ).

rule_response_collector( Responses, 0 ) ->
	Responses;
rule_response_collector( Responses, Outstanding ) ->
%% Collect the result of requests for Urls
	receive
		{ok, Request, Response} ->
			[{_, _, Url}|_] = Request, 
			NewResponses = lists:keyreplace(Url, 1, Responses, Response),
			rule_response_collector( NewResponses, Outstanding - 1 )
	end.

% Crawl helpers
get_page(URL) ->
	process_page( http:request(URL) ).

process_page( { ok, {_Status, _Headers, Body }} ) ->
	lists:subtract(lists:subtract(Body, "// [ "), "] ");
process_page( {error,no_scheme} ) ->
	io:format( "No scheme error" ).

parse_quoted(Input) ->
	[Quote|Tail] = Input,
	Quoted = lists:takewhile(fun(X) -> X =/= Quote end, Tail),
	{Quoted, lists:nthtail(length(Quoted), Tail)}.
html_extract_urls( [], Urls ) ->
	lists:reverse(Urls);
html_extract_urls( HTML, Urls ) ->
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
		47 -> Protocol ++ "://" ++ Host ++ filename:join(["/", Link]); % '/'
		63 -> Protocol ++ "://" ++ Host ++ filename:join(["/", Path]) ++ Link; % '?'
		35 -> Protocol ++ "://" ++ Host ++ filename:join(["/", Path]) ++ "?" ++ Params; % '#'
		_ ->
			case lists:member(58, Link) of % ':'
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
	{Protocol, [_|[_|Url]]} = partition(58, Uri),	% Split on ':'
	{Request, Params} = partition(63, Url),	% Split on '?'
	{Host, Path} = partition(47, Request),	% Split on '/'
	{Protocol, Host, Path, Params}.

partition(Delim, Str) ->
	{Str1, Str2} = lists:splitwith(fun(A) -> A =/= Delim end, Str),
	if hd(Str2) =:= Delim -> {Str1, tl(Str2)};
		true -> {Str1, Str2}
	end.

% Rule helpers
templates_run( Map, Templates ) ->
	% Run a list of templates with the variable definitions to return a list of strings
	lists:map(fun(X) -> sgte:render_str(X, Map) end, Templates).
find_rule( [], _ ) ->
	{undef};
find_rule( [Rule|Rules], Url ) ->
	{Type, Name, Translation} = Rule,
	{Regex, Atoms, Templates} = Translation,
	case re:run(Url, Regex, [global, {capture, Atoms, list}]) of
		{match, Subpatterns} ->
			MatchedRule = {Type, Name, templates_run(lists:zip(Atoms, Subpatterns), Templates)},
			{ok, MatchedRule};
		nomatch ->
			find_rule( Rules, Url )
	end.

response_to_string(Value) -> response_to_string( Value, "", 0 ).
response_to_string({Url, []}, Acc, Depth) ->
	"";
response_to_string({Url, [{Url, Response}|Rest]}, Acc, Depth) ->
	"";
response_to_string({Url, [{Url, Response}|Rest]}, Acc, Depth) ->
	"";
response_to_string({Url, [Filename|Filenames]}, Acc, Depth) ->
	"".