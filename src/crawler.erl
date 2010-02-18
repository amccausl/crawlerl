%%% @author Alex McCausland <alex.mccausland@gmail.com>

% TODO: finish and test config loader and matcher
% TODO: spawn manager threads for each rule
% TODO: router should prepend ID information
% TODO: write tests for rewrite functionality
% TODO: finish load and config functionality
% TODO: sketch out rule_crawl
% TODO: sketch out rule_save
% TODO: add index file handling
% TODO: should store each processed url in each thread
% TODO: on launch, should check process cache, then translated filename (if local) to determine if fetch has already occured
% TODO: figure out EUnit
% TODO: add in cycling detection for crawlers (examine id for occurrences of the same rule, if exists, spawn thread to resolve)

-module( crawler ).
-export( [ start/0, router/1, rule_actor/2 ] ).

% start with url on cmd line, maybe some preprocessing
start() ->
	init(),
	{ok, URLs} = init:get_arguments( url ),
	Rules = crawler_config:load_rules(),
	PID = spawn( crawler, router, Rules ),
	register(router, PID),
	route(PID, URLs).

route( _, [] ) ->
	{ok};
route( PID, [Url|Urls] ) ->
	PID ! {url, Url},
	route( PID, Urls ).

init() ->
  inets:start().
  % configure http client

router(Rules) ->
	receive
		{url, Url} ->
			case find_rule( Rules, Url ) of
				{undef} ->
					io:format("No rule for url: ~s/n", [Url]),
					router(Rules);
				{Type, Name, Params} ->
					io:format("Matched '~s' to rule '~s'", [Url, Name]),
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

%% Rule		= {crawl, Name, Translation}
%% Rule		= {save, Name, Translation}
%% Rule		= {rewrite, Name, Translation}
%% Request	= Id
%% Id		= [{RuleName, PID, Index, Url}] - A Unique id for every request, a heirarchy of rule ids and urls (should be useful for cycling)
%% Response = {Url, Filename}
%%			= {Url, Responses}
rule_actor( {crawl, Name, Translation}, [{Rulename, PID, Index, Url}|Rest] ) ->
	Request		 = [{Rulename, PID, Index, Url}|Rest],
	Filename	 = trans_run( Url, Translation ),
	Body		 = get_page(Url),
	RelativeUrls = parse_url(Body, []),
	Urls		 = lists:map(fun(X) -> url_makeAbsolute(Url, X) end, RelativeUrls),
	rule_crawl(Urls, Request, [], 0);
rule_actor( {save, Name, Translation}, [{Rulename, PID, Index, Url}|Request] ) ->
	Filename = trans_run( Url, Translation ),
	{ok, Status, Headers, Body} = ibrowse:send_req(Url, [], get),
	file:write_file(Filename, Body),
	{Url, Filename};
rule_actor( {rewrite, Name, Translations}, Request ) ->
	rule_rewrite( Translations, Request, [], 0 ).

rule_crawl([], Request, Responses, 0) ->
%% All urls have been crawled
	{ok, Request, Responses};
rule_crawl([], Request, Responses, Index) ->
%% All urls have been dispatched, awaiting results
	receive
		{callback, [{_, _, Url}|_], Result} ->
			NewResponses = lists:keyreplace(Url, 1, Responses, Result),
			rule_crawl( [], Request, NewResponses, Index - 1 )
	end;
rule_crawl([Url|Urls], Request, Responses, Index) ->
%% Generate requests for each of the Urls
	[{Name, _, _, _}|_] = Request,
	NewId	= [{Name, erlang:self(), Url}],
	router ! { NewId },
	rule_crawl( Urls, Request, [{Url}|Responses], Index + 1).

% Rules to rewrite urls by patters
rule_rewrite( [], Request, Responses, 0 ) ->
%% All rewrites have completed
	{ callback, Request, Responses };
rule_rewrite( [], Request, Responses, Index ) ->
%% All rewrites have been generated, waiting for responses
	receive
		{callback, [{_, _, Url}|_], Result} ->
			NewResponses = lists:keyreplace(Url, 1, Responses, Result),
			rule_rewrite( [], Request, NewResponses, Index - 1)
	end;
rule_rewrite( [Translation|Translations], Request, Responses, Index ) ->
%% Generate requests for each of the translations
	[{Name, _, _, Url}|_] = Request,
	NewUrl	= trans_run( Url, Translation ),
	NewId	= [{Name, erlang:self(), NewUrl}|Request],
	router ! { NewId },
	rule_rewrite( Translations, Request, [{NewUrl}|Responses], Index + 1 ).

% Crawl helpers
get_page(URL) ->
  process_page( http:request(URL) ).
process_page( { ok, {_Status, _Headers, Body }} ) -> lists:subtract(lists:subtract(Body, "// [ "), "] ");
process_page( {error,no_scheme} ) -> io:format( "No scheme error" ).
parse_quoted(Input) ->
	[Quote|Tail] = Input,
	Quoted = lists:takewhile(fun(X) -> X =/= Quote end, Tail),
	{Quoted, lists:sublist(Tail, length(Quoted) + 1, 100000)}.
parse_url( [], Urls ) ->
	lists:reverse(Urls);
parse_url( HTML, Urls ) ->
	case lists:prefix("src=", HTML) of
		true ->
			{Url, Remaining} = parse_quoted(lists:sublist(HTML, 5, 100000)),
			parse_url(Remaining, [Url|Urls]);
		false ->
			case lists:prefix("href=", HTML) of
				true ->
					{Url, Remaining} = parse_quoted(lists:sublist(HTML, 6, 100000)),
					parse_url(Remaining, [Url|Urls]);
				false ->
					[_|T] = HTML,
					parse_url(T, Urls)
			end
	end.
url_makeAbsolute( SourceUrl, Url ) ->
	case lists:suffix("http://", Url) of
		true -> Url;
		false ->
			Dir = url_getDirectory( SourceUrl ),
			Dir ++ Url
	end.
url_getDirectory( Url )	->
	% take everything before the '?'
	Base = lists:takewhile(fun(X) -> X =/= 63 end, Url),
	case lists:suffix("/", Base) of
		true ->
			Base;
		false ->
			RUrl = lists:reverse(Base),
			lists:reverse(lists:dropwhile(fun(X) -> X =/= 47 end, RUrl))
	end.
% Rule helpers
trans_run(Input, {Regex, Atoms, Template}) ->
  {match, Subpatterns} = re:run(Input, Regex, [global, {capture, Atoms, list}]),
  sgte:render_str(Template, lists:zip(Atoms, Subpatterns)).
find_rule( Rules, Url ) ->
	{undef}.