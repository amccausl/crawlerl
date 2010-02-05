%%% @author Alex McCausland <alex.mccausland@gmail.com>
%%% @doc 
%%% This is a learning excerize in the language.
%%%
%%% start() -> Parse parameters from command line, call init, run dispatcher
%%% init() -> configure http client, load config, start dispatcher (register(dispatcher, Dispatcher))
%%% dispatcher() -> Store rules for downloader
%%%  rule must include callbacks, save location (or undef), number of threads
%%%  use register(Name, PID) and whereis(Name) to find actor for each rule
%%% rule_actor(Rule) -> knows action to take for file, spawns threads to take action,
%%%  possible actions: save(fun filename) save url to disk, crawl([Tags]) takes array of tags to call dispatcher for
%%%  creates number of threads
%%%  accepts messages from threads ('DOWN', Ref, process, Pid2, Reason), and from dispatcher (url, URL)
%%% downloader 
%%% crawler
%%% @end

% main actor to hit param url, download threads

% as thread is parsed, spawn or pass messages as needed to download contents

% each parser should register with the name from the config

%get_filename( {URL, Headers} ) ->

-module( crawler ).
-export( [ start/0, start/1, get_page/1, dispatcher_init/1 ] ).

% start with url on cmd line, maybe some preprocessing
start() ->
  init(),
  {ok, URLs} = init:get_arguments( url ),
  register(crawler_dispatcher, spawn( crawler, dispatcher_init, URLs )).

start(URL) ->
  init(),
  register(crawler_dispatcher, spawn( crawler, dispatcher_init, [URL] )).

init() ->
  inets:start(),
  crawler_config:load(),
  % configure http client
  % create processes for each parser

dispatcher([URL|Rest]) ->
  self() ! { url, URL },
  dispatcher_init(Rest);
dispatcher([]) ->
  dispatcher_re().

dispatcher_re() ->
  receive
    { url, URL } ->
      process(URL),
      dispatcher_re()
  end.

process(URL) ->
  % pass to rule_manager (parent PID, ChildArray, Threads)
  crawler_dispatcher ! {url, URL}. 

rule_manager({crawl, Name, Pattern, Args}) ->
  end.

rule_actor(Rule, NumThreads) ->
  receive
    { url, URL } ->
  end.

receiver(Rule, Queue, Threads) ->
  % if threads > 0, queue not empty, process from queue
  receive
    { url, URL } ->
      downloader(Rule, URL),
      receiver(Rule, Queue, Threads - 1);
    {'DOWN', Ref, process, Pid, Reason} ->
	  receiver( Rule, Queue, Threads + 1 )
  end.

downloader(Rule, URL) ->
  io:format("blah~n").

get_page(URL) ->
  process_page( http:request(URL) ).

process_page( { ok, {_Status, _Headers, Body }} ) -> lists:subtract(lists:subtract(Body, "// [ "), "] ");
process_page( {error,no_scheme} ) -> io:format( "No scheme error" ).

crawl( ParentID, RuleID, Url ) ->
  {ok, {_Status, _Headers, HTML}} = http:request(Url),
  RelativeUrls = parse_url( HTML, [] ),
  Urls = lists:map(fun(X) -> url_makeAbsolute(X) end, RelativeUrls), 
  crawl_re(ParentID, Urls, Threads, 1).

crawl_re( ParentID, [], Threads, ThreadCount, Index ) ->
	receive
		{callback, PID, Index, Url, Filename} ->
			crawl_re( ParentID, [],  )lists;
    	{'DOWN', Ref, process, Pid, Reason} ->	
	end;
crawl_re( ParentID, [Url|Urls], Threads, ThreadCount, Index ) when ThreadCount > 0 ->
	NewThread = { spawn( crawler, )},
	crawl_re( ParentID, Urls, [NewThread|Threads], ThreadCount - 1, Index + 1 ).

% Final

rule_crawl(Rule, [], Threads, ThreadCount) ->
	receive
		{callback, Pid, Index, Url} ->
rule_crawl(Rule, Queue, Threads, ThreadCount) when ThreadCount > 0 ->
	.

rule_save(Rule, [], Threads, ThreadCount) when ThreadCount > 0 ->
	receive
		{url, Request} ->
	end;
rule_save(Rule, Queue, Threads, ThreadCount) ->
	receive
		{url, Request} -> rule_save( Rule, [Request|Queue], Threads, ThreadCount);
		{callback, Pid, Index, }
	end;
rule_save(Rule, Queue, Threads, ThreadCount) when ThreadCount > 0 ->
	receive
	end.

save_thread(Url, Filename) ->
	.

rule_rewrite(Rule) ->
	receive
		{url, Request} ->
	end.

rewrite(Url, []) -> {ok}. 
rewrite(Url, [Translation|Rest]) ->
	

get_rule_process(Url) ->
	.

% Crawl helpers
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