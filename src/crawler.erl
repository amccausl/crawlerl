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
-include_lib("xmerl/include/xmerl.hrl").

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
