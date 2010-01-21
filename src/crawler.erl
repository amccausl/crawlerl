-module( crawler ).
-export( [ receiver/0, giver/1, start/0, test_parse/1, get_page/1 ] ).
-include_lib("xmerl/include/xmerl.hrl").

% start with url on cmd line, maybe some preprocessing
start() ->
  inets:start(),
  RouterPID = spawn( crawler, router, [] ),
  {ok, [URLs]} = init:get_arguments( url ).
%start_test(URL) ->

router() ->
  receive
    { url, URL } ->
    %{ img, IMG } ->
  end.

% main actor to hit param url, download threads

% as thread is parsed, spawn or pass messages as needed to download contents



receiver() ->
  receive
    diediedie -> ok;
    { name, Name } -> io:format( "hello, ~s~n", [ Name ] ), receiver()
  end.
giver( ReceiverPid ) ->
  ReceiverPid ! { name, "Andre" },
  ReceiverPid ! { name, "Linux.conf.au" },
  ReceiverPid ! diediedie.
start() ->
  ReceiverPid = spawn( hello_concurrent, receiver, [] ),
  spawn( hello_concurrent, giver, [ ReceiverPid ] ),
  start_finished.

test_parse(URL) ->
  inets:start(),
  { ok, {_Status, _Headers, Body}} = http:request(URL),
  xmerl_scan:string( xmerl_ucs:to_utf8(Body) ).
  

get_page(URL) ->
  %% Don't know why I need the following line
  %% mentioned in inets documentation
  inets:start(),
  process_page( http:request(URL) ).

%get_filename( {URL, Headers} ) ->

process_page( { ok, {_Status, _Headers, Body }} ) -> lists:subtract(lists:subtract(Body, "// [ "), "] ");
process_page( {error,no_scheme} ) -> io:format( "No scheme error" ).


% start() -> Parse parameters from command line, start router, configure http client
% router() -> Store rules for downloader
 % rule must include callbacks, save location (or undef), 
% parse_with_callback(Body) -> Attempt to scan HTML to xmerl data structure
 % when tag with callback defined is closed, pass message
 % callbacks store PID of router, pass messages back up
