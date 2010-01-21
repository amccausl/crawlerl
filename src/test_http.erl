-module( test_http ).
-export([get_page/1]).

get_page(URL) ->
  %% Don't know why I need the following line
  %% mentioned in inets documentation
  inets:start(),
  process_page( http:request(URL) ).

process_page( { ok, {_Status, _Headers, Body }} ) -> lists:subtract(lists:subtract(Body, "// [ "), "] ");
process_page( {error,no_scheme} ) -> io:format( "No scheme error" ).
