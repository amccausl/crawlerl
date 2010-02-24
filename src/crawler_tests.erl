-module( crawler_tests ).
-include_lib("eunit/include/eunit.hrl").

load_test() ->
	{ok, CompiledRules} = crawler_config:load_rules(),
	CompiledRules.

ibrowse_test() ->
	ibrowse:start(),
	ibrowse:send_req("http://localhost", [], get).

rule_actor_save_html_test() ->
	Filename = filename:join(["..", "data", "save_test"]),
	file:delete(Filename),
	Rule = {save, save_test, [Filename]},
	Request = {save_test, erlang:self(), "http://localhost"},
	crawler:rule_actor( Rule, [Request] ).

rule_actor_save_bin_test() ->
	Filename = filename:join(["..", "data", "save_test2.png"]),
	file:delete(Filename),
	Rule = {save, save_test, [Filename]},
	Request = {save_test, erlang:self(), "http://localhost/feh_006977_000001_w3mtmp6966-1.png"},
	crawler:rule_actor( Rule, [Request] ).

rule_actor_crawl_test() ->
	Url = "http://localhost/",
	Uri = crawler:uri_parse(Url),
	Filename = filename:join(["..", "data", "save_test"]),
	file:delete(Filename),
	ibrowse:start(),
	{ok, _, _, Body} = ibrowse:send_req(Url, [], get),
	HTML		 = lists:subtract(lists:subtract(Body, "// [ "), "] "),
	RelativeUrls = crawler:html_extract_urls(HTML, []),
	Urls		 = lists:map(fun(X) -> crawler:url_makeAbsolute(Uri, X) end, RelativeUrls),
	ResultString = lists:foldl(fun(A, Acc) -> Acc ++ "\n" ++ A end, Url, Urls),
	file:write_file(Filename, ResultString).

uri_parse_test() ->
	{"http", "localhost", "", ""} = crawler:uri_parse("http://localhost"),
	{"http", "localhost", "path/", ""} = crawler:uri_parse("http://localhost/path/"),
	{"http", "localhost", "", "param=value"} = crawler:uri_parse("http://localhost?param=value"),
	{"http", "localhost", "path", "param=value"} = crawler:uri_parse("http://localhost/path?param=value").

rule_actor_rewrite_test() ->
	"".

router_test() ->
	crawler:start(),
	router ! {status},
	router ! {request, {[], "http://localhost"}},
	receive
		Msg -> io:format("router_test: ~w~n", [Msg])
	end.