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
	Filename = filename:join(["..", "data", "save_test"]),
	file:delete(Filename),
	ibrowse:start(),
	{ok, _, _, Body} = ibrowse:send_req(Url, [], get),
	HTML		 = lists:subtract(lists:subtract(Body, "// [ "), "] "),
	RelativeUrls = crawler:html_extract_urls(HTML, []),
	Urls		 = lists:map(fun(X) -> crawler:relativeurl_to_uri(X, crawler:url_to_uri(Url)) end, RelativeUrls),
	ResultString = lists:foldl(fun(A, Acc) -> Acc ++ "\n" ++ A end, Url, Urls),
	file:write_file(Filename, ResultString).

url_to_uri_test() ->
	{"http", "localhost", "", ""} = crawler:url_to_uri("http://localhost"),
	{"http", "localhost", "path/", ""} = crawler:url_to_uri("http://localhost/path/"),
	{"http", "localhost", "", "param=value"} = crawler:url_to_uri("http://localhost?param=value"),
	{"http", "localhost", "path", "param=value"} = crawler:url_to_uri("http://localhost/path?param=value").

rule_actor_rewrite_test() ->
	"".

run_test() ->
	crawler:start(),
	crawler:run("http://localhost/scala").

router_test() ->
	crawler:start(),
	router ! {status},
	router ! {request, {[], "http://localhost"}},
	receive
		Msg -> io:format("router_test: ~w~n", [Msg])
	end.

url_filter_test() ->
	SourceURL = "http://localhost/scala",
	true = crawler:url_filter( SourceURL, "http://localhost/scala/src", {sub_only} ),
	false = crawler:url_filter( SourceURL, "http://localhost/", {sub_only} ),
	false = crawler:url_filter( SourceURL, "http://localhost/icons/folder.gif", {sub_only} ),
	true = crawler:url_filter( SourceURL, "http://localhost/scala?C=N;O=D", {sub_only} ),
	false = crawler:url_filter( SourceURL, "http://google.ca/scala", {local_only} ).
