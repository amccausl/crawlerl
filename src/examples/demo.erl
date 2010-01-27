-module( demo ).
-export( [test_string_generate/3, test_subpattern/0, extract_atoms/1, template_string/0, test_compile/0, load_file/0, trans_compile/1, trans_run/2] ).

% generate filename from formula and regex matched
test_string_generate(String, Regex, Template) ->
  Atoms = extract_atoms( Regex ),
  Binding = 
    case re:run( String, Regex, [global, {capture, Atoms, list}] ) of
      {match, Subpatterns} -> lists:zip( Atoms, lists:append( Subpatterns ));
      _                    -> []
    end,
  {ok, SEL} = sgte:compile(Template),
  sgte:render_str(SEL, Binding).

% must extract portions of the regex that 
test_subpattern() ->
  re:run( "http://google.ca", "^http://(?<host>[^/]*)(?<path>/.*)*$" ).

extract_atoms(Regex) ->
  case re:run( Regex, ets:lookup_element(?MODULE, namedRegex, 0), [global, {capture, [1], list}] ) of
    {match, Subpatterns} -> map(fun list_to_atom/1, lists:append(Subpatterns));
    _                    -> []
  end.

template_string() ->
  {ok, SEL} = sgte:compile("host: $host$, path: $path$;~n"),
  sgte:render_str(SEL, [{host, "host"}, {path, ["path"]}]).

test_compile() ->
  {ok, MP} = re:compile("http://(?<host>.*)", []),
  re:run( "http://google.ca", MP ).

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

load_file() ->
  ConfFile = filename:join(["..", "..", "config", "crawler_dispatcher.conf"]),
  {ok, Terms} = file:consult(ConfFile),
  {ok, NamedRegex} = re:compile("\\?\\<([A-Za-z_0-9]+)\\>", []),
  ets:new(?MODULE, [named_table, public]),
  ets:insert(?MODULE, [ {namedRegex, NamedRegex} ]),
  Rules = map(fun preprocess/1, Terms),
  ets:insert(?MODULE, [ {rules, Rules} ]),
  {ok}.

preprocess({Action, Name, Translation, Params}) ->
  {Action, Name, trans_compile(Translation), Params}.

trans_compile({Regex, Filename}) ->
  {erlang:element(2, re:compile(Regex)), extract_atoms(Regex), erlang:element(2, sgte:compile(Filename))}.

trans_run(Input, {Regex, Atoms, Template}) ->
  {match, Subpatterns} = re:run(Input, Regex, [global, {capture, Atoms, list}]),
  sgte:render_str(Template, lists:zip(Atoms, Subpatterns)).