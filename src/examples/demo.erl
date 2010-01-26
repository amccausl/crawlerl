-module( demo ).
-export( [test_string_generate/3, test_subpattern/0, extract_atoms/1, template_string/0] ).

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
  case re:run( Regex, "\\?\\<([a-z0-9]*)\\>", [global, {capture, [1], list}] ) of
    {match, Subpatterns} -> map(fun list_to_atom/1, lists:append(Subpatterns));
    _                    -> []
  end.

template_string() ->
  {ok, SEL} = sgte:compile("host: $host$, path: $path$;~n"),
  sgte:render_str(SEL, [{host, "host"}, {path, ["path"]}]).

test_compile() ->
  re:compile("http://(?<host>.*)", []).

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

% TODO: TEST COMPILED STUFF
