-module(crawler_config).
-export([ load_rules/0 ]).

load_rules() ->
	ConfFile = filename:join(["..", "config", "crawler_rules.conf"]),
	{ok, Rules} = file:consult(ConfFile),
	{ok, NamedRegex} = re:compile("\\?\\<[A-Za-z_0-9]+\\>", [ungreedy]),
	ets:new(?MODULE, [named_table, public]),
	ets:insert(?MODULE, [ {namedRegex, NamedRegex} ]),

	CompiledRules = lists:map(fun preprocess/1, Rules),
	{ok, CompiledRules}.

preprocess({Action, Name, Regex, Translations, _}) ->
	{Action, Name, trans_compile(Regex, Translations)};
preprocess({Action, Name, Regex, Translations}) ->
	{Action, Name, trans_compile(Regex, Translations)}.

trans_compile(Regex, Translations) ->
	{ok, CompiledRegex} = re:compile(Regex),
	CompiledTemplates = lists:map(fun(X) -> erlang:element(2, sgte:compile(X)) end, Translations),
	{CompiledRegex, extract_atoms(Regex), CompiledTemplates}.

extract_atoms(Regex) ->
	case re:run( Regex, ets:lookup_element(?MODULE, namedRegex, 2), [global, {capture, [1], list}] ) of
		{match, Subpatterns}	-> lists:map(fun list_to_atom/1, lists:append(Subpatterns));
		_						-> []
	end.