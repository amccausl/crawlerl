-module(crawler_config).
-export([ load/0, ruleLookup/1 ]).

% entries should include: action, url regex, thread count

load() ->
  ConfFile = filename:join(["..", "config", "crawler_rules.conf"]),
  {ok, Rules} = file:consult(ConfFile),
  {ok, NamedRegexp} = re:compile("\\?\\<[A-Za-z_0-9]+\\>", [ungreedy]),

  Terms1 = lists:map(fun preprocess/1, Terms),
  ets:new(?MODULE, [named_table, public]),
  ets:insert(?MODULE, [ {rules, lists:map(fun(X) -> parser(X, NamedRegexp) end, Terms1)} ]).

%% main handler: it selects the first match in patterns list
%% and properly processes it
ruleLookup(URL) ->
  % find rule
  [{rules, Config}] = ets:lookup(?MODULE, rules),
  Selector = fun(X) -> selector(URL, X) end,
  Action = case filter(Selector, Patterns) of
    {ok, A} -> A;
    nomatch -> nomatch
  end,
  process(Action, URL).

-spec(selector/2 :: (string(), tuple()) -> bool()).	     
selector(Element, {_, Regexp, _, []}) -> 
    selector_exec(Element, Regexp);
selector(Element, {_, Regexp, _, Opts}) ->
    selector_exec(Element, Regexp, Opts).

-spec(selector_exec/2 :: (string(), tuple()) -> bool()).	     
selector_exec(Element, Regexp) ->
     case re:run(Element, Regexp, [{capture,first}]) of
	{match, _} ->
	    true;
	nomatch ->
	    false;
	{error, Reason} ->
	    exit({?MODULE, Reason})
    end.

-spec(selector_exec/3 :: (string(), tuple(), list(tuple())) -> bool()).	     
selector_exec(Element, Regexp, Opts) ->
    case lists:keysearch(named_subpatterns, 1, Opts) of
	false ->
	    selector_exec(Element, Regexp);
	{_, {_, Names}} ->
	    case re:run(Element, Regexp, [{capture, Names, list}]) of
		nomatch ->
		    false;
		match ->
		    true;
		{match, Matched} ->
		    Zipped = lists:zip(Names, Matched),
		    eptic:fset("__dispatcher_params", Zipped),
		    
		    true
	    end
    end.

%% Returns first element which satisfies Fun
-spec(filter/2 :: (fun(), list(tuple())) -> {ok, tuple()} | nomatch).
filter(_Fun, []) ->
    nomatch;
filter(Fun, [First|Rest]) ->
    case Fun(First) of
    true ->
        {ok, First};
        false ->
        filter(Fun, Rest)
    end.
