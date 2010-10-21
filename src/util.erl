-module(util).

-export([to_atom/1]).


to_atom(Value) when is_atom(Value) ->
	Value;
to_atom(Value) when is_list(Value) ->
	list_to_atom(Value).
