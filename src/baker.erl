-module(baker).
-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
	router:start().

stop(_State) ->
	router:stop(),
	ok.
