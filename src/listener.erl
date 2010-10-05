%% Author: ryan
%% Created: Sep 14, 2010
%% Description: TODO: Add description to listener
-module(listener).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

start() ->
	{ok, Listen} = gen_tcp:listen(8000, [binary, {packet, 0},
										 {reuseaddr, true},
										 {active, true},
										 {backlog, 100}]),
	spawn(fun () -> connect(Listen) end).

%%
%% Local Functions
%%

connect(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} -> 
			spawn(fun () -> connect(Listen) end),
			loop(Socket);			
		{error, emfile} ->
			io:format("listener: emfile error~n"),
			reset(Listen);
		{error,econnreset} ->
			io:format("listener: Connection reset~n"),
			reset(Listen);
		{error,closed} ->
			start()
	end.

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			router:forward_request({request, Socket, Bin}),
			loop(Socket);
		{tcp_closed, Socket} ->
			ok
		after 1000 ->
			ok
	end,
	self()!stop.

reset(Listen) ->
	gen_tcp:close(Listen),
	start(),
	self()!stop.