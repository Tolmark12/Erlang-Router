%% Author: ryan
%% Created: Sep 14, 2010
%% Description: TODO: Add description to listener
-module(listener).

%%
%% Include files
%%
-include("../include/common.hrl").

%%
%% Exported Functions
%%
-export([start/0, start/1]).

%%
%% API Functions
%%

start() ->
	start(8000).
start(Port) ->
	{ok, Listen} = gen_tcp:listen(Port, [binary, {packet, http},
										 {reuseaddr, true},
										 {active, false},
										 {packet_size, 1024}]),
	spawn(fun () -> connect(Listen) end),
	Listen.

%%
%% Local Functions
%%

connect(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			spawn(fun () -> connect(Listen) end),
			loop(Socket);			
		{error, emfile} ->
			reset(Listen);
		{error,econnreset} ->
			reset(Listen);
		{error,closed} ->
			start()
	end.

loop(Socket) ->
	[{header,Header}, {header_record,HeaderRecord}] = http_handler:receive_header(Socket),
	Body = case HeaderRecord#headers.request_type of
		'POST' -> http_handler:receive_body(Socket, HeaderRecord#headers.content_length);
		'PUT'  -> http_handler:receive_body(Socket, HeaderRecord#headers.content_length);
		_      -> <<>>
	end,
	router:forward_request({request, Socket, [Header,Body], HeaderRecord#headers.host}),
	self()!stop.

reset(Listen) ->
	gen_tcp:close(Listen),
	start(),
	self()!stop.
