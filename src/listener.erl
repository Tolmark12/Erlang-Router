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
	{ok, Listen} = gen_tcp:listen(8000, [binary, {packet, http},
										 {reuseaddr, true},
										 {active, false},
										 {packet_size, 1024}]),
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
			reset(Listen);
		{error,econnreset} ->
			reset(Listen);
		{error,closed} ->
			start()
	end.

loop(Socket) ->
	{Host, Request} = receive_request(Socket),
	router:forward_request({request, Socket, Request, Host}),
	self()!stop.

reset(Listen) ->
	gen_tcp:close(Listen),
	start(),
	self()!stop.
receive_request(Socket) ->
	receive_request(Socket, "", '').	
receive_request(Socket, Request, Host) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, {http_request, Type, {_, Path}, {VersionMajor, VersionMinor}} } ->
			receive_request(Socket, [atom_to_list(Type), " ", Path, " HTTP/", io_lib:format("~p", [VersionMajor]),".",io_lib:format("~p", [VersionMinor]),"\r\n"], Host);
		{ok, {http_header, _Num, 'Host', _, Value} } ->
			receive_request(Socket, [Request,"Host: ",Value,"\r\n"], Value);
		{ok, {http_header, _Num, Key, _, Value} } when is_atom(Key) ->
			receive_request(Socket, [Request, atom_to_list(Key),": ",Value,"\r\n"], Host);
		{ok, {http_header, _Num, Key, _, Value} } ->
			receive_request(Socket, [Request, Key,": ",Value,"\r\n"], Host);
        {error, {http_error, Socket, "\r\n"}} ->
            receive_request(Socket, Request, Host);
        {error, {http_error, Socket,"\n"}} ->
            receive_request(Socket, Request, Host);
        {error, {http_error, Socket, _}} ->
            bad_request;
		{ok, http_eoh} ->
			{Host, [Request, "\r\n"]}
	end.