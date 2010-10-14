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
	[{request, Request}, {headers, Headers}] = receive_request(Socket),
	Body = case Headers#headers.request_type of
		'POST' -> receive_body(Socket, Headers#headers.content_length);
		'PUT'  -> receive_body(Socket, Headers#headers.content_length);
		_      -> <<>>
	end,
	router:forward_request({request, Socket, [Request,Body], Headers#headers.host}),
	self()!stop.

reset(Listen) ->
	gen_tcp:close(Listen),
	start(),
	self()!stop.
receive_request(Socket) ->
	Headers = #headers{},
	receive_request(Socket, "", Headers).	
receive_request(Socket, Request, HeaderList) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, {http_request, Type, {_, Path}, {VersionMajor, VersionMinor}} } ->
			receive_request(Socket, [atom_to_list(Type), " ", Path, " HTTP/", io_lib:format("~p", [VersionMajor]),".",io_lib:format("~p", [VersionMinor]),"\r\n"], HeaderList#headers{request_type=Type});
		{ok, {http_header, _Num, 'Host', _, Value} } ->
			receive_request(Socket, [Request,"Host: ",Value,"\r\n"], HeaderList#headers{host=Value});
		{ok, {http_header, _Num, 'Content-Length', _, Value} } ->
			{Int, _} = string:to_integer(Value),
			receive_request(Socket, [Request,"Content-Length: ",Value,"\r\n"], HeaderList#headers{content_length=Int});
		{ok, {http_header, _Num, Key, _, Value} } when is_atom(Key) ->
			receive_request(Socket, [Request, atom_to_list(Key),": ",Value,"\r\n"], HeaderList);
		{ok, {http_header, _Num, Key, _, Value} } ->
			receive_request(Socket, [Request, Key,": ",Value,"\r\n"], HeaderList);
        {error, {http_error, Socket, "\r\n"}} ->
            receive_request(Socket, Request, HeaderList);
        {error, {http_error, Socket,"\n"}} ->
            receive_request(Socket, Request, HeaderList);
        {error, {http_error, Socket, _}} ->
            bad_request;
		{ok, http_eoh} ->
			lists:append([{request, [Request, "\r\n"]}], [{headers,HeaderList}])
	end.
receive_body(Socket, Length) ->
	receive_body(Socket, <<>>, Length).	
receive_body(Socket, Body, Length) ->
   	inet:setopts(Socket, [{packet, 0}]),
	case gen_tcp:recv(Socket, Length, 500) of
		{ok, Bin} ->
			if 
				Length > 0 ->
					Bin;
				true ->
					receive_body(Socket, list_to_binary([Body, Bin]))
			end;
	   {error, timeout} ->
			Body;
	   {error, closed} ->
			Body
	end.
