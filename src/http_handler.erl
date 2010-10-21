%% Author: ryan
%% Created: Sep 14, 2010
%% Description: TODO: Add description to listener
-module(http_handler).

%%
%% Exported Functions
%%
-export([receive_header/1, receive_body/2]).

%%
%% Include files
%%
-include("../include/common.hrl").

receive_header(Socket) ->
	receive_header(Socket, "", #headers{}).
receive_header(Socket, Header, HeaderRecord) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, {http_request, Type, {_, Path}, {VersionMajor, VersionMinor}} } ->
			receive_header(Socket, [atom_to_list(Type), " ", Path, " HTTP/", integer_to_list(VersionMajor),".",integer_to_list(VersionMinor),"\r\n"], 
			HeaderRecord#headers{request_type=Type, request_path=Path});
		{ok, {http_response,{VersionMajor,VersionMinor},Number,Msg}} ->
			receive_header(Socket, ["HTTP/",integer_to_list(VersionMajor),".",integer_to_list(VersionMinor),
			 	" ",integer_to_list(Number) ," ",Msg, "\r\n"], HeaderRecord);
		{ok, {http_header, _Num, 'Host', _, Value} } ->
			receive_header(Socket, [Header,"Host: ",Value,"\r\n"], HeaderRecord#headers{host=Value});
		{ok, {http_header, _Num, 'Content-Length', _, Value} } ->
			{Int, _} = string:to_integer(Value),
			receive_header(Socket, [Header,"Content-Length: ",Value,"\r\n"], HeaderRecord#headers{content_length=Int});
		{ok, {http_header, _Num, Key, _, Value} } when is_atom(Key)  ->
			receive_header(Socket, [Header, atom_to_list(Key),": ",Value,"\r\n"], HeaderRecord);
		{ok, {http_header, _Num, Key, _, Value} }  ->
			receive_header(Socket, [Header, Key,": ",Value,"\r\n"], HeaderRecord);
        {ok, {http_error, "\r\n"}} ->
            receive_header(Socket, Header, HeaderRecord);
        {ok, {http_error, "\n"} }->
            receive_header(Socket, Header, HeaderRecord);
        {ok, {http_error, _}} ->
            bad_request;
		{ok, http_eoh} ->
			[{header,[Header, "\r\n"]}, {header_record,HeaderRecord}]
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
					receive_body(Socket, list_to_binary([Body, Bin]), Length)
			end;
	   {error, timeout} ->
			Body;
	   {error, closed} ->
			Body
	end.
