%%% -------------------------------------------------------------------
%%% Author  : ryan
%%% Description :
%%%
%%% Created : Sep 14, 2010
%%% -------------------------------------------------------------------
-module(router).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/0, stop/0, forward_request/1, update_app/1, add_app/1]).
-export([start_relay/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {apps}).
-record(target, {host,port}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	Router = gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]),
	listener:start(),
	Router.

stop() ->
	gen_server:call(?MODULE,stop),
	ok.

update_app({AppName, TargetList}) ->
	AppAtom = to_atom(AppName),
	gen_server:call(?MODULE, {update_app, {AppAtom, TargetList}}).

add_app({AppName, TargetList}) ->
	AppAtom = to_atom(AppName),
	gen_server:call(?MODULE, {add_app, {AppAtom, TargetList}}).

forward_request({request, Client, Request, App}) ->
	AppAtom = to_atom(App),
	gen_server:call(?MODULE, {invoke, Client, Request, AppAtom}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	{ok, #state{apps=[{'localhost:8000',[#target{host="localhost",port=3000},#target{host="127.0.0.1",port=3000}], 1}]}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({update_app, {AppName, TargetList}}, _From, #state{apps=AppList}=State) ->
	NewAppList = lists:keystore(AppName, 1, AppList, {AppName, TargetList, 1}),
	{noreply, State#state{apps=NewAppList}};
handle_call({add_app, {AppName, TargetList}}, _From, #state{apps=AppList}=State) ->
	NewAppList = lists:merge(AppList, [{AppName, TargetList, 1}]),
	{noreply, State#state{apps=NewAppList}};
handle_call({invoke, Client, Request, AppName}, _From, State) ->
	{Target, NewState} = select_target(AppName, State),
	io:format("Target = ~p~n",[Target]),
	spawn(?MODULE, start_relay, [Client, Request, Target]),
	{noreply, NewState};
handle_call(_Msg, _From, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({invoke, Client, Request, Host}, State) ->
	spawn(?MODULE, start_relay, [Client, Request, State]),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
select_target(AppName, #state{apps=Apps}=State) ->
	App = lists:keyfind(AppName, 1, Apps),
	case App of
		{_AppName,Targets, CurTarget} ->
			Target = lists:nth(CurTarget, Targets),
			if 
				CurTarget >= length(Targets) ->
					NextTarget = 1;
				true -> 
					NextTarget = CurTarget + 1
			end,
			NewAppList = lists:keystore(AppName, 1, Apps, {AppName, Targets, NextTarget}),	
			{Target, State#state{apps=NewAppList}};
		false ->
			{#target{}, State}
	end.

start_relay(Client, Request, #target{host=Host,port=Port}) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, http}, {active, false}, {packet_size, 4094}]) of
		{ok, Server} -> 
			gen_tcp:send(Server, Request),
			{Header, Length} = receive_header(Server),
			gen_tcp:send(Client, Header),
			Response = receive_response(Server, Length),
			Result = gen_tcp:send(Client, Response),
			gen_tcp:close(Server),
			gen_tcp:close(Client);
		{error, emfile} ->
			io:format("router: emfile error~n");
		{error,econnreset} ->
			io:format("router: Connection reset~n")
	end,
	self()!stop.
	
receive_header(Socket) ->
	receive_header(Socket, "", 0).
receive_header(Socket, Response, Length) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, {http_response,{VersionMajor,VersionMinor},Number,Msg}} ->
			receive_header(Socket, ["HTTP/",integer_to_list(VersionMajor),".",integer_to_list(VersionMinor),
			 	" ",integer_to_list(Number) ," ",Msg, "\r\n"], Length);
		{ok, {http_header, _Num, 'Content-Length', _, Value} } ->
			{Int, _} = string:to_integer(Value),
			receive_header(Socket, [Response, "Content-Length: ",Value,"\r\n"], Int);
		{ok, {http_header, _Num, Key, _, Value} } when is_atom(Key)  ->
			receive_header(Socket, [Response, atom_to_list(Key),": ",Value,"\r\n"], Length);
		{ok, {http_header, _Num, Key, _, Value} }  ->
			receive_header(Socket, [Response, Key,": ",Value,"\r\n"], Length);
        {ok, {http_error, "\r\n"}} ->
            receive_header(Socket, Response, Length);
        {ok, {http_error, "\n"} }->
            receive_header(Socket, Response, Length);
        {ok, {http_error, _}} ->
            bad_request;
		{ok, http_eoh} ->
			{[Response, "\r\n"], Length}
	end.

receive_response(Socket, Length) ->
   	receive_response(Socket, <<>>, Length).
receive_response(Socket, Response, Length) ->
   	inet:setopts(Socket, [{packet, 0}]),
	case gen_tcp:recv(Socket, Length, 500) of
		{ok, Bin} ->
			if
				Length > 0 -> Bin;
				true -> receive_response(Socket, list_to_binary([Response, Bin]), Length)
			end;
	   {error, timeout} ->
			Response;
	   {error, closed} ->
			Response
	end.
	
to_atom(Value) when is_atom(Value) ->
	Value;
to_atom(Value) when is_list(Value) ->
	list_to_atom(Value).
