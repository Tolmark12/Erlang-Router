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
-export([start/0, start/1, stop/0, forward_request/1, update_app/1, add_app/1,
		add_target_to_app/1, remove_app/1]).
-export([start_relay/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {apps,listener,web_service}).
-record(target, {host,port}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	start([{web_service_port, 8080},{listener_port, 8000}]).
	
start([{web_service_port, WebSrvPort},{listener_port, ListenPort}]) ->
	Listen = listener:start(ListenPort),
	{ok, WebService} = web_service:start(WebSrvPort),
	gen_server:start_link({local, ?MODULE}, ?MODULE,[{listener,Listen},{web_service, WebService}],[]).
	
stop() ->
	gen_server:cast(?MODULE,stop),
	ok.

add_target_to_app({AppName, Target}) ->
	AppAtom = util:to_atom(AppName),
	gen_server:call(?MODULE, {add_target_to_app, {AppAtom, Target}}).

update_app({AppName, TargetList}) ->
	AppAtom = util:to_atom(AppName),
	gen_server:call(?MODULE, {update_app, {AppAtom, TargetList}}).

add_app({AppName, TargetList}) ->
	AppAtom = util:to_atom(AppName),
	gen_server:call(?MODULE, {add_app, {AppAtom, TargetList}}).

remove_app(AppName) ->
	AppAtom = util:to_atom(AppName),
	gen_server:call(?MODULE, {remove_app, AppAtom}).

forward_request({request, Client, Request, App}) ->
	AppAtom = util:to_atom(App),
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
init([{listener, Listen}, {web_service, WebService}]) ->
	{ok, #state{apps=[],
		listener=Listen, web_service=WebService}}.

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
handle_call({add_target_to_app, {AppName, Targets}}, _From, #state{apps=AppList}=State) ->
	{AppName, TargetList, CurTarget} = lists:keyfind(AppName, 1, AppList),
	NewTargets = lists:map(fun(T) -> 
					[TargetHost, TargetPort] = string:tokens(T, ":"),
					{Port, _} = string:to_integer(TargetPort),
					{target, TargetHost, Port}
		 		end, Targets),
	NewTargetList = lists:append(TargetList, NewTargets),
	NewAppList = lists:keystore(AppName, 1, AppList, {AppName, NewTargetList, CurTarget}),	
	{noreply, State#state{apps=NewAppList}};
handle_call({update_app, {AppName, Targets}}, _From, #state{apps=AppList}=State) ->
	NewTargetList = lists:map(fun(T) -> 
					[TargetHost, TargetPort] = string:tokens(T, ":"),
					{Port, _} = string:to_integer(TargetPort),
					{target, TargetHost, Port}
		 		end, Targets),
	NewAppList = lists:keystore(AppName, 1, AppList, {AppName, NewTargetList, 1}),
	{noreply, State#state{apps=NewAppList}};
handle_call({add_app, {AppName, Targets}}, _From, #state{apps=AppList}=State) ->
	TargetList = lists:map(fun(T) -> 
					[TargetHost, TargetPort] = string:tokens(T, ":"),
					{Port, _} = string:to_integer(TargetPort),
					{target, TargetHost, Port}
		 		end, Targets),
	NewAppList = lists:merge(AppList, [{AppName, TargetList, 1}]),
	{noreply, State#state{apps=NewAppList}};
handle_call({remove_app, AppName}, _From, #state{apps=AppList}=State) ->
	NewAppList = lists:keydelete(AppName, 1, AppList),
	{noreply, State#state{apps=NewAppList}};
handle_call({invoke, Client, Request, AppName}, _From, State) ->
	{Target, NewState} = select_target(AppName, State),
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
handle_cast(stop, #state{listener=Listener, web_service=WebSrv}=State) ->
	gen_tcp:close(Listener),
	WebSrv ! stop,
	{stop, normal, State};
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
	case gen_tcp:connect(Host, Port, [binary, {packet, http}, {active, false}, {packet_size, 4096}]) of
		{ok, Server} -> 
			gen_tcp:send(Server, Request),
			[{header,Header}, {header_record,HeaderRecord}] = http_handler:receive_header(Server),
			gen_tcp:send(Client, Header),
			Response = http_handler:receive_body(Server, HeaderRecord#headers.content_length),
			gen_tcp:send(Client, Response),
			gen_tcp:close(Server),
			gen_tcp:close(Client);
		{error, emfile} ->
			io:format("router: emfile error~n");
		{error,econnreset} ->
			io:format("router: Connection reset~n")
	end,
	self()!stop.