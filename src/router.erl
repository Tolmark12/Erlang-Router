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

%% --------------------------------------------------------------------
%% External exports
-export([start/0, stop/0, forward_request/1]).
-export([start_relay/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {target}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]).

stop() ->
	gen_server:call(?MODULE,stop).

forward_request({request, Client, Request}) ->
	gen_server:cast(?MODULE, {invoke, Client, Request}).

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
	Target = {{host,"localhost"},{port, 3000}},
	{ok, #state{target=Target}}.

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
handle_call({invoke, Client, Request}, _From, State) ->
	spawn(?MODULE, start_relay, [Client, Request, State]),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({invoke, Client, Request}, State) ->
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
start_relay(Client, Request, #state{target=Target}=_State) ->
	{{host,Host},{port, Port}} = Target,
	case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]) of
		{ok, Server} -> 
			gen_tcp:send(Server, Request),
			forward_reply(Server,Client),
			gen_tcp:close(Server);
		{error, emfile} ->
			io:format("router: emfile error~n");
		{error,econnreset} ->
			io:format("router: Connection reset~n")
	end,
	self()!stop.

forward_reply(Server,Client) ->
	receive
		{tcp, Server, Bin } ->
		    gen_tcp:send(Client, Bin),
			forward_reply(Server, Client);
		{tcp_closed, Server} ->
			ok
		after 1000 ->
			ok
	end.
