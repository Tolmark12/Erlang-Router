%% Author: ryan
%% Created: Sep 14, 2010
%% Description: TODO: Add description to listener
-module(web_service).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/0, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listener}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	start(8080).
	
start(Port) ->
	{ok, Listen} = gen_tcp:listen(Port, [binary, {packet, http},
										 {reuseaddr, true},
										 {active, false},
										 {packet_size, 1024}]),
	spawn(fun () -> connect(Listen) end),
	gen_server:start_link({local, ?MODULE}, ?MODULE,[{listener,Listen}],[]).
	
stop() ->
	gen_server:cast(?MODULE,stop),
	ok.

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
init([{listener, Listen}]) ->
	{ok, #state{listener=Listen}}.

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
handle_call(_Msg, _From, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({add_target_to_app, [AppName | Params]}, State) ->
	spawn(router,add_target_to_app, [{AppName, Params}]),
	{noreply, State};
handle_cast({update_app, [AppName | Params]}, State) ->
	spawn(router,update_app, [{AppName, Params}]),
	{noreply, State};
handle_cast({add_app, [AppName | Params]}, State) ->
	spawn(router, add_app, [{AppName, Params}]),
	{noreply, State};
handle_cast(stop, #state{listener=Listener}=State) ->
	gen_tcp:close(Listener),
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
	[{header,_Header}, {header_record,HeaderRecord}] = http_handler:receive_header(Socket),
	_Body = case HeaderRecord#headers.request_type of
		'POST' -> http_handler:receive_body(Socket, HeaderRecord#headers.content_length);
		'PUT'  -> http_handler:receive_body(Socket, HeaderRecord#headers.content_length);
		_      -> <<>>
	end,
	[Call | Params] = string:tokens(HeaderRecord#headers.request_path, "/"),
	gen_server:cast(?MODULE, {util:to_atom(Call), Params}),
	gen_tcp:send(Socket, ["HTTP/1.1 200 OK\r\n\r\n{\"success\":true}"]),
	gen_tcp:close(Socket),
	self()!stop.

reset(Listen) ->
	gen_tcp:close(Listen),
	start(),
	self()!stop.
