%%% -------------------------------------------------------------------
%%% Author  : gosia
%%% Description :
%%%
%%% Created : 2011-07-28
%%% -------------------------------------------------------------------
-module(list_db).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0, start_link/0, stop/0, read/1, delete/1, write/2, all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

write(Key, Value) ->
	gen_server:call(?MODULE, {write, Key, Value}).

delete(Key) ->
	gen_server:call(?MODULE, {delete, Key}).

read(Key) ->
	gen_server:call(?MODULE, {read, Key}).

all() ->
	gen_server:call(?MODULE, {all}).

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
	register(?MODULE, self()),
	{ok,[]}.

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

handle_call({write, Key, Value}, _From, State) ->
    {reply, ok, [{Key, Value} | State]};
handle_call({delete, Key}, _From, State) ->
    {reply, ok, process_delete(Key, State)};
handle_call({read, Key}, _From, State) ->
    {reply, process_read(Key, State), State};
handle_call({all}, _From, State) ->
    {reply, State, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State)->
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

process_delete(Key, [{Key, _Value} | Base]) ->
	process_delete(Key, Base);
process_delete(Key, [_Tuple | Base]) ->
	[_Tuple | process_delete(Key, Base)];
process_delete(_Key, []) ->
	[].

process_read(Key, [{Key, Value} | _Base]) ->
	{ok, Value};
process_read(Key, [_Tuple | Base]) ->
	process_read(Key, Base);
process_read(_Key, []) ->
	{error, "No such key."}.
