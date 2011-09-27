-module(sampleapp_server).
-export([start_link/0, get_version/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behavior(gen_server).

%% normal interface

start_link() ->
    gen_server:start_link({local, sampleapp_server}, ?MODULE, ignore, []).

get_version() ->
    gen_server:call(sampleapp_server, get_version).

%% gen_server callbacks

init(_Args) ->
    {ok, ignore}.

handle_call(get_version, _From, State) ->
    {reply, "2.0", State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    io:format("Sampleapp updated from ~p~n", [OldVsn]),
    {ok, State}.
