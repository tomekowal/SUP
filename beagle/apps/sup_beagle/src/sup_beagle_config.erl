-module(sup_beagle_config).
-export([start_link/0, get/1, set/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behavior(gen_server).

start_link() ->
    gen_server:start_link({local, sup_beagle_config}, ?MODULE, ignore, []).

get(Key) ->
    gen_server:call(sup_beagle_config, {get, Key}).

set(Key, Value) ->
    gen_server:call(sup_beagle_config, {set, Key, Value}).

%% gen_server callback interface

init(_Args) ->
    {ok, appenv_to_dict(application:get_all_env(sup_beagle), dict:new())}.

appenv_to_dict([{Key,Value}|AppEnvTail], Dict) ->
    appenv_to_dict(AppEnvTail, dict:store(Key, Value, Dict));
appenv_to_dict([], Dict) ->
    Dict.

find(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            {ok, Value};
        error ->
            none
    end.

handle_call({get, Key}, _From, Dict) ->
    {reply, find(Key, Dict), Dict};
handle_call({set, Key, Value}, _From, Dict) ->
    {reply, find(Key, Dict), dict:store(Key, Value, Dict)}.

handle_cast(_Request, Dict) ->
    {noreply, Dict}.

handle_info(_Request, Dict) ->
    {noreply, Dict}.

terminate(_Reason, _Dict) ->
    ok.

code_change(_OldVsn, Dict, _Extra) ->
    {ok, Dict}.
