%% @author Mochi Media <dev@mochimedia.com>
%% @copyright greeting Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the greeting application.

-module(greeting_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for greeting.
start(_Type, _StartArgs) ->
    greeting_deps:ensure(),
    greeting_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for greeting.
stop(_State) ->
    ok.
