%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mochisup Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mochisup application.

-module(mochisup_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mochisup.
start(_Type, _StartArgs) ->
    mochisup_deps:ensure(),
    mochisup_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mochisup.
stop(_State) ->
    ok.
