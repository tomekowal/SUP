%% @author Mochi Media <dev@mochimedia.com>
%% @copyright sup_mochiweb Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the sup_mochiweb application.

-module(sup_mochiweb_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for sup_mochiweb.
start(_Type, _StartArgs) ->
    sup_mochiweb_deps:ensure(),
    sup_mochiweb_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for sup_mochiweb.
stop(_State) ->
    ok.
