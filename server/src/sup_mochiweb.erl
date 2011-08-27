%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc sup_mochiweb.

-module(sup_mochiweb).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the sup_mochiweb server.
start() ->
    sup_mochiweb_deps:ensure(),
    ensure_started(crypto),
    application:start(sup_mochiweb).


%% @spec stop() -> ok
%% @doc Stop the sup_mochiweb server.
stop() ->
    application:stop(sup_mochiweb).
