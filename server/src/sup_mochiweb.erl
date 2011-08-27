%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mochisup.

-module(mochisup).
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
%% @doc Start the mochisup server.
start() ->
    mochisup_deps:ensure(),
    ensure_started(crypto),
    application:start(mochisup).


%% @spec stop() -> ok
%% @doc Stop the mochisup server.
stop() ->
    application:stop(mochisup).
