%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc greeting.

-module(greeting).
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
%% @doc Start the greeting server.
start() ->
    greeting_deps:ensure(),
    ensure_started(crypto),
    application:start(greeting).


%% @spec stop() -> ok
%% @doc Stop the greeting server.
stop() ->
    application:stop(greeting).
