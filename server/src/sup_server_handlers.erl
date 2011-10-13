-module(sup_server_handlers).
-include("db/sup_db.hrl").
-export([empty_handler/4, chain_handler/4, print_result_handler/4, upgrade_handler/4]).

empty_handler(_Message, _Result, _SessionData, _Extra) ->
    none.

chain_handler(_Message, _Result, _SessionData, 0) ->
    none;
chain_handler(Message, Result, _SessionData, Num) ->
    io:format("Handling ~p ~p ~p~n", [Message, Result, Num]),
    {next_job, #job{message = Message, module = ?MODULE, function = chain_handler, extra = Num-1, status = new}}.

print_result_handler(Message, Result, SessionData, Extra) ->
    io:format("Message: ~p~n", [Message]),
    io:format("Result: ~p~n", [Result]),
    io:format("Session data: ~p~n", [SessionData]),
    io:format("Extra data: ~p~n~n", [Extra]),
    none.

%% -----------------------------------------------------------------------------
%% @doc Handler performing release upgrade on device.
%% @end
%% -----------------------------------------------------------------------------
upgrade_handler({get_release, _Name}, {ok, Vsn}, _SessionData, _Extra) ->
    {next_job, #job{message = {update_to_release, Vsn}, module = ?MODULE, function = upgrade_handler, extra = ignore, status = new}};
upgrade_handler({get_release, Name}, Unexpected, _SessionData, _Extra) ->
    io:format("Failed to get release ~p on device: ~p~n", [Name, Unexpected]),
    none;
upgrade_handler({update_to_release, _Vsn}, {ok, Releases}, _SessionData, _Extra) ->
    io:format("Upgrade successful. Releases: ~p~n", [Releases]),
    none;
upgrade_handler({update_to_release, _Vsn}, Unexpected, _SessionData, _Extra) ->
    io:format("Failed to upgrade device: ~p~n", [Unexpected]),
    none.
