-module(sup_server_handlers).
-export([empty_handler/4, chain_handler/4, print_result_handler/4, upgrade_handler/4]).

empty_handler(_Job, _Message, _SessionData, _Extra) ->
    [].

chain_handler(_Job, _Message, _SessionData, 0) ->
    [];
chain_handler(Job, Message, _SessionData, Num) ->
    io:format("Handling ~p ~p ~p~n", [Job, Message, Num]),
    [{Job, ?MODULE, chain_handler, Num-1}].

print_result_handler(Job, Message, SessionData, Extra) ->
    io:format("Job: ~p~n", [Job]),
    io:format("Result: ~p~n", [Message]),
    io:format("Session data: ~p~n", [SessionData]),
    io:format("Extra data: ~p~n~n", [Extra]),
    [].

%% -----------------------------------------------------------------------------
%% @doc Handler performing release upgrade on device.
%% @end
%% -----------------------------------------------------------------------------
upgrade_handler({get_release, _Name}, {ok, Vsn}, _SessionData, _Extra) ->
    [{{update_to_release, Vsn}, ?MODULE, upgrade_handler, ignore}];
upgrade_handler({get_release, Name}, Unexpected, _SessionData, _Extra) ->
    io:format("Failed to get release ~p on device: ~p~n", [Name, Unexpected]),
    [];
upgrade_handler({update_to_release, _Vsn}, {ok, Releases}, _SessionData, _Extra) ->
    io:format("Upgrade successful. Releases: ~p~n", [Releases]),
    [];
upgrade_handler({update_to_release, _Vsn}, Unexpected, _SessionData, _Extra) ->
    io:format("Failed to upgrade device: ~p~n", [Unexpected]),
    [].
