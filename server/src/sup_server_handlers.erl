-module(sup_server_handlers).
-export([chain_handler/4, print_result_handler/4]).

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

