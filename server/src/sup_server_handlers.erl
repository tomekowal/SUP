-module(sup_server_handlers).
-include("db/sup_db.hrl").
-export([empty_handler/4, chain_handler/4, print_result_handler/4, upgrade_handler/4]).

%% About the handlers:
%%
%% Handler is a function that is invoked after the device sends back a result of a job
%% that was issued on it. Handler arguments are:
%% * Message - message that was sent to device
%% * Result - job result sent back by the device
%% * SessionData - data associated with the session
%% * Extra - some extra data from the job definition
%%
%% Handler returns the tuple {NextJob, Continue}, where
%% NextJobSpec = {next_job, NextJob} | none - potential next job for the device
%%    (as a continuation of this job)
%% Continue = continue | finish - whether to continue the session or finish it immediately
%%

empty_handler(_Message, _Result, _SessionData, _Extra) ->
    {none, continue}.

chain_handler(_Message, _Result, _SessionData, {0, _Continue}) ->
    {none, continue};
chain_handler(Message, Result, _SessionData, {Num, Continue}) ->
    io:format("Handling ~p ~p ~p~n", [Message, Result, Num]),
    NextJob = #job{
      message = Message,
      module = ?MODULE,
      function = chain_handler,
      extra = {Num-1, Continue},
      status = new
     },
    {{next_job, NextJob}, Continue}.

print_result_handler(Message, Result, SessionData, Extra) ->
    io:format("Message: ~p~n", [Message]),
    io:format("Result: ~p~n", [Result]),
    io:format("Session data: ~p~n", [SessionData]),
    io:format("Extra data: ~p~n~n", [Extra]),
    {none, continue}.

%% -----------------------------------------------------------------------------
%% @doc Handler performing release upgrade on device.
%% @end
%% -----------------------------------------------------------------------------
upgrade_handler({get_release, _Name}, {ok, Vsn}, _SessionData, _Extra) ->
    NextJob =  #job{
      message = {update_to_release, Vsn},
      module = ?MODULE,
      function = upgrade_handler,
      extra = ignore,
      status = new},
    {{next_job, NextJob}, continue};
upgrade_handler({get_release, Name}, Unexpected, _SessionData, _Extra) ->
    io:format("Failed to get release ~p on device: ~p~n", [Name, Unexpected]),
    {none, continue};
upgrade_handler({update_to_release, _Vsn}, {ok, Releases}, _SessionData, _Extra) ->
    io:format("Upgrade successful. Releases: ~p~n", [Releases]),
    {none, continue};
upgrade_handler({update_to_release, _Vsn}, Unexpected, _SessionData, _Extra) ->
    io:format("Failed to upgrade device: ~p~n", [Unexpected]),
    {none, continue}.
