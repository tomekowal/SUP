-module(sup_db).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("sup_db.hrl").

%%------------------------------------------------------------------------------
%% @doc Initialize Mnesia database. It has to be called once before first use
%% of database.
%% @end
%%------------------------------------------------------------------------------
init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(device, [{type, set}, {attributes, record_info(fields, device)}, {disc_copies, [node()]}]),
  mnesia:create_table(release, [{type, set}, {attributes, record_info(fields, release)}, {disc_copies, [node()]}]).

%%------------------------------------------------------------------------------
%% @doc Starts Mnesia database
%% @end
%%------------------------------------------------------------------------------
start() ->
  mnesia:start().

%%------------------------------------------------------------------------------
%% @doc Stops Mnesia database
%% @end
%%------------------------------------------------------------------------------
stop() ->
  mnesia:stop().

%%------------------------------------------------------------------------------
%% @doc Puts record to database. If key already exists, record is overwritten.
%%
%% This behaviour can be changed in {@link init/0} {type, set}.
%% @end
%%------------------------------------------------------------------------------
create(Record) ->
  mnesia:transaction(
    fun() -> mnesia:write(Record) end
  ).

%%------------------------------------------------------------------------------
%% @doc Returns all records from specified TableName
%% @end
%%------------------------------------------------------------------------------
all(TableName) ->
  Q = qlc:q([X || X <- mnesia:table(TableName)]),
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%------------------------------------------------------------------------------
%% @doc Returns one record from specified TableName matching Key.
%%
%% This behaviour can be changed by options in {@link init/0}
%% @end
%%------------------------------------------------------------------------------
find(TableName, Key) ->
  F = fun() ->
    mnesia:read(TableName, Key, write)
  end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%------------------------------------------------------------------------------
%% @doc Deletes Mnesia record.
%% @end
%%------------------------------------------------------------------------------
destroy(TableName, Key) ->
  F = fun() ->
    mnesia:delete(TableName, Key, write)
  end,
  mnesia:transaction(F).

%%------------------------------------------------------------------------------
%% @doc Clears contents of Mnesia table.
%% @end
%%------------------------------------------------------------------------------
destroy_all(TableName) ->
  mnesia:clear_table(TableName).

append_job(Identity, Job) ->
  mnesia:transaction(
    fun() ->
      [Record] = sup_db:find(device, Identity),
      Jobs = Record#device.jobs,
      NewRecord = Record#device{jobs=Jobs++[Job#job{status=pending}]},
      sup_db:create(NewRecord)
    end
  ).

fetch_job(Identity, Index) ->
  {atomic, Val} = mnesia:transaction(
    fun() ->
      [Device] = sup_db:find(device, Identity),
      case Index =< length(Device#device.jobs) of
          true ->
              Job = lists:nth(Index, Device#device.jobs),
              {FirstList, [_Head | SecondList]} = lists:split(Index-1, Device#device.jobs),
              UpdatedJob = Job#job{status=pending},
              JobList = FirstList ++ [UpdatedJob] ++ SecondList,
              UpdatedDevice = Device#device{jobs=JobList},
              sup_db:create(UpdatedDevice),
              {ok, UpdatedJob};
          false ->
              empty
      end
    end
  ),
  Val.

replace_job(Identity, Index, Job) ->
  mnesia:transaction(
    fun() ->
      [Device] = sup_db:find(device, Identity),
      {FirstList, [_Head | SecondList]} = lists:split(Index-1, Device#device.jobs),
      UpdatedJob = Job#job{status=pending},
      JobList = FirstList ++ [UpdatedJob] ++ SecondList,
      UpdatedDevice = Device#device{jobs=JobList},
      sup_db:create(UpdatedDevice)
    end
  ).

delete_job(Identity, Index) ->
  mnesia:transaction(
    fun() ->
      [Device] = sup_db:find(device, Identity),
      {FirstList, SecondList} = lists:split(Index-1, Device#device.jobs),
      [_Head | TailList] = SecondList,
      JobList = FirstList ++ TailList,
      UpdatedDevice = Device#device{jobs=JobList},
      sup_db:create(UpdatedDevice)
    end
  ).

fail_job(Identity, Index, Exception) ->
  mnesia:transaction(
    fun() ->
      [Device] = sup_db:find(device, Identity),
      {FirstList, SecondList} = lists:split(Index-1, Device#device.jobs),
      [HeadJob | TailList] = SecondList,
      FailedJob = HeadJob#job{status={failed, Exception}},
      JobList = FirstList ++ [FailedJob] ++ TailList,
      UpdatedDevice = Device#device{jobs=JobList},
      sup_db:create(UpdatedDevice)
    end
  ).
