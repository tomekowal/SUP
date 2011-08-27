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
  mnesia:create_table(program, [{type, set}, {attributes, record_info(fields, program)}, {disc_copies, [node()]}]),
  mnesia:stop().

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

