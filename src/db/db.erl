-module(db).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("db.hrl").

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(device, [{type, ordered_set}, {attributes, record_info(fields, device)}, {disc_copies, [node()]}]),
  mnesia:create_table(program, [{type, ordered_set}, {attributes, record_info(fields, program)}, {disc_copies, [node()]}]),
  mnesia:stop().

start() ->
  mnesia:start().

stop() ->
  mnesia:stop().

create(Record) ->
  mnesia:transaction(
    fun() -> mnesia:write(Record) end
  ).

all(TableName) ->
  Q = qlc:q([X || X <- mnesia:table(TableName)]),
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
  
find(TableName, Key) ->
  F = fun() ->
    mnesia:read(TableName, Key, write)
  end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
  
destroy(TableName, Key) ->
  F = fun() ->
    mnesia:delete(TableName, Key, write)
  end,
  mnesia:transaction(F).
  
destroy_all(TableName) ->
  mnesia:clear_table(TableName).

