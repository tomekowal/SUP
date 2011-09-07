-module(server_programs_controller, [Req]).
-compile(export_all).

-include("../lib/sup_db.hrl").

index('GET', []) ->
%  Programs = lists:map(
%    fun(Record) ->
%      [Atom | Fields] = tuple_to_list(Record),
%      RecordInfo = record_info(fields, program),
%      lists:zip(RecordInfo, Fields) end,
%    sup_db:all(program)),
  Programs = boss_db:find(program, []),
  {ok, [{programs, Programs}]}.

create('POST', []) ->
  Id = helper:timestamp(),
  Name = Req:post_param("name"),
  Version = Req:post_param("version"),
  Description = Req:post_param("description"),
%  sup_db:create({program, Id, Name, Version, Description}),
  Program = program:new(Id, Name, Version, Description),
  Program:save(),
  {redirect, "/programs"}.

edit('GET', [Id]) ->
  [Record] = sup_db:find(program, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, program),
  Program = lists:zip(RecordInfo, Fields),
  {ok, [{program, Program}]}.

show('GET', [Id]) ->
  [Record] = sup_db:find(program, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, program),
  Program = lists:zip(RecordInfo, Fields),
  {ok, [{program, Program}]}.

update('POST', [Id]) ->
  Name = Req:post_param("name"),
  Version = Req:post_param("version"),
  Description = Req:post_param("description"),
  sup_db:create({program, Id, Name, Version, Description}),
  {redirect, "/programs"}.

destroy('POST', [Id]) ->
  sup_db:destroy(program, Id),
  {redirect, "/programs"}.
