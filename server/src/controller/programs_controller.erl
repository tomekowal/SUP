-module(programs_controller).
-compile(export_all).

-include("../db/sup_db.hrl").

dispatch(Req, Args) ->
  case Req:get(method) of
    'GET' ->
      case Args of
        [] ->
          programs_controller:index(Req);
        [""] ->
          programs_controller:index(Req);
        ["new"] ->
          programs_controller:new(Req);
        [Id, "edit"] ->
          programs_controller:edit(Req, Id);
        [Id] ->
          programs_controller:show(Req, Id)
      end;
    'POST' ->
      case Args of
        [] ->
          programs_controller:create(Req);
        [Id, "update"] ->
          programs_controller:update(Req, Id);
        [Id, "destroy"] ->
          programs_controller:destroy(Req, Id)
      end
  end.

index(Req) ->
  Programs = lists:map(
    fun(Record) ->
      [Atom | Fields] = tuple_to_list(Record),
      RecordInfo = record_info(fields, program),
      lists:zip(RecordInfo, Fields) end,
    sup_db:all(program)),
  {ok, HTMLOutput} = programs_index_dtl:render([{programs, Programs}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

new(Req) ->
  {ok, HTMLOutput} = programs_new_dtl:render([]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

create(Req) ->
  PostData = Req:parse_post(),
  Id = helper:timestamp(),
  Name = proplists:get_value("name", PostData),
  Version = proplists:get_value("version", PostData),
  Description = proplists:get_value("description", PostData),
  sup_db:create({program, Id, Name, Version, Description}),
  Req:respond({302, [{"Location", "/programs"}], ""}).

edit(Req, Id) ->
  [Record] = sup_db:find(program, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, program),
  Program = lists:zip(RecordInfo, Fields),
  {ok, HTMLOutput} = programs_edit_dtl:render([{program, Program}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

show(Req, Id) ->
  [Record] = sup_db:find(program, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, program),
  Program = lists:zip(RecordInfo, Fields),
  {ok, HTMLOutput} = programs_show_dtl:render([{program, Program}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

update(Req, Id) ->
  PostData = Req:parse_post(),
  Id = proplists:get_value("id", PostData),
  Name = proplists:get_value("name", PostData),
  Version = proplists:get_value("version", PostData),
  Description = proplists:get_value("description", PostData),
  sup_db:create({program, Id, Name, Version, Description}),
  Req:respond({302, [{"Location", "/programs"}], ""}).

destroy(Req, Id) ->
  sup_db:destroy(program, Id),
  Req:respond({302, [{"Location", "/programs"}], ""}).

