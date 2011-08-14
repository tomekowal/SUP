-module(programs_controller).
-compile(export_all).

-record(program, {id, name, version}).

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
    db:all(program)),
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
  db:create({program, Id, Name, Version}),
  Req:respond({302, [{"Location", "/programs"}], ""}).

edit(Req, Id) ->
  [Record] = db:find(program, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, program),
  Program = lists:zip(RecordInfo, Fields),
  {ok, HTMLOutput} = programs_edit_dtl:render([{program, Program}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

show(Req, Id) ->
  Req:respond({200, [{"Content-Type", "text/html"}], "show"}).

update(Req, Id) ->
  PostData = Req:parse_post(),
  Name = proplists:get_value("name", PostData),
  Version = proplists:get_value("version", PostData),
  db:create({program, Id, Name, Version}),
  Req:respond({302, [{"Location", "/programs"}], ""}).

destroy(Req, Id) ->
  db:destroy(program, Id),
  Req:respond({302, [{"Location", "/programs"}], ""}).


