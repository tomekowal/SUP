-module(devices_controller).
-compile(export_all).

-record(device, {id, name, kernel, os}).

dispatch(Req, Args) ->
  case Req:get(method) of
    'GET' ->
      case Args of
        [] ->
          devices_controller:index(Req);
        ["new"] ->
          devices_controller:new(Req);
        [Id, "edit"] ->
          devices_controller:edit(Req, Id);
        [Id] ->
          devices_controller:show(Req, Id)
      end;
    'POST' ->
      case Args of
        [] ->
          devices_controller:create(Req);
        [Id, "update"] ->
          devices_controller:update(Req, Id);
        [Id, "destroy"] ->
          devices_controller:destroy(Req, Id)
      end
  end.

index(Req) ->
  Devices = lists:map(
    fun(Record) ->
      [Atom | Fields] = tuple_to_list(Record),
      RecordInfo = record_info(fields, device),
      lists:zip(RecordInfo, Fields) end,
    db:all(device)),
  {ok, HTMLOutput} = devices_index_dtl:render([{devices, Devices}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

new(Req) ->
  {ok, HTMLOutput} = devices_new_dtl:render([]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

create(Req) ->
  PostData = Req:parse_post(),
  Id = helper:timestamp(),
  Name = proplists:get_value("name", PostData),
  Kernel = proplists:get_value("kernel", PostData),
  Os = proplists:get_value("os", PostData),
  db:create({device, Id, Name, Kernel, Os}),
  Req:respond({302, [{"Location", "/devices"}], ""}).

edit(Req, Id) ->
  [Record] = db:find(device, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, device),
  Device = lists:zip(RecordInfo, Fields),
  {ok, HTMLOutput} = devices_edit_dtl:render([{device, Device}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

show(Req, Id) ->
  Req:respond({200, [{"Content-Type", "text/html"}], "show"}).

update(Req, Id) ->
  PostData = Req:parse_post(),
  Name = proplists:get_value("name", PostData),
  Kernel = proplists:get_value("kernel", PostData),
  Os = proplists:get_value("os", PostData),
  db:create({device, Id, Name, Kernel, Os}),
  Req:respond({302, [{"Location", "/devices"}], ""}).

destroy(Req, Id) ->
  db:destroy(device, Id),
  Req:respond({302, [{"Location", "/devices"}], ""}).


