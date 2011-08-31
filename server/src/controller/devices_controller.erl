-module(devices_controller).
-compile(export_all).

-include("../db/sup_db.hrl").

dispatch(Req, Args) ->
  case Req:get(method) of
    'GET' ->
      case Args of
        [] ->
          devices_controller:index(Req);
        [""] ->
          devices_controller:index(Req);
        ["new"] ->
          devices_controller:new(Req);
        [Id, "edit"] ->
          devices_controller:edit(Req, Id);
        [Id, "programs"] ->
          devices_controller:programs(Req, Id);
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
    sup_db:all(device)),
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
  Ip = proplists:get_value("ip", PostData),
  Port = proplists:get_value("port", PostData),
  Message = proplists:get_value("message", PostData),
  sup_db:create({device, Id, Name, Kernel, Os, Ip, Port, Message, []}),
  Req:respond({302, [{"Location", "/devices"}], ""}).

edit(Req, Id) ->
  [Record] = sup_db:find(device, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, device),
  Device = lists:zip(RecordInfo, Fields),
  {ok, HTMLOutput} = devices_edit_dtl:render([{device, Device}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

show(Req, Id) ->
  [Record] = sup_db:find(device, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, device),
  Device = lists:zip(RecordInfo, Fields),
  {ok, HTMLOutput} = devices_show_dtl:render([{device, Device}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

update(Req, Id) ->
  PostData = Req:parse_post(),
  Id = proplists:get_value("id", PostData),
  Name = proplists:get_value("name", PostData),
  Kernel = proplists:get_value("kernel", PostData),
  Os = proplists:get_value("os", PostData),
  Ip = proplists:get_value("ip", PostData),
  Port = proplists:get_value("port", PostData),
  Message = proplists:get_value("message", PostData),
  sup_db:create({device, Id, Name, Kernel, Os, Ip, Port, Message, []}),
  Req:respond({302, [{"Location", "/devices"}], ""}).

destroy(Req, Id) ->
  sup_db:destroy(device, Id),
  Req:respond({302, [{"Location", "/devices"}], ""}).

programs(Req, Id) ->
  HTMLOutput = "programs",
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).


