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
        [Id] ->
          devices_controller:show(Req, Id)
      end;
    'POST' ->
      case Args of
        [] ->
          devices_controller:create(Req);
        [Id, "destroy"] ->
          devices_controller:destroy(Req, Id);
        [Id, "jobs", "new"] ->
					devices_controller:append_job(Req, Id)
      end
  end.

index(Req) ->
  Devices = lists:map(
    fun(Record) ->
	device_to_print(Record) end,
    sup_db:all(device)),
  {ok, HTMLOutput} = devices_index_dtl:render([{devices, Devices}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

new(Req) ->
  {ok, HTMLOutput} = devices_new_dtl:render([]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

create(Req) ->
  PostData = Req:parse_post(),
  Identity = proplists:get_value("identity", PostData),
  sup_db:create(#device{identity=Identity, last_contact="never", jobs=[], releases=[], ip="unknown"}),
  Req:respond({302, [{"Location", "/devices/" ++ Identity}], ""}).

show(Req, Id) ->
  [Record] = sup_db:find(device, Id),
  Device = device_to_print(Record),
  {ok, HTMLOutput} = devices_show_dtl:render([{device, Device}]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

destroy(Req, Id) ->
  sup_db:destroy(device, Id),
  Req:respond({302, [{"Location", "/devices"}], ""}).

append_job(Req, Id) ->
	PostData = Req:parse_post(),
  Message = proplists:get_value("message", PostData),
  Module = proplists:get_value("module", PostData),
  Function = proplists:get_value("function", PostData),
  Extra = proplists:get_value("extra", PostData),
	Job = #job{message=Message, module=Module, function=Function, extra=Extra},
	sup_db:append_job(Id,Job),
  Req:respond({302, [{"Location", "/devices/" ++ Id}], ""}).

device_to_print(Record) ->
  Jobs = lists:map(
      fun(Job) ->
	      [_Atom | RawFields] = tuple_to_list(Job),
        RecordInfo = record_info(fields, job),
        Fields=lists:map(fun(Field) ->io_lib:format("~p",[Field]) end, RawFields),
        lists:zip(RecordInfo, Fields)
      end,
      Record#device.jobs),
  NewRecord = Record#device{jobs=Jobs}, 
  [_Atom | Fields] = tuple_to_list(NewRecord),
  RecordInfo = record_info(fields, device),
  lists:zip(RecordInfo, Fields).

