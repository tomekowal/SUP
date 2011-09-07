-module(server_devices_controller, [Req]).
-compile(export_all).

-include("../lib/sup_db.hrl").

index('GET', []) ->
  Devices = lists:map(
    fun(Record) ->
      [Atom | Fields] = tuple_to_list(Record),
      RecordInfo = record_info(fields, device),
      lists:zip(RecordInfo, Fields) end,
    sup_db:all(device)),
  {ok, [{devices, Devices}]}.

create('POST', []) ->
  Id = helper:timestamp(),
  Name = Req:post_param("name"),
  Kernel = Req:post_param("kernel"),
  Os = Req:post_param("os"),
  Ip = Req:post_param("ip"),
  Port = Req:post_param("port"),
  Message = Req:post_param("message"),
  sup_db:create({device, Id, Name, Kernel, Os, Ip, Port, Message, []}),
  {redirect, "/devices"}.

edit('GET', [Id]) ->
  [Record] = sup_db:find(device, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, device),
  Device = lists:zip(RecordInfo, Fields),
  {ok, [{device, Device}]}.

show('GET', [Id]) ->
  [Record] = sup_db:find(device, Id),
  [Atom | Fields] = tuple_to_list(Record),
  RecordInfo = record_info(fields, device),
  Device = lists:zip(RecordInfo, Fields),
  {ok, [{device, Device}]}.

update('POST', [Id]) ->
  Name = Req:post_param("name"),
  Kernel = Req:post_param("kernel"),
  Os = Req:post_param("os"),
  Ip = Req:post_param("ip"),
  Port = Req:post_param("port"),
  Message = Req:post_param("message"),
  sup_db:create({device, Id, Name, Kernel, Os, Ip, Port, Message, []}),
  {redirect, "/devices"}.

destroy('POST', [Id]) ->
  sup_db:destroy(device, Id),
  {redirect, "/devices"}.

ping(Req, Id) ->
  [Record] = sup_db:find(device, Id),
  {ok, Ip}=inet_parse:address(Record#device.ip),
    Request = get_data,
  Body = [server, conects, and_send, Record#device.message],
    case gen_tcp:connect(Ip, list_to_integer(Record#device.port),[binary, {packet, 2}]) of
        {ok, Sock} ->
            ok = gen_tcp:send(Sock, mochijson2:encode({struct, [{Request, Body}]})),
            ok = gen_tcp:close(Sock);
        {error, Reason} ->
            io:format("~w~n", [Reason])	
    end,
  {redirect, "/devices"}.
