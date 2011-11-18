-module(main_controller).
-compile(export_all).

index(Req) ->
  {ok, HTMLOutput} = main_index_dtl:render([]),
  Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).
