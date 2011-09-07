-module(server_main_controller, [Req]).
-compile(export_all).

index('GET', []) ->
  {ok, []}.

