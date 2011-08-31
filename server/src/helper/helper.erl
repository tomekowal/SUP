-module(helper).
-compile(export_all).

timestamp() ->
  {{Year,Month,Day},{Hours,Minutes,Seconds}} = erlang:localtime(),
  erlang:binary_to_list(
    erlang:iolist_to_binary(
      io_lib:format("~B~2..0B~2..0B~2..0B~2..0B~2..0B", [Year, Month, Day, Hours, Minutes, Seconds])
    )
  ).