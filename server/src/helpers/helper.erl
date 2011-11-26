-module(helper).
-compile(export_all).

format_date(Date) ->
  case Date of
    {{Year,Month,Day},{Hours,Minutes,Seconds}} ->
        erlang:binary_to_list(
          erlang:iolist_to_binary(
            io_lib:format("~2..0B-~2..0B-~4..0B ~2..0B:~2..0B:~2..0B", [Day, Month, Year, Hours, Minutes, Seconds])
          )
        );
    S -> S
  end.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_date_test() ->
    [
        ?assertEqual(
            format_date({{2000, 1, 1}, {0, 0, 0}}),
            "01-01-2000 00:00:00"
        ),
        ?assertEqual(
            format_date({{2011, 11, 26}, {20, 24, 31}}),
           "26-11-2011 20:24:31"
        ),
        ?assertEqual(
            format_date("26-11-2011 20:24:31"),
            "26-11-2011 20:24:31"
        )
    ].

-endif.
