%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for mochisup.

-module(mochisup_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

%%------------------------------------------------------------------------------
%% @doc Dispatches web requests.
%%
%% Sends requests to defined controllers. Currently "main",
%% "devices" and "programs".
%% @end
%%------------------------------------------------------------------------------
loop(Req, DocRoot) ->
    %% Path is everything after host name
    "/" ++ Path = Req:get(path),
    try
      [Controller | Args] = lists:map(fun(X) -> binary_to_list(X) end, re:split(Path, "/")),
      case Controller of
        "" ->
          main_controller:index(Req);
        "devices" ->
          devices_controller:dispatch(Req, Args);
        "programs" ->
          programs_controller:dispatch(Req, Args);
        _ ->
          Req:serve_file(Path, DocRoot)
      end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
