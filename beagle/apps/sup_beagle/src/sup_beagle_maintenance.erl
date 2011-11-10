-module(sup_beagle_maintenance).
-export([create_RELEASES/1, upgrade_release/1, clean_old_releases/1]).

%% this module is used by debian package maintainer scripts through escript and rpc

create_RELEASES([TargetRoot, Release, Version]) ->
    application:start(sasl),
    ok = release_handler:create_RELEASES
      (TargetRoot,
       filename:join([TargetRoot, "releases"]),
       filename:join([TargetRoot, "releases", Version, Release++".rel"]),
       []
      ),
    ok.

upgrade_release([TargetRoot, Release, Version]) ->
    application:start(sasl),
    RelFile = filename:join([TargetRoot, "releases", Version, Release++".rel"]),
    {ok, Version} = release_handler:set_unpacked(RelFile, []),
    {ok, _OtherVsn, _Descr} = release_handler:install_release(Version),
    ok = release_handler:make_permanent(Version),
    ok.

clean_old_releases([]) ->
    application:start(sasl),
    ok = lists:foldl(
           fun({_, RelVsn, _, _}, ResultSoFar) ->
                   case {ResultSoFar, release_handler:remove_release(RelVsn)} of
                       {ok, ok} ->
                           ok;
                       _ ->
                           error
                   end
           end,
           ok,
           lists:filter(
             fun({_Name, _RelVsn, _Apps, State}) ->
                     case State of
                         old ->
                             true;
                         _ ->
                             false
                     end
             end,
             release_handler:which_releases()
            )
          ),
    ok.
