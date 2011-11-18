-module(sup_beagle_maintenance).
-export([create_RELEASES/1, manual_upgrade/1, upgrade_release/1]).

%% this module is used by debian package maintainer scripts through nodetool rpc or directly as erl -s param

create_RELEASES([TargetRoot, Release, Version, halt]) ->
    application:load(sasl),
    create_RELEASES(lists:map(fun atom_to_list/1, [TargetRoot, Release, Version])),
    halt();
create_RELEASES([TargetRoot, Release, Version]) ->
    ok = release_handler:create_RELEASES
      (TargetRoot,
       filename:join([TargetRoot, "releases"]),
       filename:join([TargetRoot, "releases", Version, Release++".rel"]),
       []
      ),
    ok.

manual_upgrade([TargetRoot, Release, Version, OldVersion, halt]) ->
    application:load(sasl),
    manual_upgrade(lists:map(fun atom_to_list/1, [TargetRoot, Release, Version, OldVersion])),
    halt();
manual_upgrade([TargetRoot, Release, Version, OldVersion]) ->
    NewApps = applications_from_rel(filename:join([TargetRoot, "releases", Version, Release++".rel"])),
    OldApps = applications_from_rel(filename:join([TargetRoot, "releases", OldVersion, Release++".rel"])),
    create_RELEASES([TargetRoot, Release, Version]),
    lists:foreach(fun(App) ->
                          AppDir = filename:join([TargetRoot, "lib", App]),
                          ok = remove_recursively([AppDir])
                  end,
                  OldApps--NewApps
                 ),
    RelDir = filename:join([TargetRoot, "releases", OldVersion]),
    ok = remove_recursively([RelDir]),
    ok.

applications_from_rel(RelFile) ->
    {ok, [{release, {_Release, _Version}, {erts, _ErtsVsn}, NewAppTuples}]} =
        file:consult(RelFile),
    lists:map(fun({App,Vsn}) -> atom_to_list(App)++"-"++Vsn end, NewAppTuples).

remove_recursively([Path | Rest]) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, DirContents} = file:list_dir(Path),
            {ok, Cwd} = file:get_cwd(),
            ok = file:set_cwd(Path),
            ok = remove_recursively(DirContents),
            ok = file:set_cwd(Cwd),
            ok = file:del_dir(Path);
        _ ->
            ok = file:delete(Path)
    end,
    remove_recursively(Rest);
remove_recursively([]) ->
    ok.

upgrade_release([TargetRoot, Release, Version]) ->
    RelFile = filename:join([TargetRoot, "releases", Version, Release++".rel"]),
    {ok, Version} = release_handler:set_unpacked(RelFile, []),
    {ok, OldVsn, _Descr} = release_handler:install_release(Version),
    ok = release_handler:make_permanent(Version),
    ok = release_handler:remove_release(OldVsn),
    ok.
