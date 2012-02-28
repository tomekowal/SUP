-module(repository_controller).
-compile(export_all).

dispatch(Req) ->
    PathToRepository = sup_mochiweb_deps:local_path(["priv", "repository", "binary"]),
    "/repository/" ++ RelPath = Req:get(path),
    FullPath = filename:join([PathToRepository, RelPath]),
    case filelib:is_dir(FullPath) of
        true ->
            listdir(Req, FullPath, RelPath);
        _ ->
            Req:serve_file(RelPath, PathToRepository)
    end.

listdir(Req, FullPath, RelPath) ->
    {ok, FileList} = file:list_dir(FullPath),
    Files = lists:sort(lists:foldl(
    	fun(FileName, Files) ->
            Len = string:len(FileName),
            FullPathToFile = filename:join([FullPath, FileName]),
            ModificationTime = helper:format_date(filelib:last_modified(FullPathToFile)),
            FileSize = filelib:file_size(FullPathToFile),
            case string:sub_string(FileName, Len-3, Len) of
                ".deb" ->
                    File = case string:tokens(string:sub_string(FileName, 1, Len-4), "_") of
                        [Package, Version, Architecture] ->
                            {FileName, Package, Version, Architecture, ModificationTime, FileSize};
                        _ ->
                            {FileName, FileName, "", "", ModificationTime, FileSize}
                    end,
                    [File | Files];
                _ ->
                    Files
            end
        end,
        [],
        FileList
    )),
    {ok, HTMLOutput} = repository_listdir_dtl:render([{relpath, RelPath}, {files, Files}]),
    Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).
