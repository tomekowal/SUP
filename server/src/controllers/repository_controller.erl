-module(repository_controller).
-compile(export_all).

dispatch(Req) ->
    PathToPriv = sup_mochiweb_deps:local_path(["priv"]),
    "/" ++ RelPath = Req:get(path),
    FullPath = filename:join([PathToPriv, RelPath]),
    case filelib:is_dir(FullPath) of
        true ->
            listdir(Req, FullPath, RelPath);
        _ ->
            Req:serve_file(RelPath, PathToPriv)
    end.

listdir(Req, FullPath, RelPath) ->
    {ok, FileList} = file:list_dir(FullPath),
    Files = lists:sort(lists:map(fun(FileName) ->
        FullPathToFile = filename:join([FullPath, FileName]),
        ModificationTime = helper:format_date(filelib:last_modified(FullPathToFile)),
        FileSize = filelib:file_size(FullPathToFile),
        {FileName, ModificationTime, FileSize} end,
        FileList
    )),
    {ok, HTMLOutput} = repository_listdir_dtl:render([{relpath, RelPath}, {files, Files}]),
    Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).
