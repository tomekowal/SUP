-module(delete_controller).
-compile(export_all).
  
dispatch(Req, Args) ->
  case Req:get(method) of
    'GET' ->
      case Args of
        [] ->
          delete_controller:index(Req);
        [""] ->
          delete_controller:index(Req)
      end;
    'POST' ->
      case Args of
        [] ->
          delete_controller:delete(Req)
      end
  end.

index(Req) ->
    {ok, HTMLOutput} = upload_index_dtl:render([]),
    Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

delete(Req) ->
    FormData = mochiweb_multipart:parse_form(Req),
    RelPath = proplists:get_value("relpath", FormData),
    FileName = proplists:get_value("filename", FormData),
    PathToBinaryRepo = sup_mochiweb_deps:local_path(["priv", "repository", "binary"]),
    FinalFilename = filename:join([PathToBinaryRepo, RelPath, FileName]),
    io:format("File is ~s~n", [FinalFilename]),
    ok = file:delete(FinalFilename),
    generate_packages_gz(),
    Req:respond({302, [{"Location", "/repository/" ++ RelPath }], ""}).

generate_packages_gz() ->
    PathToRepository = sup_mochiweb_deps:local_path(["priv", "repository"]),
    Command = "cd " ++ PathToRepository ++ " && " ++
        "dpkg-scanpackages binary | gzip -9c > binary/Packages.gz && " ++
        "dpkg-scansources source | gzip -9c > source/Pacakges.gz",
    os:cmd(Command).
