-module(upload_controller).
-compile(export_all).

chunk_handler(Filename, ContentType, TempFilename, File) ->
    fun(Next) ->
        case Next of
            eof ->
                file:close(File),
                {Filename, ContentType, TempFilename};
            Data ->
                file:write(File, Data),
                chunk_handler(Filename, ContentType, TempFilename, File)
        end
    end.

handle_file(Filename, ContentType) ->
    PathToTmp = sup_mochiweb_deps:local_path(["priv", "tmp"]),
    TempFilename = filename:join([PathToTmp, Filename]),
    {ok, File} = file:open(TempFilename, [raw, write]),
    chunk_handler(Filename, ContentType, TempFilename, File).

dispatch(Req, Args) ->
  case Req:get(method) of
    'GET' ->
      case Args of
        [] ->
          upload_controller:index(Req);
        [""] ->
          upload_controller:index(Req)
      end;
    'POST' ->
      case Args of
        [] ->
          upload_controller:create(Req)
      end
  end.

index(Req) ->
    {ok, HTMLOutput} = upload_index_dtl:render([]),
    Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

create(Req) ->
    FileHandler = fun(Filename, ContentType) -> handle_file(Filename, ContentType) end,
    FormData = mochiweb_multipart:parse_form(Req, FileHandler),
    {Filename, _ContentType, _TempFilename} = proplists:get_value("file", FormData),
    RelPath = proplists:get_value("relpath", FormData),
    PathToBinaryRepo = sup_mochiweb_deps:local_path(["priv", "repository", "binary"]),
    PathToTmp = sup_mochiweb_deps:local_path(["priv", "tmp"]),
    TmpFilename = filename:join([PathToTmp, Filename]),
    FinalFilename = filename:join([PathToBinaryRepo, RelPath, Filename]),
    ok = file:rename(TmpFilename, FinalFilename),
    generate_packages_gz(),
    Req:respond({302, [{"Location", "/repository/" ++ RelPath }], ""}).

generate_packages_gz() ->
    PathToRepository = sup_mochiweb_deps:local_path(["priv", "repository"]),
    Command = "cd " ++ PathToRepository ++ " && " ++
        "dpkg-scanpackages binary | gzip -9c > binary/Packages.gz && " ++
        "dpkg-scansources source | gzip -9c > source/Pacakges.gz",
    os:cmd(Command).
