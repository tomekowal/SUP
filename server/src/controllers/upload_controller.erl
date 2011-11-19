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
    PathToPriv = sup_mochiweb_deps:local_path(["priv"]),
    TmpFilename = filename:join([PathToPriv, "tmp", Filename]),
    FinalFilename = filename:join([PathToPriv, RelPath, Filename]),
    {ok, _} = file:copy(TmpFilename, FinalFilename),
    ok = file:delete(TmpFilename),
    generate_packages_gz(),
    Req:respond({302, [{"Location", "/" ++ RelPath }], ""}).

generate_packages_gz() ->
    PathToRepository = sup_mochiweb_deps:local_path(["priv", "repository"]),
    os:cmd("dpkg-scanpackages "
            ++ PathToRepository
            ++ "/binary | gzip -9c > "
            ++ PathToRepository
            ++ "/binary/Packages.gz"),
    os:cmd("dpkg-scansources "
            ++ PathToRepository
            ++ "/source | gzip -9c > "
            ++ PathToRepository
            ++ "/source/Packages.gz").


