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
    TempFilename = "/tmp/" ++ helper:timestamp() ++ "_" ++ integer_to_list(erlang:phash2(make_ref())),
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
    Files = mochiweb_multipart:parse_form(Req, FileHandler),
    Photo = proplists:get_value("file", Files),
    Req:respond({302, [{"Location", "/upload" }], ""}).
