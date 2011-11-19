-module(categories_controller).
-compile(export_all).

-include("../db/sup_db.hrl").

dispatch(Req, Args) ->
    case Req:get(method) of
        'GET' ->
            case Args of
                [] ->
                    categories_controller:index(Req);
                [""] ->
                    categories_controller:index(Req)
            end;
        'POST' ->
            case Args of
                ["add"] ->
                    categories_controller:add(Req);
                ["remove"] ->
                    categories_controller:remove(Req)
            end
    end.

index(Req) ->
    Categories = lists:sort(sup_db:all(category)),
    {ok, HTMLOutput} = categories_index_dtl:render([{categories, Categories}]),
    Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

add(Req) ->
    PostData = Req:parse_post(),
    CategoryName = proplists:get_value("category", PostData),
    sup_db:create(#category{name=CategoryName, count=0}),
    Req:respond({302, [{"Location", "/categories"}], ""}).

remove(Req) ->
    PostData = Req:parse_post(),
    CategoryName = proplists:get_value("category", PostData),
    sup_db:destroy(category, CategoryName),
    Req:respond({302, [{"Location", "/categories"}], ""}).
