-module(management_controller).
-compile(export_all).

-include("../db/sup_db.hrl").

dispatch(Req, Args) ->
    case Req:get(method) of
        'GET' ->
            case Args of
                [] ->
                    management_controller:index(Req);
                [""] ->
                    management_controller:index(Req)
            end;
        'POST' ->
            case Args of
                ["add_category"] ->
                    management_controller:add_category(Req);
                ["remove_category"] ->
                    management_controller:remove_category(Req)
            end
    end.

index(Req) ->
    Categories = lists:sort(sup_db:all(category)),
    {ok, HTMLOutput} = management_index_dtl:render([{categories, Categories}]),
    Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

add_category(Req) ->
    PostData = Req:parse_post(),
    CategoryName = proplists:get_value("category", PostData),
    sup_db:create(#category{name=CategoryName, count=0}),
    Req:respond({302, [{"Location", "/management"}], ""}).

remove_category(Req) ->
    PostData = Req:parse_post(),
    CategoryName = proplists:get_value("category", PostData),
    sup_db:destroy(category, CategoryName),
    Req:respond({302, [{"Location", "/management"}], ""}).
