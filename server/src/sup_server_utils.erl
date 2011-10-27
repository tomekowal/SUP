-module(sup_server_utils).
-compile(export_all).
-include("db/sup_db.hrl").

ip4addr_to_list({Oct1,Oct2,Oct3,Oct4}) ->
    integer_to_list(Oct1)++"."++integer_to_list(Oct2)++"."++integer_to_list(Oct3)++"."++integer_to_list(Oct4).

make_release_record(Release) ->
    {Name, Version, _Apps, _State} = Release,
    #release{
              name = Name,
              version = Version
            }.

list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case erl_parse:parse_term(T) of
        {ok, Term} ->
            Term;
        {error, Error} ->
            Error
    end.
