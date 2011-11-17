-module(sup_beagle_hwaddress).

-compile(export_all).

running_interfaces_hwaddress({_Name, [{flags, Flags} | [{hwaddr, Hwaddr} | _Options ] ]}) ->
    case lists:member(running, Flags) and (Hwaddr =/= [0,0,0,0,0,0]) of
        true ->
            Hwaddr;
        _ ->
            false
    end.

filter_interfaces(false) ->
    false;
filter_interfaces(_) ->
    true.

get_hwaddresses() ->
    {ok, IfList} = inet:getifaddrs(),
    lists:filter(fun sup_beagle_hwaddress:filter_interfaces/1, lists:map(fun sup_beagle_hwaddress:running_interfaces_hwaddress/1, IfList)).

get_first_hwaddress() ->
    [Hwaddr | _T] = get_hwaddresses(),
    lists:flatten(io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B", Hwaddr)).
