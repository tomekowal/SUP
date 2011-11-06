-module(sup_beagle_hwaddress).

-compile(export_all).

running_interfaces_hwaddress({_Name, [{flags, Flags} | Options]}) ->
    case lists:member(running, Flags) of
        true ->
            lists:keyfind(hwaddr, 1, Options);
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
    [{hwaddr, Hwaddr} | _T] = get_hwaddresses(),
    io_lib:format("~.16B:~.16B:~.16B:~.16B:~.16B:~.16B", Hwaddr).
