-module(sup_beagle_download).
-export([download_tar/1]).

%%------------------------------------------------------------------------------
%% @doc Downloads a tar.gz archive with a release from FTP server configured in
%% app file for sup_beagle application. Archive is downloaded into 'releases'
%% directory, in order to be ready to be unpacked and installed by release
%% handler.
%% @end
%% -----------------------------------------------------------------------------
download_tar(Name) ->
    {ok, Host} = application:get_env(sup_beagle, ftp_host),
    {ok, Port} = application:get_env(sup_beagle, ftp_port),
    {ok, User} = application:get_env(sup_beagle, ftp_user),
    {ok, Pass} = application:get_env(sup_beagle, ftp_pass),
    {ok, Pid} = inets:start(ftpc, [{host, Host}, {port, Port}]),
    ok = ftp:user(Pid, User, Pass),
    ok = ftp:recv(Pid, Name++".tar.gz", "releases/"++Name++".tar.gz"),
    ok = ftp:close(Pid),
    ok.
