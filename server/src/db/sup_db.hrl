%% Record definitions
-record(job, {message, module, function, extra, status}).
-record(release, {name, version}).

-record(device, {identity :: nonempty_string(),         %% device identity
                 last_contact :: nonempty_string(),      %% last contact
                 releases :: [#release{}],              %% releases present on device
                 ip :: nonempty_string(),               %% device IP
                 jobs :: [#job{}]
                }
       ).
