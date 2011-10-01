%% Record definitions
-record(job, {module, function, extra, status}).
-record(release, {name, version}).

-record(device, {identity :: nonempty_string(),         %% device identity
                 last_contact :: {term(), term()},      %% last contact
                 releases :: [#release{}],              %% releases present on device
                 ip :: nonempty_string(),               %% device IP
                 jobs :: [#job{}]
                }
       ).


