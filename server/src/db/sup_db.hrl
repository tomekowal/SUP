%% Record definitions
-record (job, {message :: term(),
               module :: term(),
               function :: term(),
               extra :: term(),
               status :: failed | in_progress | pending
              }
        ).

-record(release, {name, version}).

-record(device, {identity :: nonempty_string(),         %% device identity
                 last_contact :: nonempty_string(),     %% last contact
                 releases :: [#release{}],              %% releases present on device
                 running_applications :: [term()],      %% running applications
                 ip :: nonempty_string(),               %% device IP
                 jobs :: [#job{}],
                 finished_jobs :: [#job{}],
                 categories :: nonempty_string()
                }
       ).

-record(category, {name :: nonempty_string(),  %% category name
                   count :: integer()          %% amount of devices in category
                  }
       ).
