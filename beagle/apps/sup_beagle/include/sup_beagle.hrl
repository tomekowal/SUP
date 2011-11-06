-record(inform, {identity :: nonempty_string(),     %% device identity
                 reason :: term(),                  %% session reason
                 releases :: [term()],               %% releases present on device
                 running_applications :: [term()]   %% running applications on device
                }
       ).
