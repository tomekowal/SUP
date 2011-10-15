-record(inform, {identity :: nonempty_string(),     %% device identity
                 reason :: term(),                  %% session reason
                 releases :: [term()]               %% releases present on device
                }
       ).
