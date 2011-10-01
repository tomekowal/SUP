%% Record definitions

-record(device, {identity :: nonempty_string(),         %% device identity
                 last_contact :: t_datetime(),          %% last contact
                 releases :: [term()],                  %% releases present on device
                 ip :: nonempty_string()                %% device IP
                }
       ).

-record(program, {id, name, version, description}).
