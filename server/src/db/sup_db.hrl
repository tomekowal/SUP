%% Record definitions
-record	(job,	{message :: term(),
							module :: term(), 
							function :: term(), 
							extra :: term(), 
							status :: failed | in_progress | pending
							}
				).
-record(release, {name, version}).

-record(device, {identity :: nonempty_string(),         %% device identity
                 last_contact :: nonempty_string(),      %% last contact
                 releases :: [#release{}],              %% releases present on device
                 ip :: nonempty_string(),               %% device IP
                 jobs :: [#job{}]
                }
       ).
