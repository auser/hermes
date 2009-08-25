{application, hermes, [
        {description, "Hermes"},
        {vsn, "0.0.1"},
        {modules, [hermes,ambassador,ambassador_app,ambassador_sup,assets,athens,athens_srv,
                cluster,commandInterface_thrift,config,erlrrd,erlrrd_app,erlrrd_sup,
                hermes_logger,hermes_sup,home,loudmouth,make_boot,mapreduce,mon_server,
                mon_server_sup,monitors,nag,nag_app,nag_sup,poolparty_types,protobuffs_ambassador,
                rest_app,rest_server,rest_server_sup,target_system,testing,thrift_ambassador,utils]},
        {env, [
          {port, 8642},
          {monitors, [cpu, memory]},
          {clouds_config, undefined},
          {cloud_name, undefined}
        ]},
        {registered, [hermes]},
        {applications, [kernel, stdlib, sasl]},
        {included_applications, [stoplight]},
        {start_phases, [{go,[]}]},
        {mod, {application_starter,[hermes,[]]}}
]}.
