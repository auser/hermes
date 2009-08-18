{application, hermes, [
        {description, "Hermes"},
        {vsn, "0.1"},
        {modules, [hermes]},
        {env, [
          {port, 8642},
          {monitors, [cpu, memory]},
          {clouds_config, undefined},
          {cloud_name, undefined}
        ]},
        {registered, [hermes]},
        {applications, [kernel, stdlib]},
        {mod, {hermes, []}}
]}.

