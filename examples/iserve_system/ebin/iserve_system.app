{application,iserve_system,
     [{applications,[kernel,stdlib,sasl,iserve]},
      {modules,[iserve_system_app, iserve_system_cb]},
      {mod,{iserve_system_app,[]}},
      {registered,[]},
      {env,[{port,6464},{callback,iserve_system_cb}]},
      {vsn,"1.0"},
      {description,"Web Server Example"}]}.
