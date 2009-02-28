%% -*- mode: Erlang; -*-
{application, iserve_example,
 [{description, "Web Server Example"},
  {vsn, "1"},
  {modules, [iserve_example]},
  {applications, [iserve]},
  {mod, {iserve_example, []}},
  {env, [{port, 8080}]}]}.
