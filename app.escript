#!/usr/bin/env escript

template() ->
    {application, iserve,
        [{description, "Web Server"},
         {vsn, "%ISERVE_VSN%"},
         {modules, [
            iserve_sup,
            iserve_app,
            iserve_server,
            iserve_socket
            ]},

         {registered, [ iserve_sup]},
         {applications, [kernel, stdlib, sasl]},
     {mod, {iserve_app, []}},
     {env, [{port, 8080}, {callback, iserve_test}]}]}.

main([VSN]) ->
    {application, Name, Settings} = template(),
    D1 = dict:from_list(Settings),
    D2 = dict:store(vsn, VSN, D1),
    AppOut = {application, Name, dict:to_list(D2)},
    io:fwrite('~p.~n', [AppOut]).

