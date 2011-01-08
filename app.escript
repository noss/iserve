#!/usr/bin/env escript

template(Vsn) ->
    {application, iserve,
        [{description, "IServe Web Server"},
         {vsn, Vsn},
         {modules, [
            iserve_sup,
            iserve_app,
            iserve_server,
            iserve_socket
            ]},
         {registered, [ iserve_sup]},
         {applications, [kernel, stdlib, sasl]},
         {mod, {iserve_app, []}},
         {env, []}
         ]}.

main([VSN]) ->
    AppOut = template(VSN),
    io:fwrite('~p.~n', [AppOut]).

