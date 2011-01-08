-module(iserve_system_app).
-behaviour(application).

-export([
         start/2,
         stop/1
         ]).

start(_Type, _StartArgs) ->
    {ok, Callback} = application:get_env(callback),
    {ok, Port} = application:get_env(port),
    {ok, Server} = iserve:add_server(iserve_master, Port, Callback, x),
    {ok, Server}.

stop(_State) ->
    ok.
