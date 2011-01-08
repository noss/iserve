#!/usr/bin/env escript
%%! -pa ebin

release(Name, Vsn, Apps) ->
    {release, {Name, Vsn}, {erts, erlang:system_info(version)}, Apps}.

appvsn(App) ->
    application:load(App),
    {ok, Vsn} = application:get_key(App, vsn),
    {App, Vsn}.

main([AppStr, AppName, AppVsn]) ->
    App = list_to_atom(AppStr),
    application:load(App),
    {ok, AppMods} = application:get_key(App, applications),
    Apps = [appvsn(Mod) || Mod <- AppMods ++ [App]],
    RelOut = release(AppName, AppVsn, Apps),
    io:fwrite('~p.~n', [RelOut]).

