-module(iserve_master_SUITE).
-compile(export_all).


all() ->
    [unittest].

unittest(_Config) ->
    eunit:test(iserve_master_test),
    ok.

