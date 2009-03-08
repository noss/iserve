-module(iserve_master_test).

-include_lib("eunit/include/eunit.hrl").

supmock(start_child, _) ->
    {reply, {ok, x}};
supmock(terminate_child, x) ->
    {reply, ok}.


server_test_() ->
    Sup = emock:supervisor(fun supmock/2),
    {ok, Serv} = gen_server:start_link(iserve_master, [Sup], []),
    [
     ?_assertMatch(
	{ok, x}, gen_server:call(Serv, {add_server, {starter, port, callback, context}})),     
     ?_assertMatch(
	ok, gen_server:call(Serv, {del_server, x}))
     
    ].

