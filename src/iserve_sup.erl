-module(iserve_sup).
-behaviour(supervisor).
-export([
	 start_link/0,
         init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ConfigArgs = get_config(),
    Server = {iserve_server, 
	      {iserve_server, start_link, ConfigArgs},
	      permanent, 2000, worker, [iserve_server]},
    {ok, {{one_for_one, 10, 1}, [Server]}}.

get_config() ->
    DefaultPort = 8080,
    DefaultCallback = iserve_test,
    try 
	file:consult(filename:join(code:priv_dir(iserve), "iserve.conf")) 
    of
	{error, enoent} ->
	    [DefaultPort, DefaultCallback];
	{ok, Conf} ->
	    [
	     proplists:get_value(port, Conf, DefaultPort),
	     proplists:get_value(callback, Conf, DefaultCallback)
	    ]
    catch
	_ ->
	    [DefaultPort, DefaultCallback]
    end.
