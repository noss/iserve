-module(iserve_sup).
-behaviour(supervisor).
-export([
	 start_link/0,
	 start_link/2,
	 start_link/3,
         init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_link(Port, Callback) ->
    supervisor:start_link(?MODULE, [Port, Callback]).
start_link(Port, Callback, CallbackData) ->
    supervisor:start_link(?MODULE, [Port, Callback, CallbackData]).

init([]) ->
    ConfigArgs = get_config(),
    init(ConfigArgs);
init(ConfigArgs) ->
    Server = {iserve_server, 
	      {iserve_server, start_link, ConfigArgs},
	      permanent, 2000, worker, [iserve_server]},
    {ok, {{one_for_one, 10, 1}, [Server]}}.

get_config() ->
    {ok, Port} = application:get_env(iserve, port),
    {ok, Callback} = application:get_env(iserve, callback),
    [Port, Callback].

