%%% Description : Simple one for one supervisor for servers/listeners
-module(iserve_server_sup).
-behaviour(supervisor).

-export([start_link/0,
	 start_link/1,
	 add_server/1
	]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%% API functions

%% Register under the module name
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_link(Supervisor) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Supervisor]).

%% Add new dynamic worker
add_server(Conf) ->
    supervisor:start_child(iserve_server_sup, [Conf]).

%%% Supervisor callbacks

init(_) ->
    %% The dynamic worker. We pass it a reference to the master
    %% process as the first arg, more args added at the time the
    %% workers are added.
    AChild = {iserve_server,{iserve_server,start_link,[iserve_master]},
	      temporary,2000,worker,[iserve_server]},
    
    {ok,
     {{simple_one_for_one,10,300}, 
      [AChild]}}.

%%% Internal functions
