-module(iserve_sup).
-behaviour(supervisor).
-export([
	 start_link/0,
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Supervisor = self(),
    
    %% The servers supervisor is for keeping individual servers,
    %% adding and removing them dynamically
    Servers = {servers_sup, 
	      {iserve_server_sup, start_link, [Supervisor]},
	      permanent, 2000, worker, [iserve_server_sup]},

    %% The master is for book keeping all running servers and
    %% to hold information about them.
    Master = {master, 
	      {iserve_master, start_link, [iserve_server_sup]},
	      permanent, 2000, worker, [iserve_master]},
    
    {ok, 
     { {one_for_one, 5, 60}, 
       [Servers, Master]}}.


