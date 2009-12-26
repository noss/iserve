-module(iserve_server).
-behaviour(gen_server).

-export([
	 start_link/2,
         create/2
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          listen_socket,
          port,
          acceptor,
          cb_mod,
          cb_data,
	  starter 
         }).

%%% API

start_link(_Master, {Starter, PortAndIP, Callback, Context}) ->
    Name = list_to_atom(lists:concat([iserve_, port_of(PortAndIP)])),
    gen_server:start_link({local, Name}, ?MODULE, 
			  [Starter, PortAndIP, Callback, Context], []).

%% Send message to cause a new acceptor to be created
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).


%%% Callbacks

port_of({Port, _IP}) -> Port;
port_of(Port)        -> Port.

%% TODO: Not very ipv6 safe, uh.
ip_of({_Port, IP}) -> IP;
ip_of(_Port)       -> {0,0,0,0}.
	     

init([Starter, PortAndIP, CbMod, CbData]) ->
    process_flag(trap_exit, true),
    erlang:link(Starter),
    Port = port_of(PortAndIP),
    IP = ip_of(PortAndIP),
    case gen_tcp:listen(Port,[binary,
                              {reuseaddr, true},
			      {ip, IP},
                              {active, false},
                              {backlog, 30}]) of
	{ok, ListenSocket} ->
            %%Create first accepting process
	    Pid = iserve_socket:start_link(CbMod, CbData, 
					   self(), ListenSocket, Port),
	    {ok, #state{listen_socket = ListenSocket,
                        port          = Port,
			acceptor      = Pid,
                        cb_mod        = CbMod,
                        cb_data       = CbData,
			starter       = Starter
		       }};
	{error, Reason} ->
	    {stop, Reason}
    end.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% Called by gen_server framework when the cast message from create/2 is received
handle_cast({create, _Pid}, #state{listen_socket = Listen_socket} = State) ->
    New_pid = iserve_socket:start_link(State#state.cb_mod,
                                       State#state.cb_data, 
                                       self(), 
                                       Listen_socket, 
                                       State#state.port),
    {noreply, State#state{acceptor=New_pid}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', StartPid, normal}, #state{starter=StartPid} = State) ->
    {stop, normal, State};
handle_info({'EXIT', Pid, normal}, #state{acceptor=Pid} = State) ->
    {noreply, State};

%% The current acceptor has died, wait a little and try again
handle_info({'EXIT', Pid, _Abnormal}, #state{acceptor=Pid} = State) ->
    timer:sleep(2000),
    iserve_socket:start_link(State#state.cb_mod, 
                             State#state.cb_data, 
                             self(), 
                             State#state.listen_socket, 
                             State#state.port),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
