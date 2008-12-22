-module(iserve_server).
-behaviour(gen_server).

-export([start/2, 
	 start/3,
	 start_link/2,
	 start_link/3,
	 start_link/4,
         create/2
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          listen_socket,
          port,
          acceptor,
          cb_mod,
          cb_data 
         }).

%%% API

start(Port, CbMod) when is_integer(Port), is_atom(CbMod) ->
    start(Port, CbMod, undef).

start(Port, CbMod, CbData) when is_integer(Port), is_atom(CbMod) ->
    Name = list_to_atom(lists:concat([iserve_, Port])),
    gen_server:start({local, Name}, ?MODULE, [Port,CbMod,CbData], []).

start_link(Master, {_Starter, Port, Callback, Context}) ->
    start_link(Master, Port, Callback, Context).

start_link(_Master, Port, CbMod) when is_integer(Port), is_atom(CbMod) ->
    start_link(Port, CbMod, undef).

start_link(_Master, Port, CbMod, CbData) when is_integer(Port), is_atom(CbMod) ->
    Name = list_to_atom(lists:concat([iserve_, Port])),
    gen_server:start_link({local, Name}, ?MODULE, [Port,CbMod,CbData], []).

%% Send message to cause a new acceptor to be created
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).


%%% Callbacks

init([Port,CbMod,CbData]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port,[binary, {packet, http},
                              {reuseaddr, true},
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
                        cb_data       = CbData}};
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
