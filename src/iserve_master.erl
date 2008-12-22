%%% File    : iserve_master.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : Bookkeeping of running servers

-module(iserve_master).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%% Records
-record(state, {supervisor}).

%%% API
start_link(SupervisorPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [SupervisorPid], []).

%%% gen_server callbacks

init(_) ->
    %% TODO, look up the sibling server supervisor?
    {ok, #state{}}.

handle_call({add_server, Conf}, _From, State) ->
    {Reply, NewState} = do_add_server(Conf, State),
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    {stop, unknown_call, State}.


handle_cast(info, State) ->
    erlang:display(State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, unknown_cast, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

do_add_server({Starter, Port, _Callback, _Context}=Conf, State) 
  when is_pid(Starter),  is_integer(Port) ->
    case iserve_server_sup:add_server(Conf) of
	{ok, Pid} ->
	    %% TODO: Monitor the little bugger
	    {{ok, Pid}, State};
	{error, _Reason}=E ->
	    {E, State}
    end;
do_add_server(X, State) ->
    {{error, X}, State}.

    
	    
    
