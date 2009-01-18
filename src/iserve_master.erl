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
    call_add_server(Conf, State);
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

call_add_server({Starter, _Port, _Callback, _Context}=Conf, State) 
  when is_pid(Starter) ->
    case iserve_server_sup:add_server(Conf) of
	{ok, Pid} ->
	    %% TODO: Monitor the little bugger
	    Reply = {ok, Pid},
	    NewState = State,
	    {reply, Reply, NewState};
	{error, _Reason}=E ->
	    Reply = {fail, E},
	    NewState = State,
	    {reply, Reply, NewState}
    end;
call_add_server(_X, State) ->
    {reply, fail, State}.

    
	    
    
