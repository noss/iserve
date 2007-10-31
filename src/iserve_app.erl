-module(iserve_app).
-behaviour(application).
-export([
	 start/2,
	 stop/1
        ]).

start(_Type, _StartArgs) ->
    case iserve_sup:start_link() of
	{ok, Pid} -> 
	    alarm_handler:clear_alarm({application_stopped, iserve}),
	    {ok, Pid};
	Error ->
	    alarm_handler:set_alarm({{application_stopped, iserve}, []}),
	    Error
    end.

stop(_State) ->
    alarm_handler:set_alarm({{application_stopped, iserve}, []}),
    ok.
