%% -*- mode: Erlang; indent-tabs-mode: nil; -*- 
-module(iserve_example).

%% application callbacks
-behaviour(application).
-export([start/2, stop/1]).

%% iserve callbacks
-behaviour(iserve).
-export([iserve_request/2]).


start(_Type, _Args) ->
    {ok, Port} = application:get_env(?MODULE, port),
    Master = iserve_master,
    {ok, Pid} = iserve:add_server(Master, {Port, {127, 0, 0, 1}}, ?MODULE, []),
    {ok, self(), Pid}.

stop(Pid) ->
    iserve:remove_server(Pid).


iserve_request(_C, _Req) ->
    Mime = <<"text/plain;charset=utf-8">>, 
    Body = <<"Hello world!">>,
    iserve:reply_ok([], {Mime, Body}).

