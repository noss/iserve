-module(iserve_example).
-behaviour(iserve).

-export([start_link/0, iserve_request/2]).

start_link() ->
  application:start(sasl),
  application:start(iserve),
  iserve:add_server({8080, {127, 0, 0, 1}}, ?MODULE, []).

iserve_request(_C, _Req) ->
	erlang:display({_C, _Req}),
  Mime = <<"text/plain;charset=utf-8">>, 
  Body = <<"Hello world!">>,
  iserve:reply_ok([], {Mime, Body}).

