-module(iserve_test).
-export([start/1, iserve_request/1]).
-include("../include/iserve.hrl").


start(Port) ->
    iserve_server:start(Port, ?MODULE).


iserve_request(Req) ->
    error_logger:info_msg("~p(~p): GOT Req = ~p~n", 
                          [?MODULE, ?LINE, Req]),

    {200, [], <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
<head>
  <title>Welcome to iserve</title>
</head>
<body>
  Hello
</body>
</html>">>}.
