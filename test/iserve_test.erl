-module(iserve_test).
-export([start/1, iserve_request/1]).
-include("../include/iserve.hrl").


start(Port) ->
    iserve_server:start(Port, ?MODULE).


iserve_request(Req) ->
    error_logger:info_report(
      lists:zip(record_info(fields, req), tl(tuple_to_list(Req)))),
    
    Headers = [{'Content-Type', "text/html"}],
    iserve:reply_ok(Headers, body_ok()).

body_ok() ->
    <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
<head>
  <title>Welcome to iserve</title>
</head>
<body>
  <h1>Welcome</h1>
  this is iserve.
</body>
</html>
">>.

