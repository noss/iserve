-module(iserve_test2).
-export([start/1, iserve_request/2]).
-include("../include/iserve.hrl").

-behaviour(iserve).


start(Port) ->
    iserve:add_server(iserve_master, Port, ?MODULE, x).


iserve_request(_C, Req) ->
    error_logger:info_report(
      lists:zip(
	record_info(fields, req), 
	tl(tuple_to_list(Req)))),
    
    Pid = erlang:spawn(fun() -> stream() end),
    stream_chunked(Pid, "text/html", all).

stream_chunked(FromPid, MimeType, Subscribe) ->
    Headers = [{'Content-Type', MimeType}],
    {stream, 200, Headers, FromPid, Subscribe}.

stream() ->
    receive 
	{subscribe, Pid, Ref, _} ->
	    Pid ! {subscribed, Ref},
	    stream(Pid, Ref, head)
    end.
stream(Pid, Ref, head) ->
    wait_ready(Ref),
    Pid ! {chunk, Ref, chunk_head()},
    stream(Pid, Ref, 0);
stream(Pid, Ref, tail) ->
    wait_ready(Ref),
    Pid ! {chunk, Ref, chunk_tail()},
    Pid ! {last_chunk, Ref, []};
stream(Pid, Ref, N) when is_integer(N), N<10 ->
    wait_ready(Ref),
    receive after 500 -> ok end,
    Pid ! {chunk, Ref, 
	   ["<p>The number is: ", 
	    erlang:integer_to_list(N), "</p>\n"]},
    stream(Pid, Ref, N+1);
stream(Pid, Ref, N) when is_integer(N) ->
    stream(Pid, Ref, tail).



wait_ready(Ref) ->
    receive
	{ready, Ref} ->
	    ok
    after
	30000 ->
	    exit(normal)
    end.

chunk_head() ->
    <<"<html>
<head>
  <title>Streaming test.</title>
</head>
<body>
  <h1>Test of streaming</h1>
">>.

chunk_tail() ->
    <<"</body>
</html>
">>.

