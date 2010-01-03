-module(iserve_socket).

-export([start_link/5
         ,send_reply/3, send_reply/4
        ]).

-export([init/1]).

%% TODO: Remove me
%%-compile(export_all).

-include("../include/iserve.hrl").
-include("iserve_socket.hrl").

-define(not_implemented_501, <<"HTTP/1.1 501 Not Implemented\r\n\r\n">>).
-define(forbidden_403, <<"HTTP/1.1 403 Forbidden\r\n\r\n">>).
-define(not_found_404, <<"HTTP/1.1 404 Not Found\r\n\r\n">>).
-define(request_timeout_408, <<"HTTP/1.1 408 Request Timeout\r\n\r\n">>).
-define(request_entity_too_large_413, <<"HTTP/1.1 413 Request Entity Too Large">>).

-define(server_idle_timeout, 30*1000).

start_link(CbMod, CbData, ListenPid, ListenSocket, ListenPort) ->
    proc_lib:spawn_link(?MODULE, init, 
                        [{CbMod, CbData, ListenPid, ListenSocket, ListenPort}]).

init({CbMod, CbData, Listen_pid, Listen_socket, ListenPort}=InArg) ->
    Transport = gen_tcp,
    case Transport:accept(Listen_socket, 2000) of
	{ok, Socket} ->
	    inet:setopts(Socket, 
			 [{send_timeout, 20000},
			  {send_timeout_close, true},
			  {packet, http_bin}]),
            %% Send the cast message to the listener process 
	    %% to create a new acceptor
	    iserve_server:create(Listen_pid, self()),
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #c{sock      = Socket,
		   transport = Transport,
                   port      = ListenPort,
                   peer_addr = Addr,
                   peer_port = Port,
                   cb_mod    = CbMod,
                   cb_data   = CbData},
	    request(C, #req{});
	{error, timeout} ->
	    init(InArg);
	{error, closed} ->
	    exit(normal);
	Else ->
	    error_logger:error_report([{application, iserve},
				       "Accept failed error",
				       io_lib:format("~p",[Else])]),
	    exit({error, accept_failed})
    end.

request(#c{transport=T}=C, Req) ->
    case T:recv(C#c.sock, 0, 5000) of
        {ok, {http_request, Method, Path, Version}} ->
            headers(C, Req#req{vsn = Version,
                               method = Method,
                               uri = Path}, []);
        {error, {http_error, <<"\r\n">>}} ->
	    request(C, Req);
	{error, {http_error, <<"\n">>}} ->
            request(C, Req);
	{error, timeout} ->
            send(C, ?request_timeout_408),
	    exit(normal);
	_Other ->
	    erlang:display(_Other),
	    exit(normal)
    end.

headers(C, _Req, H) when length(H) > 30 ->
    send(C, ?request_entity_too_large_413),
    exit(normal);
headers(#c{transport=T, sock=Socket}=C, Req, H) ->
    case T:recv(Socket, 0, ?server_idle_timeout) of
        {ok, {http_header, _, Header, _, Val}} ->
            headers(C, Req, [{Header, Val}|H]);
        {error, {http_error, "\r\n"}} ->
	    headers(C, Req, H);
	{error, {http_error, "\n"}} ->
            headers(C, Req, H);
        {ok, http_eoh} ->
	    Headers = lists:reverse(H),
	    ContentLength = 
		case proplists:get_value('Content-Length', Headers) of
		    undefined -> undefined;
		    LengthValue when is_binary(LengthValue) ->
			{Length, _} = string:to_integer(binary_to_list(LengthValue)),
			Length
		end,
	    KeepAlive = 
		case proplists:get_value('Connection', Headers) of
		    undefined -> close;
		    ConnectionValue ->
			keep_alive(Req#req.vsn, ConnectionValue)
		end,
	    NewReq = 
		Req#req{headers = Headers,
			connection = KeepAlive,
			content_length = ContentLength},
	    body(C, NewReq);
	_Other ->
	    exit(normal)
    end.

%% Shall we keep the connection alive? 
%% Default case for HTTP/1.1 is yes, default for HTTP/1.0 is no.
%% Exercise for the reader - finish this so it does case insensitivity properly !
keep_alive({1,1}, <<"close">>)      -> close;
keep_alive({1,1}, <<"Close">>)      -> close;
keep_alive({1,1}, _)            -> keep_alive;
keep_alive({1,0}, <<"Keep-Alive">>) -> keep_alive;
keep_alive({1,0}, _)            -> close;
keep_alive({0,9}, _)            -> close;
keep_alive(Vsn, KA) ->
    io:format("Got = ~p~n",[{Vsn, KA}]),
    close.

body(#c{sock = Sock, transport=T} = C, Req) ->
    case Req#req.method of
        'GET' ->
            Close = handle_get(C, Req),
            case Close of
                close ->
                    T:close(Sock);
                keep_alive ->
                    inet:setopts(Sock, [{packet, http_bin}]),
                    request(C, #req{})
            end;
        'POST' when is_integer(Req#req.content_length) ->
            inet:setopts(Sock, [{packet, raw}]),
            case recv_bytes(C, Req#req.content_length, 60000) of
                {ok, Bin} ->
                    Close = handle_post(C, Req#req{body = Bin}),
                    case Close of
                        close ->
                            T:close(Sock);
                        keep_alive ->
                            inet:setopts(Sock, [{packet, http_bin}]),
                            request(C, #req{})
                    end;
                _Other ->
                    exit(normal)
            end;
        _Other ->
            erlang:display(_Other),
            send(C, ?not_implemented_501),
            exit(normal)
    end.

%% A posted body can be zero, but passing zero to gen_tcp:recv asks it
%% to block and read as much as available.
recv_bytes(_C, 0, _Timeout) ->
    {ok, <<>>};
recv_bytes(#c{sock=S, transport=T}, Bytes, Timeout) ->
    T:recv(S, Bytes, Timeout).

handle_get(C, #req{connection = Conn} = Req) ->
    case Req#req.uri of
        {abs_path, _Path} ->
            call_mfa(C, Req),
            Conn;
        {absoluteURI, http, _Host, _, _Path} ->
            call_mfa(C, Req),
            Conn;
        {absoluteURI, _Other_method, _Host, _, _Path} ->
            send(C, ?not_implemented_501),
            close;
        {scheme, _Scheme, _RequestString} ->
            send(C, ?not_implemented_501),
            close;
        _  ->
            send(C, ?forbidden_403),
            close
    end.

handle_post(C, #req{connection = Conn} = Req) ->
    case Req#req.uri of
        {abs_path, _Path} ->
            call_mfa(C, Req),
            Conn;
        {absoluteURI, http, _Host, _, _Path} ->
            call_mfa(C, Req),
            Conn;
        {absoluteURI, _Other_method, _Host, _, _Path} ->
            send(C, ?not_implemented_501),
            close;
        {scheme, _Scheme, _RequestString} ->
            send(C, ?not_implemented_501),
            close;
        _  ->
            send(C, ?forbidden_403),
            close
    end.

call_mfa(C, Req) ->
    Mod = C#c.cb_mod,
    case catch Mod:iserve_request(C, Req) of
        no_reply ->
            ok;

        {'EXIT', Reason} ->
            io:format("Worker Crash = ~p~n",[Reason]),
            exit(normal);
	
	%% These patterns are hidden from callback code
	%% through iserve:reply_* functions

	%% A basic identity http response.
	{respond, StatusCode, Headers0, empty} ->
	    send_reply(C, StatusCode, Headers0);
        {respond, StatusCode, Headers0, Body} ->
            Headers = add_content_length(Headers0, Body),
            send_reply(C, StatusCode, Headers, Body);
	
	%% Chunked transfer-encoding for streaming output
	{stream, StatusCode, Headers0, Pid, Subscribe} ->
	    TE = {'Transfer-Encoding', "chunked"},
	    Headers1 = [TE |Headers0],
            send_reply(C, StatusCode, Headers1),
	    send_chunked(C, Pid, Subscribe)
    end.

%%% Part of the exported API.
send_reply(C, StatusCode, Headers) ->
    send_reply(C, StatusCode, Headers, "").

send_reply(C, StatusCode, Headers, Body) ->
    Enc_headers = enc_headers(Headers),
    Enc_status = enc_status(StatusCode),
    Resp = [<<"HTTP/1.1 ">>, Enc_status, <<"\r\n">>,
            Enc_headers,
            <<"\r\n">>,
            Body],
    send(C, Resp).

       
add_content_length(Headers, Body) ->
    case lists:keysearch('Content-Length', 1, Headers) of
        {value, _} ->
            Headers;
        false ->
            [{'Content-Length', erlang:iolist_size(Body)}|Headers]
    end.


enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
    [atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
    [Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([]) ->
    [].
    
enc_header_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
enc_header_val(Val) ->
    Val.

enc_status(200)  -> "200 OK";
enc_status(304)  -> <<"304 NOT MODIFIED">>;
enc_status(404)  -> "404 NOT FOUND";
enc_status(501)  -> "501 INTERNAL SERVER ERROR";
enc_status(Code) -> [integer_to_list(Code), " WHATEVER"].
  

send(#c{sock = Sock}, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.


send_chunked(C, Pid, Subscribe) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {subscribe, self(), MRef, Subscribe},
    receive
	{subscribed, MRef} ->
	    send_chunked0(C, Pid, MRef);
	{'DOWN', MRef, process, Pid, Info} ->
	    throw({chunked_process_crash, Info})
    end,
    erlang:demonitor(MRef, [flush]),
    ok.
    
send_chunked0(C, Pid, MRef) ->
    Pid ! {ready, MRef},
    receive
	{chunk, MRef, ChunkData} ->
	    ChunkSize = erlang:integer_to_list(erlang:iolist_size(ChunkData), 
					       16),
	    Chunk = [ChunkSize, <<"\r\n">>, ChunkData, <<"\r\n">>],
	    send(C, Chunk),
	    send_chunked0(C, Pid, MRef);
	{last_chunk, MRef, Trailers} ->
	    Enc_trailers = enc_headers(Trailers),
	    Chunk = [<<"0\r\n">>,
		     Enc_trailers,
		     <<"\r\n">>],
	    send(C, Chunk);
	{'DOWN', MRef, process, Pid, Info} ->
	    throw({chunked_process_crash, Info})
    end.

	    
	    
