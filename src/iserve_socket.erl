-module(iserve_socket).

-export([start_link/4]).

-export([init/1]).
-include("../include/iserve.hrl").

-define(not_implemented_501, "HTTP/1.1 501 Not Implemented\r\n\r\n").
-define(forbidden_403, "HTTP/1.1 403 Forbidden\r\n\r\n").
-define(not_found_404, "HTTP/1.1 404 Not Found\r\n\r\n").

-record(c,  {sock,
             port,
             peer_addr,
             peer_port,
             cb_mod      % callback module M:iserve_request(#req{})
	     }).

-define(server_idle_timeout, 30*1000).

start_link(CbMod, ListenPid, ListenSocket, ListenPort) ->
    proc_lib:spawn_link(?MODULE, init, 
                        [{CbMod, ListenPid, ListenSocket, ListenPort}]).

init({CbMod, Listen_pid, Listen_socket, ListenPort}) ->
    case catch gen_tcp:accept(Listen_socket) of
	{ok, Socket} ->
            %% Send the cast message to the listener process to create a new acceptor
	    iserve_server:create(Listen_pid, self()),
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #c{sock      = Socket,
                   port      = ListenPort,
                   peer_addr = Addr,
                   peer_port = Port,
                   cb_mod    = CbMod},
	    request(C, #req{}); %% Jump to state 'request'
	Else ->
	    error_logger:error_report([{application, iserve},
				       "Accept failed error",
				       io_lib:format("~p",[Else])]),
	    exit({error, accept_failed})
    end.
request(C, Req) ->
    case gen_tcp:recv(C#c.sock, 0, 30000) of
        {ok, {http_request, Method, Path, Version}} ->
            headers(C, Req#req{vsn = Version,
                               method = Method,
                               uri = Path}, []);
        {error, {http_error, "\r\n"}} ->
	    request(C, Req);
	{error, {http_error, "\n"}} ->
            request(C, Req);
	_Other ->
	    exit(normal)
    end.

headers(C, Req, H) ->
    case gen_tcp:recv(C#c.sock, 0, ?server_idle_timeout) of
        {ok, {http_header, _, 'Content-Length', _, Val}} ->
            Len = list_to_integer(Val),
            headers(C, Req#req{content_length = Len}, [{'Content-Length', Len}|H]);
        {ok, {http_header, _, 'Connection', _, Val}} ->
            Keep_alive = keep_alive(Req#req.vsn, Val),
            headers(C, Req#req{connection = Keep_alive}, [{'Connection', Val}|H]);
        {ok, {http_header, _, Header, _, Val}} ->
            headers(C, Req, [{Header, Val}|H]);
        {error, {http_error, "\r\n"}} ->
	    headers(C, Req, H);
	{error, {http_error, "\n"}} ->
            headers(C, Req, H);
        {ok, http_eoh} ->
            body(C, Req#req{headers = lists:reverse(H)});
	_Other ->
	    exit(normal)
    end.

%% Shall we keep the connection alive? 
%% Default case for HTTP/1.1 is yes, default for HTTP/1.0 is no.
%% Exercise for the reader - finish this so it does case insensitivity properly !
keep_alive({1,1}, "close")      -> close;
keep_alive({1,1}, "Close")      -> close;
keep_alive({1,1}, _)            -> keep_alive;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive({1,0}, _)            -> close;
keep_alive({0,9}, _)            -> close;
keep_alive(Vsn, KA) ->
    io:format("Got = ~p~n",[{Vsn, KA}]),
    close.

body(#c{sock = Sock} = C, Req) ->
    case Req#req.method of
        'GET' ->
            Close = handle_get(C, Req),
            case Close of
                close ->
                    gen_tcp:close(Sock);
                keep_alive ->
                    inet:setopts(Sock, [{packet, http}]),
                    request(C, #req{})
            end;
        'POST' when is_integer(Req#req.content_length) ->
            inet:setopts(Sock, [{packet, raw}]),
            case gen_tcp:recv(Sock, Req#req.content_length, 60000) of
                {ok, Bin} ->
                    Close = handle_post(C, Req#req{body = Bin}),
                    case Close of
                        close ->
                            gen_tcp:close(Sock);
                        keep_alive ->
                            inet:setopts(Sock, [{packet, http}]),
                            request(C, #req{})
                    end;
                _Other ->
                    exit(normal)
            end;
        _Other ->
            send(C, ?not_implemented_501),
            exit(normal)
    end.

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
    case catch Mod:iserve_request(Req) of
        {'EXIT', Reason} ->
            io:format("Worker Crash = ~p~n",[Reason]),
            exit(normal);
        {200, Headers0, Body} ->
            Headers = add_content_length(Headers0, Body),
            Enc_headers = enc_headers(Headers),
            Resp = [<<"HTTP/1.1 200 OK\r\n">>,
                    Enc_headers,
                    <<"\r\n">>,
                    Body],
            send(C, Resp)
    end.
       
add_content_length(Headers, Body) ->
    case lists:keysearch('Content-Length', 1, Headers) of
        {value, _} ->
            Headers;
        false ->
            [{'Content-Length', size(Body)}|Headers]
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

%% Split the path at the ?. This would have to do all sorts of
%% horrible ../../ path checks and %C3 etc decoding if we wanted to
%% retrieve actual paths to real filesystem files. As it is we only
%% want to look it up as a key in mnesia/ets :)
split_at_q_mark([$?|T], Acc) ->
    {lists:reverse(Acc), T};
split_at_q_mark([H|T], Acc) ->
    split_at_q_mark(T, [H|Acc]);
split_at_q_mark([], Acc) ->
    {lists:reverse(Acc), []}.

  
send(#c{sock = Sock}, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.
