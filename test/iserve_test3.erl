%%%-------------------------------------------------------------------
%%% Created :  7 Nov 2007 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%
%%%     Test the ifile:sendfile functionality for fast 
%%%     delivery of static files.
%%%
%%%-------------------------------------------------------------------
-module(iserve_test3).

-export([start/2, iserve_request/2]).

-include("../include/iserve.hrl").
-include_lib("kernel/include/file.hrl").

-behaviour(iserve).

-define(elog(S,A),
        error_logger:error_msg("~p(~p): " ++ S, [?MODULE,?LINE|A])).


-record(data, {
          docroot = "/tmp"
          }).

%%% Example: iserve_test3:start(4422, "/home/tobbe").
start(Port, Docroot) ->
    try
        assert_ifile(),
        {ok,_} = ifile:start(),
        iserve_server:start(Port, ?MODULE, #data{docroot = Docroot})
    catch
         throw:no_ifile ->   
            {error, 
             "Module: ifile, not found in path. "
             "Consider starting erl with:  -pa .../ifile/ebin."}
    end.

assert_ifile() ->
    case code:which(ifile) of
        non_existing -> throw(ifile);
        _            -> ok
    end.
            
%%% Callback
iserve_request(C, Req) ->
    try
        D = iserve:c_cb_data(C),
        Docroot = D#data.docroot,
        Path = safe_path(Docroot, iserve:req_uri(Req)),
        {ok, #file_info{size = Size}} = file:read_file_info(Path),
        Headers = [{'Content-Type', "text/html"},
                   {'Content-Length', Size}],
        iserve:send_reply(C, iserve:status_code(ok), Headers),
        Sock = iserve:c_sock(C),
        {ok,SockFD} = prim_inet:getfd(Sock),
        {ok, 0} = ifile:sendfile(SockFD, Path),
        iserve:no_reply()
    catch
        error:Reason ->
            ?elog("iserve_request/2 ERROR: ~p~n", [Reason])
    end.

    
%%% Remove dangerous parts from file path.
safe_path(Docroot, {abs_path, Path}) ->
    Z = [X || X <- string:tokens(Path, "/"), X =/= ".."],
    filename:join(Docroot, filename:join(Z)).
