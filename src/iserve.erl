%%% File    : iserve.erl
%%% Author  : Christian S <chsu79@gmail.com>
%%% Description : External iserve API 
%%% Created :  2 Nov 2007 by Christian S <chsu79@gmail.com>

-module(iserve).

-include("../include/iserve.hrl").
-include("iserve_socket.hrl").

-export([behaviour_info/1]).

%% API to use from iserver_socket-behavior-callback modules.
-export([reply_ok/2
         ,reply_not_modified/1
	 ,reply_not_found/2
         ,reply_redirect/2
         ,reply_temp_redirect/2
	 ,reply_raw/3
         ,no_reply/0
         ,send_reply/3
         ,send_reply/4
         ,status_code/1
        ]).

-export([req_method/1
         ,req_content_length/1
	 ,req_uri/1
         ,req_headers/1
	 ,req_body/1
         ,req_http_version/1
        ]).

-export([c_sock/1
         ,c_port/1
         ,c_peer_addr/1
         ,c_peer_port/1
         ,c_cb_mod/1
         ,c_cb_data/1
        ]).

-export([
	 start/0,
	 add_server/3
	 ]).
     
behaviour_info(callbacks) ->
    [{iserve_request,2}];
behaviour_info(_Other) ->
    undefined.

start() ->
    application:start(iserve).

add_server(Port, Module, Args) ->
    Conf = {self(), Port, Module, Args},
    gen_server:call(iserve_master, {add_server, Conf}).

send_reply(C, StatusCode, Headers) ->
    iserve_socket:send_reply(C, StatusCode, Headers).

send_reply(C, StatusCode, Headers, Body) ->
    iserve_socket:send_reply(C, StatusCode, Headers, Body).


no_reply() ->
    no_reply.

reply_ok(Headers, Body)                              ->
    response(ok, Headers, Body).
reply_not_modified(Headers)                          ->
    response(not_modified, Headers, <<>>).
reply_not_found(Headers, Body)                       ->
    response(not_found, Headers, Body).
reply_redirect(Headers, URL)                         ->
    LocationHeader = {'Location', URL},
    response(redirect, [LocationHeader |Headers], <<>>).
reply_temp_redirect(Headers, URL)                         ->
    LocationHeader = {'Location', URL},
    response(temporary_redirect, [LocationHeader |Headers], <<>>).
reply_raw(Status, Headers, Body)                     ->
    response(Status, Headers, Body).

response(Status, Headers, Body) when is_atom(Status) ->
    response(status_code(Status), Headers, Body);
response(StatusCode, Headers, Body)                  ->
    {respond, StatusCode, Headers, Body}.


%% Named HTTP status codes to numeric code.
status_code(ok)                    -> 200;
status_code(moved_permanently)     -> 301;
status_code(found)                 -> 302;
status_code(redirect)              -> 302;
status_code(see_other)             -> 303;
status_code(not_modified)          -> 304;
status_code(temporary_redirect)    -> 307;
status_code(bad_request)           -> 400;
status_code(unauthorized)          -> 401;
status_code(forbidden)             -> 403;
status_code(not_found)             -> 404;
status_code(internal_server_error) -> 500;
status_code(service_unavailable)   -> 503.


req_method(#req{method=Method})              ->    Method.
req_content_length(#req{content_length=Len}) ->    Len.
req_uri(#req{uri=URI})                       ->    URI.
req_headers(#req{headers=Hs})                ->    Hs.
req_body(#req{body=Data})                    ->    Data.
req_http_version(#req{vsn=Version})          ->    Version.


c_sock(#c{sock = Sock})               -> Sock.
c_port(#c{port = Port})               -> Port.
c_peer_addr(#c{peer_addr = PeerAddr}) -> PeerAddr.
c_peer_port(#c{peer_port = PeerPort}) -> PeerPort.
c_cb_mod(#c{cb_mod = CbMod})          -> CbMod.
c_cb_data(#c{cb_data = CbData})       -> CbData.
     
