%%%-------------------------------------------------------------------
%%% File    : iserve_example_SUITE.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : 
%%%
%%% Created :  8 Mar 2009 by Christian <chsu79@gmail.com>
%%%-------------------------------------------------------------------
-module(iserve_example_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

% -include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:start(sasl),
    application:start(inets),
    application:start(iserve),
    Config.

end_per_suite(Config) ->
    application:stop(iserve),
    application:stop(inets),
    application:stop(sasl),
    ok.

all() -> 
    [a_test_case].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

a_test_case() -> 
    [].

a_test_case(Config) -> 
    application:start(iserve_example),
    
    {ok, {Reply, Headers, Body}} = http:request("http://127.0.0.1:8080/foo"),
    
    {_Protocol, 200, _Msg} = Reply,

    application:stop(iserve_example),
    ok.
