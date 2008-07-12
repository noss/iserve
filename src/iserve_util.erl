%%% File    : iserve_util.erl
%%% Author  : Christian S <chsu79@gmail.com>
%%% Created : 12 Jul 2008 by Christian S <chsu79@gmail.com>

-module(iserve_util).

-export([urldecode/1, urlencode/1]).

urldecode(String) ->
    httpd_util:decode_hex(String).

-ifdef(TEST).
urlencode_test_() ->
    [
     ?_assertMatch("ABCXYZ", urlencode("ABCXYZ")),
     ?_assertMatch("abcxyz", urlencode("abcxyz")),
     ?_assertMatch("0189", urlencode("0189")),
     ?_assertMatch("hello%20world", urlencode("hello world")),
     ?_assertMatch("~hello.world", urlencode("~hello.world"))
    ].
-endif.

urlencode(Bin) when is_binary(Bin) ->
    urlencode(binary_to_list(Bin));
urlencode(Integer) when is_integer(Integer) ->
    urlencode(erlang:integer_to_list(Integer));
urlencode(Bytes) ->
    lists:flatten([maybe_quote(Byte) || Byte <- Bytes]).


maybe_quote(Byte) when Byte >= $A, Byte =< $Z;
                       Byte >= $a, Byte =< $z;
                       Byte >= $0, Byte =< $9;
                        Byte =:= $-;
                        Byte =:= $_;
                        Byte =:= $.;
                        Byte =:= $~ ->
    Byte;
maybe_quote(Byte) ->
    io_lib:format("%~2.16.0b", [Byte]).

