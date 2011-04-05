-module(hex).
-export([list_to_hex/1, hex_to_list/1, int_to_hex/1]).

%-include_lib("proper/include/proper.hrl").

-export([bin_to_hexstr/1,hexstr_to_bin/1]).

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).


unhex(C) when $0 =< C, C =< $9 ->
    C - $0;
unhex(C) when $a =< C, C =< $f ->
    C - $a.


int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].


list_to_hex(L) when is_list(L) ->
    lists:flatmap(fun(N) -> int_to_hex(N) end, L).


hex_to_list([], L) -> lists:reverse(L);
hex_to_list([A, B | Tail], L) ->
    hex_to_list(Tail, [unhex(A) * 16 + unhex(B)|L]).

hex_to_list(H) ->
    hex_to_list(H, "").


% hex is 0-9,a-f
%% prop_alphabet() ->
%%     ?FORALL(X, list(integer(0,255)),
%%             lists:all(fun(I) -> (($0 =< I) and (I =< $9))
%%                                     or (($a =< I) and (I =< $f)) end,
%%                       list_to_hex(X))
%%            ).

% hex_to_list(list_to_hex(X)) == X
%% prop_identity() ->
%%    ?FORALL(X, list(union(range($0, $9), range($a, $f))),
%%            begin
%%                hex_to_list(list_to_hex(X)) == X
%%            end).
%
% http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html
%

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).
