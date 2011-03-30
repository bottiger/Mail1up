-module(erl_parser).
-export([find/2]).


find(_Key, []) -> {error, not_found};
find(Key, [{Key, Value} | _Haystack]) -> {ok, Value};
find(Key, [{_Other, _Value} | Haystack]) -> find(Key, Haystack).
