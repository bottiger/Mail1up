-module(config).
-export([get/1]).

read() -> file:consult("application.cfg").

get(Key) -> 
    {ok, Cfg} = read(),
    get(Key, Cfg).
get(_Key, []) -> {error, not_found};
get(Key, [{Key, Value} | _Config]) -> Value; %{ok, Value};
get(Key, [{_Other, _Value} | Config]) -> get(Key, Config).
