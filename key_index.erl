-module(key_index).
-export([build/1, search/1]).

build(0, F) -> file:close(F), F;
build(N, F) ->
    Key = crypto:sha(integer_to_list(N)),
    file:write(F, Key),
    build(N-1, F).

build(N) ->
    {ok, Device} = file:open("data.dat", [write]),
    build(N, Device).

search_block(Key, Block) ->
    case Block of
        <<K:20/binary, R/binary>> ->
            case K of
                Key -> found;
                _ -> search_block(Key, R)
            end;
        _ -> not_found
    end.

search(Device, Key) ->
    case file:read(Device, 16380) of
        {ok, Data} ->
            case search_block(Key, Data) of
                found -> found;
                not_found -> search(Device, Key)
            end;
        eof -> not_found
    end.

search(N) ->
    {ok, Device} = file:open("data.dat", [read, binary]),
    Key = crypto:sha(integer_to_list(N)),
    search(Device, Key).
