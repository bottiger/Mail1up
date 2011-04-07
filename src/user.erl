-module(user).
-export([encryption_key/0, create/1, store/1]).

password_hash(Password, Salt) ->
    crypto:sha(Password ++ Salt).

create(Username) ->
    Salt = hex:bin_to_hexstr(crypto:rand_bytes(16)),
    User = dict:new(),
    User1 = dict:store("username",Username,User),
    User2 = dict:store("salt",Salt,User1),
    {ok, User2}.

store(Data) ->
    Username = dict:fetch("username",Data),
    logger:info("Updating user data for username: " ++ Username),
    {ok, JsonData} =  json:term_to_json(Data),
    data_store:save(Username, encryption_key(), JsonData),
    {ok, Data}.

encryption_key() ->
    config:get(crypto_bootstrap).

