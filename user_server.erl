-module(user_server).
-behaviour(gen_server).

%% Server API
-export([start_link/1, stop/1]).

%% Client API
-export([get/2, set/3]).
-compile(export_all). % remove this. only for debugging
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Server API
%%====================================================================

start_link(Username) ->
    logger:notice("Starting User server: " ++ Username),
    Result = gen_server:start_link({local, list_to_atom(Username)}, user_server, Username, []),
    Result.

stop(Username) ->
    gen_server:cast(Username, shutdown).

%%====================================================================
%% Client API
%%====================================================================

get(Username, Key) ->
    gen_server:call(Username, {get, Key}).

set(Username, Key, Value) -> 
    gen_server:call(Username, {set, Key, Value}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Username) ->
    logger:notice("Started User server: " ++ Username),
    Data = try
        json:json_to_term(data_store:load(Username, encryption_key()))
    catch
        _:_ -> []
    end,
    %logger:notice("loaded data: ~w~n", Data),
    User =  process_s3_data(Username, Data),
    {ok, User}.

handle_cast(shutdown, User) ->
    {stop, normal, User}.

handle_call({get, Key}, _From, User) ->
    logger:debug("user_server:get called"),
    Value = dict:fetch(Key,User),
    {reply, Value, User};

handle_call({set, Key, Value}, _From, User) ->
    {ok, NewUser} = store_user(dict:store(Key,Value,User)),
    {reply, ok, NewUser}.

handle_info(_Info, User) ->
    {noreply, User}.

terminate(_Reason, _State) ->
    logger:notice("User server is terminating"),
    ok.

code_change(_OldVsn, User, _Extra) ->
    {ok, User}.

%%====================================================================
%% Internal functions
%%====================================================================


process_s3_data(Username, {ok, Data}) -> {ok, json:json_to_term(Data)};
process_s3_data(Username, []) -> 
    logger:info("Creating new user: " ++ Username),
    {ok, NewUser} = new_user(Username),
    {ok, UserJson} =  json:term_to_json(NewUser),
    %io:format("json: ~w~n", UserJson),
    data_store:save(Username, encryption_key(), UserJson),
    {ok, NewUser}.

encryption_key() ->
    config:get(crypto_bootstrap).

password_hash(Password, Salt) ->
    crypto:sha(Password ++ Salt).

new_user(Username) -> 
    Salt = hex:bin_to_hexstr(crypto:rand_bytes(16)),
    User = dict:new(),
    User1 = dict:store("username",Username,User),
    User2 = dict:store("salt",Salt,User1),
    {ok, User2}.
    
store_user(Data) -> 
    Username = dict:fetch("username",Data),
    logger:info("Updating user data for username: " ++ Username),
    data_store:save(Username, encryption_key(), json:term_to_json(Data)),
    {ok, Data}.


