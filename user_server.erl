-module(user_server).
-behaviour(gen_server).

%% Server API
-export([start_link/1, stop/0]).

%% Client API
-export([get/1, set/2]).
-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, user_server).

%%====================================================================
%% Server API
%%====================================================================

start_link(Username) ->
    logger:notice("Starting User server: " ++ Username),
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, Username, []),
    Result.

stop() ->
    gen_server:cast(?SERVER, shutdown).

%%====================================================================
%% Client API
%%====================================================================

get(Key) ->
    gen_server:call(?SERVER, {get, [], []}).

set(Key, Value) -> 
    gen_server:call(?SERVER, {set, Value, []}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Username) ->
    logger:notice("Started User server: " ++ Username),
    Data = try
        data_store:load(Username ++ ".json", encryption_key())
    catch
        _:_ -> []
    end,
    process_s3_data(Username, Data).

handle_cast(shutdown, User) ->
    {stop, normal, User}.

handle_call({get, Key}, _From, User) ->
    Value = dict:fetch(Key,User),
    {reply, Value, User};

handle_call({set, Key, Value}, _From, User) ->
    NewUser = dict:store(Key,Value,User),
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


process_s3_data(Username, {ok, Data}) -> {ok, term_to_binary(Data)};
process_s3_data(Username, []) -> 
    logger:info("Creating new user: " ++ Username),
    New_user = new_user(Username),
    data_store:save(Username, encryption_key(), New_user),
    {ok, New_user}.

encryption_key() ->
    hex:hexstr_to_bin("90B340950A1510CF1196AACFBB3F15B0E62CFD03574279A0E51B1ED629B510F2").

password_hash(Password, Salt) ->
    crypto:sha(Password ++ Salt).

new_user(Username) -> 
    Salt = hex:bin_to_hexstr(crypto:rand_bytes(16)),
    User = dict:new(),
    User1 = dict:store("username",Username,User),
    User2 = dict:store("salt",Salt,User1),
    term_to_binary(User2). % FIXME - potential unsecure
    


