-module(user_server).
-behaviour(gen_server).

%% Server API
-export([start_link/2, stop/0]).

%% Client API
-export([get/1, set/2]).
-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, user_server).

%%====================================================================
%% Server API
%%====================================================================

start_link(Username, Password) ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, {Username, Password}, []),
    logger:notice("Started User server"),
    Result.

stop() ->
    gen_server:cast(?SERVER, shutdown).

%%====================================================================
%% Client API
%%====================================================================

get(Key) ->
    gen_server:call(?SERVER, {user_prop, [], []}).

set(Key, Value) -> Value.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Username, Password}) ->
    process_s3_data(Username, Password, data_store:load(Username ++ ".json", user_encryption_key(Username, Password))).

handle_cast(shutdown, Cfg) ->
    {stop, normal, Cfg}.

handle_call({user_prop, User, Key}, _From, Cfg) ->
    %
    % Loopup user thread or create new.
    %
    Value = 1,
    {reply, Value, Cfg}.

handle_info(_Info, Cfg) ->
    {noreply, Cfg}.

terminate(_Reason, _State) ->
    logger:notice("User server is terminating"),
    ok.

code_change(_OldVsn, Cfg, _Extra) ->
    {ok, Cfg}.

%%====================================================================
%% Internal functions
%%====================================================================


process_s3_data(Username, Password, {ok, Data}) -> {ok, json_eep:json_to_term(Data)};
process_s3_data(Username, Password, {s3error,"NoSuchKey",ErrorMsg}) -> 
    logger:info("Creating new user: " ++ Username),
    New_user = new_user(Username, Password),
    data_store:save(Username, user_encryption_key(Username, Password), New_user),
    New_user.

%
% NOT FINISHED!
%
user_encryption_key(Username, Password) ->
    Userpass = user_namepass_concat(Username, Password),
    crypto:sha(Userpass ++ hex:hex_to_list(crypto:sha(Userpass))).

user_namepass_concat(Username, Password) ->
    Username ++ Password.

password_hash(Password, Salt) ->
    crypto:sha(Password ++ Salt).

new_user(Username, Password) -> 
    Salt = hex:hex_to_list(crypto:rand_bytes(16)),
    User0 = [],
    User1 = lists:append(User0,[{username,Username}]),
    User2 = lists:append(User1,[{password,password_hash(Password, Salt)}]),
    User3 = lists:append(User2,[salt,Salt]),
    User3.
