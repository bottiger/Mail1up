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
    Result = gen_server:start_link(user_server, Username, []),
    Result.

stop(Username) ->
    gen_server:cast(Username, shutdown).

%%====================================================================
%% Client API
%%====================================================================

get(Username, Key) ->
    logger:notice("user_server:get(). ServerPid: ??"),
    gen_server:call(server_pid(Username), {get, Key}).

set(Username, Key, Value) -> 
    gen_server:call(server_pid(Username), {set, Key, Value}).

store(Username) ->
   gen_server:call(server_pid(Username), {store}). 

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Creates a process for a user. If the user exists his/her data
%% is retrived. If not a new stub user is created.
%% @end
init(Username) ->
    logger:notice("Started User server: " ++ Username),
    register_server(Username),
    Data = try
        {ok, DataLoad} = data_store:load(Username, user:encryption_key()),
        {ok, DataTerm} = json:json_to_term(DataLoad),
        DataTerm
    catch
        _:_ -> []
    end,
    %logger:notice("loaded data: ~w~n", Data),
    User =  process_s3_data(Username, Data),
    User.

handle_cast(shutdown, User) ->
    {stop, normal, User}.

%% @doc Looks up the value coresponding Key in the user dictonary
%% @end
handle_call({get, Key}, _From, User) ->
    logger:debug("user_server:get called"),
    Value = dict:fetch(Key,User),
    {reply, Value, User};

%% @doc Adds a Key/Value pair to the User dictonary
%% @end
handle_call({set, Key, Value}, _From, User) ->
    NewUser = dict:store(Key,Value,User),
    logger:debug("Created NewUser"),
    {reply, ok, NewUser};

handle_call({store}, _From, User) ->
    user:store(User),
    {reply, ok, User}.

%% @doc Unimplemented
%% @end
handle_info(_Info, User) ->
    {noreply, User}.

%% @doc Is executed when the process is terminated. With or without intend
%% @end
terminate(_Reason, _State) ->
    logger:notice("User server is terminating"),
    ok.

%% @doc Not implemented
%% @end
code_change(_OldVsn, User, _Extra) ->
    {ok, User}.

%%====================================================================
%% Internal functions
%%====================================================================

register_server(ID) ->
    mail1up_utils:register(server_name(ID)).

lookup_server(ID) ->
    mail1up_utils:lookup(server_name(ID)).

await_server(ID) ->
    mail1up_utils:await(server_name(ID)).

server_name(ID) ->
    {userserver, ID}.

server_pid(ID) ->
     await_server(ID).

%%
%% Should be refactored into another module
%%

process_s3_data(Username, {ok, Data}) -> {ok, Data};
process_s3_data(Username, []) -> 
    logger:info("Creating new user: " ++ Username),
    {ok, NewUser} = user:create(Username),
    {ok, UserJson} =  json:term_to_json(NewUser),
    data_store:save(Username, user:encryption_key(), UserJson),
    {ok, NewUser}.


