-module(user).
-behaviour(gen_server).

%% Server API
-export([start_link/0, stop/0]).

%% Client API
-export([get/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Server API
%%====================================================================

start_link() ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    logger:notice("Started User server"),
    Result.

stop() ->
    gen_server:cast(?SERVER, shutdown).

%%====================================================================
%% Client API
%%====================================================================

get(User, Key) ->
    gen_server:call(?SERVER, {user_prop, User, Key}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok}.

handle_cast(shutdown, Cfg) ->
    {stop, normal, Cfg}.

handle_call({user_prop, User, Key}, _From, Cfg) ->
    %
    % Loopup user thread or create new.
    %
    Value = get_key(Key, Cfg),
    {reply, Value, Cfg}.

handle_info(_Info, Cfg) ->
    {noreply, Cfg}.

terminate(_Reason, _State) ->
    logger:notice("Config server is terminating~n"),
    ok.

code_change(_OldVsn, Cfg, _Extra) ->
    {ok, Cfg}.

%%====================================================================
%% Internal functions
%%====================================================================

