-module(config).
-behaviour(gen_server).

%% Server API
-export([start_link/0, stop/0]).

%% Client API
-export([get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, stopwatch).

%%====================================================================
%% Server API
%%====================================================================

start_link() ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    logger:notice("Started Config server~n"),
    Result.

stop() ->
    gen_server:cast(?SERVER, shutdown).

%%====================================================================
%% Client API
%%====================================================================

get(Key) ->
    gen_server:call(?SERVER, {get_key, Key}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, read()}.

handle_cast(shutdown, Cfg) ->
    {stop, normal, Cfg}.

handle_call({get_key, Key}, _From, Cfg) ->
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

read() -> 
    {ok, Cfg} = file:consult("application.cfg"),
    Cfg.

get_key(_Key, []) -> {error, not_found};
get_key(Key, [{Key, Value} | _Config]) -> Value; %{ok, Value};
get_key(Key, [{_Other, _Value} | Config]) -> get_key(Key, Config).
