-module(config).
-behaviour(gen_server).

%% Server API
-export([start_link/0, stop/0]).

%% Client API
-export([get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, config).
%%====================================================================
%% Server API
%%====================================================================

start_link() ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    %logger:notice("Started Config server"),
    Result.

stop() ->
    gen_server:cast(?SERVER, shutdown).

%%====================================================================
%% Client API
%%====================================================================

-spec get(atom()) -> list().
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
    Value = proplists:get_value(Key, Cfg),
    {reply, Value, Cfg}.

handle_info(_Info, Cfg) ->
    {noreply, Cfg}.

terminate(_Reason, _State) ->
    %logger:notice("Config server is terminating~n"),
    ok.

code_change(_OldVsn, Cfg, _Extra) ->
    {ok, Cfg}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec read() -> list().
read() ->
    %logger:notice("config.erl: Reading application.cfg"),
    application:get_all_env(mail1up).

