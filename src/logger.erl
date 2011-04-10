-module(logger).
-behavior(gen_event).

-export([start_link/0, stop/0]).

-export([info/1, debug/1, notice/1]).

-export([init/1, handle_event/2, terminate/2, code_change/3, handle_call/2, handle_info/2]).

%
% Server API
%

start_link() ->
    Result = gen_event:start_link({local, gen_logger}),
    gen_event:add_handler(gen_logger, logger, []),
    %logger:notice("Started Config server"),
    Result.

stop() ->
    ok.
    %gen_server:cast(?SERVER, shutdown).

%
%
%

info(Msg) -> gen_log("Info", Msg).
debug(Msg) -> gen_log("Debug", Msg).
notice(Msg) -> gen_log("Notice", Msg).

gen_log(Type, Msg) ->
    gen_event:notify(gen_logger, {Type, Msg}).


%
%
%

init(_Args) ->
    {ok, []}.

handle_event({Type, Msg}, State) ->
    log(Type, Msg, State).

terminate(_Args, _State) ->
    ok.

% Stub implementation
code_change(OldVsn, State, Extra) ->
    {ok, State}.

% Stub implementation
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

% Stub implementation
handle_info(Info, State) ->
    {ok, State}.

log(Prefix, Message, State) -> case config:get(debug) of
                            true -> io:format("~s: ~s~n", [Prefix, Message] );
                            false -> false
                        end,
                        {ok, State}.


