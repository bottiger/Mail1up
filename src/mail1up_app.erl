-module(mail1up_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(gproc),
    mail1up_sup:start_link().
    %gen_event:add_handler(gen_logger, logger, []).

stop(_State) ->
    ok.
