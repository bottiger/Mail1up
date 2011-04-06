-module(minit).
-export([boot/0]).

boot() ->
    code:add_path("ebin"),
    code:add_path("erlang-json-parser"),
    code:add_path("ibrowse/ebin"),
    code:add_path("s3erl/ebin"),
    code:add_path("gproc/ebin"),
    config:start_link(),
    application:start(gproc).
