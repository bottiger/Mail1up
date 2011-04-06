-module(minit).
-export([boot/0]).

boot() ->
    code:add_path("erlang-json-parser"),
    code:add_path("ibrowse/ebin"),
    code:add_path("s3erl/ebin"),
    config:start_link().
