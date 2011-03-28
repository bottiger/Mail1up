-module(minit).
-export([boot/0]).

boot() ->
    code:add_path("erlang-json-eep-parser"),
    code:add_path("ibrowse/ebin"),
    code:add_path("s3erl/ebin").
