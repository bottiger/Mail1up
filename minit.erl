-module(minit).
-export([boot/0, aws_key_id/0, aws_key/0, aws_bucket/0]).

boot() ->
    code:add_path("erlang-json-eep-parser"),
    code:add_path("ibrowse/ebin"),
    code:add_path("s3erl/ebin").


aws_key_id() -> "Amazon KEY ID".
aws_key() -> " Amazon secret KEY".
aws_bucket() -> "mailbackupbottiger".
