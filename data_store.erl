-module(data_store).
-export([load/1, save/2]).
-include("/home/bottiger/dev/Mailup/s3erl/include/s3.hrl"). % For some reason this path is hard-coded. Didn't bother fix it

%-spec load(string()) -> {ok, binary()} | {error, atom()}.

load(Key) -> 
    ibrowse:start(),
    Credentials = #aws_credentials{ accessKeyId=minit:aws_key_id(), secretAccessKey=minit:aws_key() },
    S3 = s3:new( Credentials ),
    {_, Read} = S3:read_object( minit:aws_bucket(), [Key] ),
    [Read].

save(Key, Data) ->
    ibrowse:start(),
    Bucket = "mailbackupbottiger",
    Credentials = #aws_credentials{ accessKeyId=minit:aws_key_id(), secretAccessKey=minit:aws_key() },
    S3 = s3:new( Credentials ),
    io:format("Writing object ~p~n", [S3:write_object( Bucket, Key, Data, "text/plain")] ).
