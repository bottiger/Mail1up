-module(data_store).
-export([load/1, save/2]).
-include("/home/bottiger/dev/Mailup/s3erl/include/s3.hrl"). % For some reason this path is hard-coded. Didn't bother fix it

%-spec load(string()) -> {ok, binary()} | {error, atom()}.

load(Key) -> 
    S3 = s3init(),
    {_, Read} = S3:read_object( config:get(aws_bucket), [Key] ),
    [Read].

save(Key, Data) ->
    %Bucket = "mailbackupbottiger",
    S3 = s3init(),
    io:format("Writing object ~p~n", [S3:write_object( config:get(aws_bucket), Key, Data, "text/plain")] ).

s3init() ->
    ibrowse:start(),
    Credentials = #aws_credentials{ accessKeyId=config:get(aws_key_id), secretAccessKey=config:get(aws_secret_key) },
    s3:new( Credentials ).
