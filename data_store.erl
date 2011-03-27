-module(data_store).
-export([ivec/0, load/2, save/3]).
-include("/home/bottiger/dev/Mailup/s3erl/include/s3.hrl"). % For some reason this path is hard-coded. Didn't bother fix it

%-spec load(string()) -> {ok, binary()} | {error, atom()}.

load(StorageKey, DecryptionKey) -> 
    S3 = s3init(),
    {_, Read} = S3:read_object( config:get(aws_bucket), [StorageKey] ),
    crypto:aes_ctr_decrypt(DecryptionKey, ivec(), Read).

save(StorageKey, EncryptionKey, Data) ->
    S3 = s3init(),
    CipherText = crypto:aes_ctr_encrypt(EncryptionKey, ivec(), Data),
    {_, Wrote} = S3:write_object( config:get(aws_bucket), StorageKey, CipherText, "text/plain"),
    logger:info(["Writing object: " ++ Wrote]).

s3init() ->
    ibrowse:start(),
    Credentials = #aws_credentials{ accessKeyId=config:get(aws_key_id), secretAccessKey=config:get(aws_secret_key) },
    s3:new( Credentials ).

ivec() -> crypto:md5(config:get(crypto_global_iv)). % md5 for convenience since it produces 128 bit. This should be enhanced.

