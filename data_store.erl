-module(data_store).
-export([ivec/0, load/2, save/3, test/0]).
-include("s3erl/include/s3.hrl"). % For some reason this path is hard-coded. Didn't bother fix it

%-spec save(string(), binary(), binary()) -> {ok, binary()} | {error, atom()}.

load(StorageKey, DecryptionKey) ->
    S3 = s3init(),
    {_, Read} = S3:read_object(config:get(aws_bucket), [StorageKey]),
    <<IV:16/binary, CipherText/binary>> = list_to_binary(Read),
    {ok, zlib:gunzip(crypto:aes_ctr_decrypt(DecryptionKey, IV, CipherText))}.

save(StorageKey, EncryptionKey, Data) ->
    S3 = s3init(),
    IV = ivec(),
    CipherText = crypto:aes_ctr_encrypt(EncryptionKey, IV, zlib:gzip(Data)),
    StorageData = <<IV/binary, CipherText/binary>>,
    Md5 = hex:list_to_hex(binary_to_list(crypto:md5(StorageData))),
    {ok, Wrote} = S3:write_object(config:get(aws_bucket),
                                 StorageKey, StorageData, "text/plain"),
    Wrote = "\"" ++ Md5 ++ "\"", % TODO: why is the md5 returned from s3 in qoutes?
    logger:info(["Wrote object: " ++ Wrote]).

s3init() ->
    ibrowse:start(),
    Credentials = #aws_credentials{
      accessKeyId=config:get(aws_key_id),
      secretAccessKey=config:get(aws_secret_key)
     },
    s3:new( Credentials ).

ivec() -> crypto:rand_bytes(16).


test() ->
    Data = <<"dette er jo en test hvor man tester ting.">>,
    Key = crypto:rand_bytes(32),
    save("thisisatest2", Key, Data),
    load("thisisatest2", Key) == Data.
