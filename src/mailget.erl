-module(mailget).
-compile(export_all).
-export([check/0, check/1, check/2, check/3]).

-spec list_folders(string(), string()) -> list(string()).
%% @doc Imap returns folders like this: [<"(\\HasNoChildren) \"/\" \"Personal\"">>, ...]
%% list_folders parses this into a list of strings. ["Personal",...]
%% @end
list_folders(User, Pass) ->
    % ImapFolders is a list of Binary objects like:
    {ok, ImapFolders} = imappy(User, Pass, ["--list-folders"]),
    lists:map(fun(X) -> lists:last(string:tokens(binary:bin_to_list(X), "\"")) end, ImapFolders).

%% @doc Returns a dict with Key => mail-id and Value = MailContent
%% @end
get_mails(User, Pass) ->
    {ok, Mails} = imappy(User, Pass),
    parse_mail_list(Mails).

%% @doc Returns a dict with Key => mail-id and Value = Headers
%% @end
get_headers(User, Pass) ->
    {ok, Headers} = imappy(User, Pass, ["--headers-only"]),
    parse_mail_list(Headers).

%% @doc Returns a dict with Key => mail-id and Value => (A dict where Key => Header and Value => Header Value)
%% For instance: Key => "Date" and Value => "Fri, 18 Mar 2011 08:49:18 -0700"
%% @end
get_headers_dict(User, Pass) ->
    HeaderDict = get_headers(User, Pass),
    dict:fold(fun(K, V, AccIn) -> dict:store(K, header_to_dict(V), AccIn) end, dict:new(), HeaderDict).

%-spec mail_id(string(), string(), integer()) ->
mail_id(User, Pass, Id) ->
    mail_id(User, Pass, Id, "INBOX").

%-spec mail_id(string(), string(), integer(), string()) -> list(string()).
mail_id(User, Pass, Id, Folder) ->
    {ok, MailDict} = imappy(User, Pass, ["--message-id", integer_to_list(Id), "--folder", Folder]),
    parse_mail_dict(MailDict).

%% @doc Return the raw header of the mail with id=ID
%% @end
mail_header_id(User, Pass, Id) ->
    {ok, MailDict} = imappy(User, Pass, ["--message-id", integer_to_list(Id), "--headers-only"]),
    parse_mail_dict(MailDict).

%% @doc Returns the e-mail headers of the mail with Id as a Dict
%% @end
mail_header_id_dict(User, Pass, Id) ->
    MailDict = mail_header_id(User, Pass, Id),
    HeaderString = dict:fetch("content", MailDict),
    header_to_dict(HeaderString).

%-spec parse_mail_dict(dict()) -> dict().
%% @doc Converts MailDict from a Dict of <<"binaries">> to utf-8">> into a dict of "utf8 strings"
%% @end
parse_mail_dict(MailDict) ->
    dict:fold(fun(K, V, AccIn) -> dict:store(unicode:characters_to_list(K), unicode:characters_to_list(V), AccIn) end, dict:new(), MailDict).

%% @doc Transform a list of E-mails returned from imappy into a dict where Key = mail-id and Value = MailContent 
%% @end
parse_mail_list(MailList) ->
    lists:foldl(fun(M, AccIn) ->
        ParsedMail = parse_mail_dict(M),
        Key = list_to_integer(dict:fetch("mail-id", ParsedMail)),
        Value = dict:fetch("content", ParsedMail),
        dict:store(Key, Value, AccIn)
        end, dict:new(), MailList).

%% @doc Transform the raw headers into a dict where Key => Header and Value => Headers Value
%% For instance: Key => "Date" and Value => "Fri, 18 Mar 2011 08:49:18 -0700"
%% @end
header_to_dict(Header) ->
    HeaderList = string:tokens(Header, "\r\n"), % Gmail uses \r\n instead of \n. What should we split at?
    dict:from_list(lists:map(fun(X) ->
                Parts = string:tokens(X,":"),
                Key = lists:nth(1, Parts),
                Value = string:strip(string:join(lists:nthtail(1, Parts), ":")),
                {Key, Value}
              end, HeaderList)).

-spec check() -> term().
check() -> check("").
check(MsgID) -> check("jmailbackup44@gmail.com", "qwerty60", MsgID).
check(User, Pass) -> check(User, Pass, "").

check(User, Pass, MsgID) when is_integer(MsgID) ->
    check(User, Pass, ["--message-id", integer_to_list(MsgID)]);

check(User, Pass, MsgArgs) ->
    imappy(User, Pass, MsgArgs).

imappy(User, Pass) ->
    imappy(User, Pass, "").

-spec imappy(list(), list(), list()) -> list().
imappy(User, Pass, MsgArgs) ->
    % Run python wrapper
    PortArgs = ["--username", "-", "--password", "-"] ++ MsgArgs,
    logger:notice(PortArgs),
    Port = sh:run("priv/imap.py", PortArgs),
    % Write user and pass
    sh:write_line(Port, User),
    sh:write_line(Port, Pass),
    % Fetch all output lines
    Lines = sh:lines(Port),
    % First line is JSON
    MailOut = lists:nth(1, Lines),
    json:json_to_term(MailOut).


