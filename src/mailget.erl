-module(mailget).
-compile(export_all).
-export([check/0, check/1, check/2, check/3]).

-spec list_folders(string(), string()) -> list(string()).
list_folders(User, Pass) ->
    % ImapFolders is a list of Binary objects like:
    % <<"(\\HasNoChildren) \"/\" \"Personal\"">>
    {ok, ImapFolders} = imappy(User, Pass, ["--list-folders"]),
    lists:map(fun(X) -> lists:last(string:tokens(binary:bin_to_list(X), "\"")) end, ImapFolders).
    
%-spec mail_id(string(), string(), integer()) -> list(string()). 
mail_id(User, Pass, Id) ->
    mail_id(User, Pass, Id, "INBOX").

%-spec mail_id(string(), string(), integer(), string()) -> list(string()).
mail_id(User, Pass, Id, Folder) ->
    {ok, MailDict} = imappy(User, Pass, ["--message-id", integer_to_list(Id), "--folder", Folder]),
    %string:tokens(binary:bin_to_list(Mail), "\r\n").
    parse_mail_dict(MailDict).

mail_header_id(User, Pass, Id) ->
    {ok, MailDict} = imappy(User, Pass, ["--message-id", integer_to_list(Id), "--headers-only"]),
    parse_mail_dict(MailDict).

%% @doc Returns the e-mail headers of the mail with Id as a Dict
%% @end
mail_header_id_dict(User, Pass, Id) ->
    MailDict = mail_header_id(User, Pass, Id),
    HeaderString = dict:fetch("content", MailDict),
    HeaderList = string:tokens(HeaderString, "\r\n"), % Gmail uses \r\n instead of \n. What should we split at?
    dict:from_list(lists:map(fun(X) -> 
                Parts = string:tokens(X,":"),
                Key = lists:nth(1, Parts),
                Value = string:strip(string:join(lists:nthtail(1, Parts), ":")),
                {Key, Value}
              end, HeaderList)).

%-spec parse_mail_dict(dict()) -> dict().
parse_mail_dict(MailDict) ->
    % Converts MailDict from a Dict of <<"binaries">> th utf-8">> into a dict of "utf8 strings"
    dict:fold(fun(K, V, AccIn) -> dict:store(unicode:characters_to_list(K), unicode:characters_to_list(V), AccIn) end, dict:new(), MailDict).



-spec check() -> term().
check() -> check("").
check(MsgID) -> check("jmailbackup44@gmail.com", "qwerty60", MsgID).
check(User, Pass) -> check(User, Pass, "").

check(User, Pass, MsgID) when is_integer(MsgID) ->
    check(User, Pass, ["--message-id", integer_to_list(MsgID)]);

check(User, Pass, MsgArgs) ->
    imappy(User, Pass, MsgArgs).

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


