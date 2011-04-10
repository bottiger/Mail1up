-module(mailget).
-compile(export_all).
-export([check/0, check/1, check/2, check/3]).

list_folders(User, Pass) ->
    % ImapFolders is a list of Binary objects like:
    % <<"(\\HasNoChildren) \"/\" \"Personal\"">>
    {ok, ImapFolders} = imappy(User, Pass, ["--list-folders"]),
    FolderString = binary:bin_to_list(lists:nth(1,ImapFolders)).


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


