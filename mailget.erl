-module(mailget).
-export([check/0, check/1, check/2, check/3]).

check() -> check("").
check(MsgID) -> check("jmailbackup44@gmail.com", "qwerty60", MsgID).
check(User, Pass) -> check(User, Pass, "").

check(User, Pass, MsgID) when is_integer(MsgID) ->
    check(User, Pass, ["--message-id", integer_to_list(MsgID)]);

check(User, Pass, MsgArgs) ->
    code:add_path("erlang-json-eep-parser"),
    % Run python wrapper
    Port = sh:run("imap.py", ["--username", "-", "--password", "-"] ++ MsgArgs),
    % Write user and pass
    sh:write_line(Port, User),
    sh:write_line(Port, Pass),
    % Fetch all output lines
    Lines = sh:lines(Port),
    % First line is JSON
    MailOut = lists:nth(1, Lines),
    json_eep:json_to_term(MailOut).


