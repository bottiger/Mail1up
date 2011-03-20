-module(mailget).
-export([check/0, check/1, check/2, check/3]).

check() -> check("").
check(MsgID) -> check("jmailbackup44@gmail.com", "qwerty60", MsgID).
check(User, Pass) -> check(User, Pass, "").

check(User, Pass, MsgID) when is_integer(MsgID) -> check(User, Pass, " --msg-id " ++ integer_to_list(MsgID));
check(User, Pass, MsgString) ->
    code:add_path("erlang-json-eep-parser"),
    MailOut = os:cmd("./imap.py --username " ++ User ++ " --password " ++ Pass ++ MsgString),
    %%io:fwrite(MailOut).
    json_eep:term_to_json(MailOut).


