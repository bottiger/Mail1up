-module(logger).
-export([info/1, debug/1, notice/1, log/2]).

info(Message) -> log("Info", Message).
debug(Message) -> log("Debug", Message).
notice(Message) -> log("Notice", Message).

log(Prefix, Message) -> case config:get(debug) of
                    true -> io:format("~s: ~s~n", [Prefix, Message] );
                    false -> false
                  end.
