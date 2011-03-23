-module(sh).
-export([run/2, lines/1, write/2, write_line/2, test/0]).

lines(Port, ParLine, Lines) ->
    receive
        {Port, {data, {eol, Line}}} ->
            NewLines =
                case Lines of
                    [] -> [ParLine ++ Line];
                    Ls -> [ParLine ++ Line | Ls]
                end,
            lines(Port, "", NewLines);
        {Port, {data, {noeol, Line}}} ->
            lines(Port, ParLine ++ Line, Lines);
        {Port, eof} ->
            "" = ParLine,
            true = port_close(Port),
            lists:reverse(Lines)
    end.

lines(Port) ->
    lines(Port, "", []).

write(Port, Data) ->
    true = port_command(Port, Data).

write_line(Port, Line) ->
    write(Port, Line ++ "\n").

run(Exec, Args) ->
    ExecPath = case os:find_executable(Exec) of
                   false -> Exec; % Assume same directory
                   Path -> Path
               end,
    Port = open_port(
            {spawn_executable, ExecPath},
            [{args, Args},
             {line, 4096}, in, out, eof, hide]),
    Port.


test() ->
    % Run executable and get port
    Port = run("imap.py",
               ["--username", "-", "--password", "-", "--headers-only", "--message-id", "4"]),
    % Write to stdin
    write_line(Port, "jmailbackup44@gmail.com"),
    write_line(Port, "qwerty60"),
    % Read output as lines
    lines(Port).

