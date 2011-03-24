-module(sh).
-export([run/2, line/1, lines/1, write/2, write_line/2, test/0]).

line(Port, Parts) ->
    receive
        {Port, {data, {eol, Line}}} ->
            {line, string:join(lists:reverse([Line | Parts]), "")};
        {Port, {data, {noeol, Line}}} ->
            line(Port, [Line | Parts]);
        {Port, eof} ->
            [] = Parts,
            eof
    end.

line(Port) ->
    line(Port, "").


lines(Port, Lines) ->
    case line(Port) of
        eof ->
            true = port_close(Port),
            lists:reverse(Lines);
        {line, Line} ->
            lines(Port, [Line | Lines])
    end.

lines(Port) ->
    lines(Port, []).

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
             {line, 4096}, in, out, eof, use_stdio]),
    Port.


test() ->
    % Run executable and get port
    Port = run("imap.py",
               ["--username", "-", "--password", "-", "--headers-only", "--message-id", "4"]),
    % Write to stdin
    write_line(Port, "jmailbackup44@gmail.com"),
    write_line(Port, "qwerty60"),
    % Read output as lines
    line(Port).

