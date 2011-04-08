main: compile

compile:
	./rebar compile

rel: compile
	(rm -rf rel/mail1up && ./rebar generate)

console: rel
	rel/mail1up/bin/mynode console

deps:
	./rebar get-deps

clean:
	./rebar clean

VERSION=Mailup-0.1
