main:	clean
	erlc -o ebin -I include *.erl

clean:
	mkdir -p ebin
	rm -f ebin/*

VERSION=Mailup-0.1

