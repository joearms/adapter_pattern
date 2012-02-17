.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $< 

server1: beam
	(sleep 1 && ./openchrome http://localhost:1234/json_round_trip.ehe) &
	erl -noshell -s server start misultin_adapter

server2: beam
	(sleep 1 && ./openchrome http://localhost:1234/json_round_trip.ehe) &
	erl -noshell -s server start cowboy_adapter

server3: beam
	(sleep 1 && ./openchrome http://localhost:1234/json_round_trip.ehe) &
	erl -noshell -s server start mochiweb_adapter

beam: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam *~ erl_crash.dump 







