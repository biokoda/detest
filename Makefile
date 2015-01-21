all:
	./rebar get-deps
	./rebar compile
	./rebar escriptize

clean:
	./rebar clean

