all:
	./rebar get-deps
	./rebar compile
	 ./rebar escriptize
	#erl -pa ebin -noinput -eval "detest:ez(),init:stop()"

clean:
	./rebar clean

