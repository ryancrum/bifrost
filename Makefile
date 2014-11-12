all: compile

compile:
	rebar -v compile

test:
	rebar -v eunit

clean:
	rebar clean
