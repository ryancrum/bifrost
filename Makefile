all: compile

compile:
	rebar -v compile skip_deps=true

test:
	rebar -v eunit skip_deps=true

clean:
	rebar clean skip_deps=true
