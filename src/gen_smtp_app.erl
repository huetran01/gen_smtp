-module(gen_smtp_app).
-bahaviour(application).

-export([start/0, start/2, stop/1]).


start() ->
	application:start(gen_smtp).

start(_Type, _Args) ->
	gen_smtp_sup:start_link().

stop(_State) ->
	ok.

