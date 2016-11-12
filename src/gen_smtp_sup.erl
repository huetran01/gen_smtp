-module(gen_smtp_sup).
-author('hue.tran').

-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Child = {gen_smtp_conn_sup, {gen_smtp_conn_sup, start_link, []},
		temporary, 5000, supervisor, [gen_smtp_conn_sup]},
	{ok, {{one_for_one, 5, 10}, [Child]}}.

