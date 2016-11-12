-module(gen_smtp_conn_sup).
-author('hue.tran').

-include("gen_smtp.hrl").

-export([start_link/0, init/1, start_connection/0]).

-define(Handler, gen_smtp_conn).



start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 5, 10}, []}}.

start_connection() ->
	PoolSize = application:get_env(gen_smtp, poolsize, 10),
	Options = application:get_env(gen_smtp, opts, []),
	NewOptions = lists:ukeymerge(1, lists:sort(Options), lists:sort(?DEFAULT_OPTIONS)),
	lager:info("NewOptions: ~p",[NewOptions]),
	case gen_smtp_conn_util:check_options(NewOptions) of
	ok ->  
		ConnectionWorkerOPts = [{workers, PoolSize},
								{worker, { ?Handler, [NewOptions]}}],
		Child = {gen_smtp_conn_sup_child, {wpool, start_pool, [?Handler:pool_name(), ConnectionWorkerOPts]},
					'temporary', 10000, 'supervisor', [wpool]},	
		supervisor:start_child(?MODULE, Child);
	{error, no_relay} ->
		lager:error("Error: Must enable relay at option configure to enable smtp. 
			Otherwise, you can not send email via this app."),
		ignore;
	{error, no_credentials} ->
		lager:error("Error: Must enable username/password to enable smtp.
			Otherwise, you can not send email via this app."),
		ignore
	end. 

