-module(gen_smtp_conn).
-author('hue.tran').

-behaviour(gen_server).

-include("gen_smtp.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([start_connection/0, pool_name/0, statistic/0]).
-export([send/3, send/2]).


-define(CALLBACK, fun handle_callback:response/1).
-define(COUNT_OPTS, {1, 1, 10, 0}).

-record(state, {sock, ext, host, opts, status, reason}).


start_connection() ->
	gen_smtp_conn_sup:start_connection().

init([Options]) ->	
	RelayDomain = proplists:get_value(relay, Options),
	CallBack = proplists:get_value(smtp_cb, Options, []),
	CB_Mod = proplists:get_value(module, CallBack, handle_callback),
	CB_Fun = proplists:get_value(func, CallBack, response),
	NewOptions = [{callback, fun CB_Mod:CB_Fun/1} | Options],
	MXRecords = case proplists:get_value(no_mx_lookups, NewOptions) of
	true -> [];
	_ -> smtp_util:mxlookup(RelayDomain)
	end,
	Hosts = case MXRecords of
	[] -> [{0, RelayDomain}]; % maybe we're supposed to relay to a host directly
	_ -> MXRecords
	end,
	case try_smtp_sessions(Hosts, NewOptions, []) of 
	State when is_record(State, state) -> 
		{ok, State};
	ErrMsg  -> 
		lager:error("Fail Connection With: ~p",[ErrMsg]),
		{stop, ErrMsg}
	end.

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({From, To, Body}, #state{status = connected, sock = Socket, ext = Extensions, opts = Options} = State) ->
	Callback = proplists:get_value(callback, Options, ?CALLBACK),
	lager:debug("connected: Msg: ~p",[{From, To, Body}]),
	NewState = case send_mail({From, To, Body}, Socket, Extensions) of 
		{errsock, _} = ErrMsg -> 
			lager:info("Perform Reconn and resend; ~p", [ErrMsg]),
			resend_email({From, To, Body}, State, Callback);
		EmailResp -> 
			callback(Callback, EmailResp),
			State
	end,
	{noreply, NewState};


handle_cast({To, Body}, #state{status = connected, sock = Socket, ext = Extensions, opts = Options} = State) ->
	Callback = proplists:get_value(callback, Options, ?CALLBACK),
	From = proplists:get_value(username, Options, ""),
	lager:debug("connected: Msg: ~p",[{From, To, Body}]),
	NewState =  if From /= "" ->
		case send_mail({From, To, Body}, Socket, Extensions) of 
		{errsock, _} = ErrMsg -> 
			lager:info("Perform Reconn and resend: ~p",[ErrMsg]),
			resend_email({From, To, Body}, State, Callback); 
		EmailResp -> 
			callback(Callback, EmailResp),
			State
		end;
	true ->
		callback(Callback, {error, "Require username configure", {From, To, Body}}),
		State
	end,
	{noreply, NewState};

handle_cast({_From, _To, _Body} = Email, #state{status = Status, reason = Reason, opts = Options} = State) ->
	lager:debug("Disconnection: Status: ~p; Msg: ~p",[Status, Email]),
	Callback = proplists:get_value(callback, Options, ?CALLBACK),
	callback(Callback, {error, Reason, Email }),
	{noreply, State};

handle_cast({To, Body}, #state{status = Status, reason = Reason, opts = Options} = State) ->
	Callback = proplists:get_value(callback, Options, ?CALLBACK),
	From = proplists:get_value(username, Options, ""),
	lager:debug("Disconnection: Status: ~p; Msg: ~p",[Status, {From, To, Body}]),
	if From /= "" ->
		callback(Callback, {error, Reason, {From, To, Body}});
	true ->
		callback(Callback,  {error, from, "Require username configure", {From, To, Body}})
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({reconnect, Hosts, Options}, State) ->
	case try_smtp_sessions(Hosts, Options, []) of 
	NewState when is_record(NewState, state) -> 
		{noreply, NewState};
	ErrMsg -> 
		lager:error("Fail Reconnection With: ~p",[ErrMsg]),
		{stop, ErrMsg, State}
	end;

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_NewVs, _OldVs, State) ->
	{noreply, State}.

terminate(Reason, #state{sock = Socket, status = connected} =  _State) ->
	lager:debug("Reason: ~p",[Reason]),
	gen_smtp_conn_util:quit(Socket),
	ok;

terminate(Reason, _State) ->
	lager:debug("Reason: ~p",[Reason]),
	ok.


try_smtp_sessions([{_Distance, Host} | _Tail] = Hosts, Options, RetryList) ->
	case gen_smtp_conn_util:smtp_connect(Host, Options) of 
	{ok, {Socket, Extensions}} ->
		#state{sock = Socket, ext = Extensions, host = Hosts, opts = Options, status = connected};
	ErrMsg ->
		handle_error(ErrMsg, Hosts, Options, RetryList)
	end;
try_smtp_sessions(_Host, _Options, _RetryList) ->
	{error, "Not Support The Relay Format"}.

handle_error({error, {missing_requirement, _Message} = ErrMsg}, _Hosts, _Options, _RetryList) ->
	{error, ErrMsg};
handle_error({error, {permanent_failure, _Message} = ErrMsg}, _Hosts, _Options, _RetryList) ->
	{error, ErrMsg};
handle_error({error, {temporary_failure, tls_failed}}, [{_Distance, Host} | _Tail] = Hosts, Options, RetryList) ->
	% Could not start the TLS handshake; if tls is optional then try without TLS
	case proplists:get_value(tls, Options) of
		if_available ->
			NoTLSOptions = [{tls, never} | proplists:delete(tls, Options)],
			case gen_smtp_conn_util:smtp_connect(Host, NoTLSOptions) of 
			{ok, {Socket, Extensions}} ->
				#state{sock = Socket, ext = Extensions, host = Hosts, opts = Options, status = connected};
			ErrMsg ->
				handle_error(ErrMsg, Hosts, Options, RetryList)
			end;
		_ ->
			try_next_host({error, {temporary_failure, tls_failed}}, Hosts, Options, RetryList)
	end;

handle_error(FailMsg, Hosts, Options, RetryList) ->
	try_next_host(FailMsg, Hosts, Options, RetryList).


try_next_host({_Err, {FailureType, Message}}, [{_Distance, Host} | _Tail] = Hosts, Options, RetryList) ->
	Retries = proplists:get_value(retries, Options, 0),
	RetryCount = proplists:get_value(Host, RetryList),
	case fetch_next_host(Retries, RetryCount, Hosts, RetryList) of
		{[], _NewRetryList} ->
			ReconnTime = proplists:get_value(reconnect, Options, ?RECONNECT),
			case  ReconnTime of  
			Time when is_integer(Time) ->
				lager:error("retries exceeded: ~p. Reconn after ~p",[{retries_exceeded, {FailureType, Host, Message}}, ReconnTime]),
				erlang:send_after(Time, self(), {reconnect, Hosts, Options}),
				#state{status = connecting,  reason = {FailureType, Host, Message}, opts = Options, host = Hosts};
			_ ->
				lager:error("retries exceeded: ~p",[{retries_exceeded, {FailureType, Host, Message}}]),
				{error, {retries_exceeded, FailureType, Host, Message}}
			end;
		{NewHosts, NewRetryList} ->
			try_smtp_sessions(NewHosts, Options, NewRetryList)
	end.

fetch_next_host(Retries, RetryCount, [{_Distance, Host} | Tail], RetryList) when is_integer(RetryCount), RetryCount >= Retries ->
	% out of chances
	%io:format("retries for ~s exceeded (~p of ~p)~n", [Host, RetryCount, Retries]),
	{Tail, lists:keydelete(Host, 1, RetryList)};
fetch_next_host(_Retries, RetryCount, [{Distance, Host} | Tail], RetryList) when is_integer(RetryCount) ->
	%io:format("scheduling ~s for retry (~p of ~p)~n", [Host, RetryCount, Retries]),
	{Tail ++ [{Distance, Host}], lists:keydelete(Host, 1, RetryList) ++ [{Host, RetryCount + 1}]};
fetch_next_host(0, _RetryCount, [{_Distance, Host} | Tail], RetryList) ->
	% done retrying completely
	{Tail, lists:keydelete(Host, 1, RetryList)};
fetch_next_host(_Retries, _RetryCount, [{Distance, Host} | Tail], RetryList) ->
	% otherwise...
	%io:format("scheduling ~s for retry (~p of ~p)~n", [Host, 1, Retries]),
	{Tail ++ [{Distance, Host}], lists:keydelete(Host, 1, RetryList) ++ [{Host, 1}]}.


send(From, To, Body) ->
	wpool:cast(pool_name(), {From, To, Body}).

send(To, Body) ->
	wpool:cast(pool_name(), {To, Body}).


pool_name() ->
	list_to_atom(atom_to_list(?MODULE) ++ "_pool").

statistic() ->
	Get = fun proplists:get_value/2,
	InitStats = wpool:stats(pool_name()),
	PoolPid = Get(supervisor, InitStats),
	Options = Get(options, InitStats),
	InitWorkers = Get(workers, InitStats),
	WorkerStatus = 
	[begin
	    WorkerStats = Get(I, InitWorkers),
	    MsgQueueLen = Get(message_queue_len, WorkerStats),
	    Memory = Get(memory, WorkerStats),
	    {status, WorkerStats, MsgQueueLen, Memory}
   	end || I <- lists:seq(1, length(InitWorkers))],
   	[PoolPid, Options, WorkerStatus].

%% Internal is_function

send_mail({From, To, Body} = Email, Socket, Extensions) ->
	case gen_smtp_conn_util:try_MAIL_FROM(From, Socket, Extensions) of 
	true -> 
		case gen_smtp_conn_util:try_RCPT_TO(To, Socket, Extensions) of 
		[] -> 
			case gen_smtp_conn_util:try_DATA(Body, Socket, Extensions) of 
				{ok, Receipt} -> {ok, Receipt, Email};
				{errsock, _} = ErrSock -> ErrSock; 
				ErrMsg -> {error, ErrMsg, Email}
			end;
		{errsock, _} = ErrSock -> ErrSock;   
		ErrMsg ->  {error, ErrMsg, Email}
		end;
	{errsock, _} = ErrSock -> ErrSock; 
	ErrMsg -> {error, ErrMsg, Email}
	end.

callback(Callback, Reply) ->
	case is_function(Callback) of 
	true -> Callback(Reply);
	_ -> ok
	end.


%% reconnect to socket and send email
resend_email({_From, _To, _Body} = Email, #state{opts = Options, host = [{_Distance, Host} | _Tail] = Hosts, sock = Socket}, Callback) ->
	gen_smtp_conn_util:quit(Socket),
	case gen_smtp_conn_util:smtp_connect(Host, Options) of 
	{ok, {NewSocket, NewExtensions}} ->
		EmailResp = send_mail(Email, NewSocket, NewExtensions),
		Resp = case EmailResp of 
		{errsock, _} = ErrMsg -> {error, ErrMsg, Email};
		Reply -> Reply 
		end,
		callback(Callback, Resp),
		#state{sock = NewSocket, ext = NewExtensions, host = Hosts, opts = Options, status = connected};
	ErrMsg ->
		callback(Callback, {error, ErrMsg, Email}),
		case handle_error(ErrMsg, Hosts, Options, []) of 
		{error, _} = ErrMsg -> 
			#state{status = disconnected, reason = ErrMsg, opts = Options, host = Hosts};
		State -> State 
		end
	end;
resend_email(_Email, State, _Callback) ->
	State.


% ts() ->
% 	{Mega, Sec, Micr} = erlang:timestamp(),
% 	Mega * 1000000 * 1000000 + Sec * 1000000 + Micr.
