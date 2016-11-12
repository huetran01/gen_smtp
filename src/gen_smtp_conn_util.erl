-module(gen_smtp_conn_util).

-export([smtp_connect/2,
	check_options/1]).

-export([try_MAIL_FROM/3,
	try_RCPT_TO/3,
	try_DATA/3,
	quit/1]).

%-define(TIMEOUT, 1200000).
-define(TIMEOUT, 180000). %% 3m

-define(AUTH_PREFERENCE, [
		"CRAM-MD5",
		"LOGIN",
		"PLAIN"
	]).


smtp_connect(Host, Options) ->
	case do_smtp_session(Host, Options) of 
	{ok, _Msg} = Res -> Res  ;
	ErrMsg -> 
		lager:error("Smtp Session Fail ~p ; Reason: ~p",[Host, ErrMsg]),
		{error, ErrMsg}
	end.

-spec do_smtp_session(Host :: string(), Options :: list()) -> binary().
do_smtp_session(Host, Options) ->
	case connect(Host, Options) of 
	{ok, Socket, _Host, _Banner} -> 
		case try_EHLO(Socket, Options) of 
		{ok, Extensions}  -> 	
			case try_STARTTLS(Socket, Options, Extensions) of 
			{Socket2, Extensions2}  ->
				Authed = try_AUTH(Socket2, Options, proplists:get_value(<<"AUTH">>, Extensions2)),
				if Authed == false ; Authed == true -> {ok, {Socket2, Extensions2}};
				true -> Authed 
				end;
			Err -> Err 
			end;
		Err ->
			Err
		end;
	Err ->	
		Err
	end. 

connect(Host, Options) when is_binary(Host) ->
	connect(binary_to_list(Host), Options);
connect(Host, Options) ->
	AddSockOpts = case proplists:get_value(sockopts, Options) of
		undefined -> [];
		Other -> Other
	end,
    SockOpts = [binary, {packet, line}, {keepalive, true}, {active, false} | AddSockOpts],
	Proto = case proplists:get_value(ssl, Options) of
		true ->
			ssl;
		_ ->
			tcp
	end,
	Port = case proplists:get_value(port, Options) of
		undefined when Proto =:= ssl ->
			465;
		OPort when is_integer(OPort) ->
			OPort;
		_ ->
			25
	end,
	case socket:connect(Proto, Host, Port, SockOpts, 5000) of
		{ok, Socket} ->
			case read_possible_multiline_reply(Socket) of
				{ok, <<"220", Banner/binary>>} ->
					{ok, Socket, Host, Banner};
				{ok, <<"4", _Rest/binary>> = Msg} ->
					quit(Socket),
					{temporary_failure, Msg};
				{ok, Msg} ->
					quit(Socket),
					{permanent_failure, Msg};
				Err ->
					quit(Socket),
					Err 
			end;
		{error, Reason} ->
			{network_failure, Reason}
	end.


%% read a multiline reply (eg. EHLO reply)
-spec read_possible_multiline_reply(Socket :: socket:socket()) -> {ok, binary()}.
read_possible_multiline_reply(Socket) ->
	case socket:recv(Socket, 0, ?TIMEOUT) of
		{ok, Packet} ->
			case binstr:substr(Packet, 4, 1) of
				<<"-">> ->
					Code = binstr:substr(Packet, 1, 3),
					read_multiline_reply(Socket, Code, [Packet]);
				<<" ">> ->
					{ok, Packet}
			end;
		{error, closed} = Err -> {errsock, Err };
		{error, timeout} = Err -> {errsock, Err};
		Error -> {network_failure, Error}
	end.


-spec read_multiline_reply(Socket :: socket:socket(), Code :: binary(), Acc :: [binary()]) -> {ok, binary()}.
read_multiline_reply(Socket, Code, Acc) ->
	case socket:recv(Socket, 0, ?TIMEOUT) of
		{ok, Packet} ->
			case {binstr:substr(Packet, 1, 3), binstr:substr(Packet, 4, 1)} of
				{Code, <<" ">>} ->
					{ok, list_to_binary(lists:reverse([Packet | Acc]))};
				{Code, <<"-">>} ->
					read_multiline_reply(Socket, Code, [Packet | Acc]);
				_ ->
					{unexpected_response, lists:reverse([Packet | Acc])}
			end;
		{error, closed} = Err -> {errsock, Err};
		{error, timeout} = Err -> {errsock, Err};
		Error ->
			{network_failure, Error}
	end.

quit(Socket) ->
	socket:send(Socket, "QUIT\r\n"),
	socket:close(Socket),
	ok.


-spec try_EHLO(Socket :: socket:socket(), Options :: list()) -> {ok, list()}.
try_EHLO(Socket, Options) ->
	ok = socket:send(Socket, ["EHLO ", proplists:get_value(hostname, Options, smtp_util:guess_FQDN()), "\r\n"]),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"500", _Rest/binary>>} ->
			% Unrecognized command, fall back to HELO
			try_HELO(Socket, Options);
		{ok, <<"4", _Rest/binary>> = Msg} ->
			quit(Socket),
			{temporary_failure, Msg};
			%throw({temporary_failure, Msg});
		{ok, Reply} ->
			{ok, parse_extensions(Reply)};
		Err ->
			quit(Socket),
			Err
	end.

-spec try_HELO(Socket :: socket:socket(), Options :: list()) -> {ok, list()}.
try_HELO(Socket, Options) ->
	ok = socket:send(Socket, ["HELO ", proplists:get_value(hostname, Options, smtp_util:guess_FQDN()), "\r\n"]),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"250", _Rest/binary>>} ->
			{ok, []};
		{ok, <<"4", _Rest/binary>> = Msg} ->
			quit(Socket),
			{temporary_failure, Msg};
			%throw({temporary_failure, Msg});
		{ok, Msg} ->
			quit(Socket),
			{permanent_failure, Msg};
			% throw({permanent_failure, Msg})
		Err -> 
			quit(Socket),
			Err
	end.

% check if we should try to do TLS
-spec try_STARTTLS(Socket :: socket:socket(), Options :: list(), Extensions :: list()) -> {socket:socket(), list()}.
try_STARTTLS(Socket, Options, Extensions) ->
	case {proplists:get_value(tls, Options),
			proplists:get_value(<<"STARTTLS">>, Extensions)} of
		{Atom, true} when Atom =:= always; Atom =:= if_available ->
			%io:format("Starting TLS~n"),
			case {do_STARTTLS(Socket, Options), Atom} of
				{false, always} ->
					quit(Socket),
					{temporary_failure, tls_failed};
				{false, if_available} ->
					{Socket, Extensions};
				{{S, E}, _} ->
					{S, E};
				{Err, _} ->
					Err 
			end;
		{always, _} ->
			quit(Socket),
			{missing_requirement, tls};
		_ ->
			{Socket, Extensions}
	end.

%% attempt to upgrade socket to TLS
-spec do_STARTTLS(Socket :: socket:socket(), Options :: list()) -> {socket:socket(), list()} | false.
do_STARTTLS(Socket, Options) ->
	socket:send(Socket, "STARTTLS\r\n"),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"220", _Rest/binary>>} ->
			case catch socket:to_ssl_client(Socket, [], 5000) of
				{ok, NewSocket} ->
					%NewSocket;
					{ok, Extensions} = try_EHLO(NewSocket, Options),
					{NewSocket, Extensions};
				{'EXIT', Reason} ->
					quit(Socket),
					lager:error("Error in ssl upgrade: ~p.~n", [Reason]),
					{temporary_failure, tls_failed};
				{error, ssl_not_started} ->
					quit(Socket),
					{permanent_failure, ssl_not_started};
				_Else ->
					%io:format("~p~n", [Else]),
					false
			end;
		{ok, <<"4", _Rest/binary>> = Msg} ->
			quit(Socket),
			{temporary_failure, Msg};
		{ok, Msg} ->
			quit(Socket),
			{permanent_failure, Msg};
		Err ->
			quit(Socket), 
			Err 
	end.

-spec try_AUTH(Socket :: socket:socket(), Options :: list(), AuthTypes :: [string()]) -> boolean().
try_AUTH(Socket, Options, []) ->
	case proplists:get_value(auth, Options) of
		always ->
			quit(Socket),
			{missing_requirement, auth};
		_ ->
			false
	end;
try_AUTH(Socket, Options, undefined) ->
	case proplists:get_value(auth, Options) of
		always ->
			quit(Socket),
			{missing_requirement, auth};
		_ ->
			false
	end;
try_AUTH(Socket, Options, AuthTypes) ->
	case proplists:is_defined(username, Options) and
		proplists:is_defined(password, Options) and
		(proplists:get_value(auth, Options) =/= never) of
		false ->
			case proplists:get_value(auth, Options) of
				always ->
					quit(Socket),
					{missing_requirement, auth};
				_ ->
					false
			end;
		true ->
			Username = to_string(proplists:get_value(username, Options)),
			Password = to_string(proplists:get_value(password, Options)),
			Types = re:split(AuthTypes, " ", [{return, list}, trim]),
			case do_AUTH(Socket, Username, Password, Types) of
				false ->
					{permanent_failure, auth_failed};
				true ->
					true;
				Err -> 
					quit(Socket),
					Err
			end
	end.


to_string(String) when is_list(String)   -> String;
to_string(Binary) when is_binary(Binary) -> binary_to_list(Binary).


-spec do_AUTH(Socket :: socket:socket(), Username :: string(), Password :: string(), Types :: [string()]) -> boolean().
do_AUTH(Socket, Username, Password, Types) ->
	FixedTypes = [string:to_upper(X) || X <- Types],
	%io:format("Fixed types: ~p~n", [FixedTypes]),
	AllowedTypes = [X  || X <- ?AUTH_PREFERENCE, lists:member(X, FixedTypes)],
	%io:format("available authentication types, in order of preference: ~p~n",
	%	[AllowedTypes]),
	do_AUTH_each(Socket, Username, Password, AllowedTypes).

-spec do_AUTH_each(Socket :: socket:socket(), Username :: string() | binary(), Password :: string() | binary(), AuthTypes :: [string()]) -> boolean().
do_AUTH_each(_Socket, _Username, _Password, []) ->
	false;
do_AUTH_each(Socket, Username, Password, ["CRAM-MD5" | Tail]) ->
	socket:send(Socket, "AUTH CRAM-MD5\r\n"),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"334 ", Rest/binary>>} ->
			Seed64 = binstr:strip(binstr:strip(Rest, right, $\n), right, $\r),
			Seed = base64:decode_to_string(Seed64),
			Digest = smtp_util:compute_cram_digest(Password, Seed),
			String = base64:encode(list_to_binary([Username, " ", Digest])),
			socket:send(Socket, [String, "\r\n"]),
			case read_possible_multiline_reply(Socket) of
				{ok, <<"235", _Rest/binary>>} ->
					lager:debug("CRAM-MD5: authentication accepted"),
					true;
				{ok, _Msg} ->
					lager:debug("CRAM-MD5: authentication rejected"),
					do_AUTH_each(Socket, Username, Password, Tail);
				Err -> Err 
			end;
		{ok, _Something} ->
			lager:debug("CRAM-MD5: authentication Something: ~p",[_Something]),
			do_AUTH_each(Socket, Username, Password, Tail);
		Err -> Err 
	end;
do_AUTH_each(Socket, Username, Password, ["LOGIN" | Tail]) ->
	socket:send(Socket, "AUTH LOGIN\r\n"),
	case read_possible_multiline_reply(Socket) of
		%% base64 Username: or username:
		{ok, Prompt} when Prompt == <<"334 VXNlcm5hbWU6\r\n">>; Prompt == <<"334 dXNlcm5hbWU6\r\n">> ->
			%io:format("username prompt~n"),
			U = base64:encode(Username),
			socket:send(Socket, [U,"\r\n"]),
			case read_possible_multiline_reply(Socket) of
				%% base64 Password: or password:
				{ok, Prompt2} when Prompt2 == <<"334 UGFzc3dvcmQ6\r\n">>; Prompt2 == <<"334 cGFzc3dvcmQ6\r\n">> ->
					%io:format("password prompt~n"),
					P = base64:encode(Password),
					socket:send(Socket, [P,"\r\n"]),
					case read_possible_multiline_reply(Socket) of
						{ok, <<"235 ", _Rest/binary>>} ->
							lager:debug("LOGIN: authentication accepted",[]),
							true;
						{ok, _Msg} ->
							lager:debug("LOGIN: password rejected: ~s",[_Msg]),
							do_AUTH_each(Socket, Username, Password, Tail);
						Err -> Err 
					end;
				{ok, _Msg2} ->
					lager:debug("LOGIN: Username rejected: ~p",[_Msg2]),
					do_AUTH_each(Socket, Username, Password, Tail);
				Err -> Err 
			end;
		{ok, _Something} ->
			%io:format("got ~s~n", [Something]),
			lager:debug("LOGIN: authentication Something: ~p",[_Something]),
			do_AUTH_each(Socket, Username, Password, Tail);
		Err -> Err 
	end;
do_AUTH_each(Socket, Username, Password, ["PLAIN" | Tail]) ->
	lager:info("PLAIN"),
	AuthString = base64:encode("\0"++Username++"\0"++Password),
	socket:send(Socket, ["AUTH PLAIN ", AuthString, "\r\n"]),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"235", _Rest/binary>>} ->
			%io:format("authentication accepted~n"),
			lager:debug("PLAIN: authentication accepted",[]),
			true;
		_Else ->
			% TODO do we need to bother trying the multi-step PLAIN?
			%io:format("authentication rejected~n"),
			%io:format("~p~n", [Else]),
			lager:debug("PLAIN: authentication Something ~p",[_Else]),
			do_AUTH_each(Socket, Username, Password, Tail)
	end;
do_AUTH_each(Socket, Username, Password, [_Type | Tail]) ->
	%io:format("unsupported AUTH type ~s~n", [Type]),
	do_AUTH_each(Socket, Username, Password, Tail).


-spec parse_extensions(Reply :: binary()) -> [{binary(), binary()}].
parse_extensions(Reply) ->
	[_ | Reply2] = re:split(Reply, "\r\n", [{return, binary}, trim]),
	[
		begin
				Body = binstr:substr(Entry, 5),
				case re:split(Body, " ",  [{return, binary}, trim, {parts, 2}]) of
					[Verb, Parameters] ->
						{binstr:to_upper(Verb), Parameters};
					[Body] ->
						case binstr:strchr(Body, $=) of
							0 ->
								{binstr:to_upper(Body), true};
							_ ->
								%io:format("discarding option ~p~n", [Body]),
								[]
						end
				end
		end  || Entry <- Reply2].

check_options(Options) ->
	case proplists:get_value(relay, Options) of
	undefined ->
		{error, no_relay};
	_ ->
		case proplists:get_value(auth, Options) of
			Atom when Atom =:= always ->
				case proplists:is_defined(username, Options) and
					proplists:is_defined(password, Options) of
					false ->
						{error, no_credentials};
					true ->
						ok
				end;
			_ ->
				ok
		end
	end.

%%SMTP Error Reference 
%% https://support.google.com/a/answer/3726730?hl=en

-spec try_MAIL_FROM(From :: string() | binary(), Socket :: socket:socket(), Extensions :: list()) -> true.
try_MAIL_FROM(From, Socket, Extensions) when is_binary(From) ->
	try_MAIL_FROM(binary_to_list(From), Socket, Extensions);
try_MAIL_FROM("<" ++ _ = From, Socket, _Extensions) ->
	% TODO do we need to bother with SIZE?
	socket:send(Socket, ["MAIL FROM: ", From, "\r\n"]),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"250", _Rest/binary>>} ->
			true;
		{ok, <<"451 4.4.2 Timeout", _Rest/binary>> = Msg} ->
			{errsock, Msg};
		{ok, <<"4", _Rest/binary>> = Msg} ->
			{temporary_failure, Msg};
		{ok, Msg} ->
			{permanent_failure, Msg};
		Err ->
			Err 
	end;
try_MAIL_FROM(From, Socket, Extensions) ->
	% someone was bad and didn't put in the angle brackets
	try_MAIL_FROM("<"++From++">", Socket, Extensions).

-spec try_RCPT_TO(Tos :: [binary() | string()], Socket :: socket:socket(), Extensions :: list()) -> true.

try_RCPT_TO(ListTo, Socket, Extensions) -> try_RCPT_TO(ListTo, Socket, Extensions, []).
try_RCPT_TO([], _Socket, _Extensions, Acc) ->
	Acc;
try_RCPT_TO([To | Tail], Socket, Extensions, Acc) when is_binary(To) ->
	try_RCPT_TO([binary_to_list(To) | Tail], Socket, Extensions, Acc);
try_RCPT_TO(["<" ++ _ = To | Tail], Socket, Extensions, Acc) ->
	socket:send(Socket, ["RCPT TO: ",To,"\r\n"]),
	NewAcc = case read_possible_multiline_reply(Socket) of
		{ok, <<"250", _Rest/binary>>} -> 
			Acc;
			%try_RCPT_TO(Tail, Socket, Extensions);
		{ok, <<"251", _Rest/binary>>} -> 
			Acc;
			% try_RCPT_TO(Tail, Socket, Extensions);
		{ok, <<"451 4.4.2 Timeout", _Rest/binary>> = Msg} ->
			{errsock, Msg};
		{ok, <<"4", _Rest/binary>> = Msg} ->
			[{temporary_failure, Msg} | Acc];
		{ok, Msg} ->
			[{permanent_failure, Msg} | Acc];
		{errsock, _} = Err -> Err;
		Err -> 
			[Err | Acc]
	end,
	case NewAcc of 
	{errsock, _} = ErrSock -> ErrSock;
	_ -> try_RCPT_TO(Tail, Socket, Extensions, NewAcc)
	end;
try_RCPT_TO([To | Tail], Socket, Extensions, Acc) ->
	% someone was bad and didn't put in the angle brackets
	try_RCPT_TO(["<"++To++">" | Tail], Socket, Extensions, Acc).

-spec try_DATA(Body :: binary() | function(), Socket :: socket:socket(), Extensions :: list()) -> binary().
try_DATA(Body, Socket, Extensions) when is_function(Body) ->
    try_DATA(Body(), Socket, Extensions);
try_DATA(Body, Socket, _Extensions) ->
	socket:send(Socket, "DATA\r\n"),
	case read_possible_multiline_reply(Socket) of
		{ok, <<"354", _Rest/binary>>} ->
			%% Escape period at start of line (rfc5321 4.5.2)
			EscapedBody = re:replace(Body, <<"^\\\.">>, <<"..">>, [global, multiline, {return, binary}]),
			socket:send(Socket, [EscapedBody, "\r\n.\r\n"]),
			case read_possible_multiline_reply(Socket) of
				{ok, <<"250 ", Receipt/binary>>} ->
					{ok, Receipt};
				{ok, <<"451 4.4.2 Timeout", _Rest/binary>> = Msg} ->
					{errsock, Msg};
				{ok, <<"4", _Rest2/binary>> = Msg} ->
					{temporary_failure, Msg};
				{ok, Msg} ->
					{permanent_failure, Msg};
				Err -> Err 
		 	end;
		 {ok, <<"451 4.4.2 Timeout", _Rest/binary>> = Msg} ->
			{errsock, Msg};
		 {ok, <<"4", _Rest/binary>> = Msg} ->		 	
		 	{temporary_failure, Msg};
		 {ok, Msg} ->
		 	{permanent_failure, Msg};
		 Err -> Err 
	end.