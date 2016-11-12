%%% Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimer.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%%% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc A simple SMTP client used for sending mail - assumes relaying via a
%% smarthost.

-module(gen_smtp_client).

-export([
		start_connection/0,
		send/3, send/2,
		pool_statistic/0, 
		pool_name/0]).

-spec start_connection() -> ok.
start_connection() ->
	gen_smtp_conn:start_connection().

-spec send(string() | binary(), [string() | binary(), ...], string() | binary() | function()) -> ok.
send(From, [_H | _] = LTo, Body) ->
	gen_smtp_conn:send(From, LTo, Body);
send(From, To, Body) when is_binary(To) orelse is_list(To) ->
	gen_smtp_conn:send(From, [To], Body).

send([_H | _] = LTo, Body) ->
	gen_smtp_conn:send(LTo, Body);
send(To, Body) when is_binary(To) orelse is_list(To) ->
	gen_smtp_conn:send([To], Body).

pool_statistic() ->
	gen_smtp_conn:statistic().

pool_name() ->
	gen_smtp_conn:pool_name().


