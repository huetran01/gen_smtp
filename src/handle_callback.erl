-module(handle_callback).

-export([response/1]).


response({error, ErrMsg, EmailInfo}) ->
	lager:error("From: Err Report: ~p for email: ~p", [ErrMsg, EmailInfo]);

response({ok, Receipt, EmailInfo}) ->
	lager:info("Receipt: ~p for email: ~p",[Receipt, EmailInfo]);

response(Msg) ->
	lager:info("Other Msg: ~p",[Msg]).


