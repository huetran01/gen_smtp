-module(smtp_client_example).

-export([send/3, send/2]).


-spec send(binary(), binary()|[binary()], binary()) -> ok.
send(From, To, Text) -> 
    Subject =  <<"Subject Example">>,
    MailBody = mimemail:encode({<<"text">>, <<"plain">>,
                              [{<<"Subject">>, Subject},
                              {<<"From">>, From},
                              {<<"To">>, To}],
                              [], Text}),
    lager:info("MailBody: ~p ~n",[MailBody]),
    gen_smtp_client:send(From, To, MailBody).
    

-spec send(binary()|[binary()], binary()) -> ok.
send(To, Text) -> 
    Subject =  <<"Subject Example">>,
    MailBody = mimemail:encode({<<"text">>, <<"plain">>,
                              [{<<"Subject">>, Subject},
                              {<<"from">>, <<>>},
                              {<<"To">>, To}],
                              [], Text}),
    lager:info("MailBody: ~p ~n",[MailBody]),
    gen_smtp_client:send(To, MailBody).