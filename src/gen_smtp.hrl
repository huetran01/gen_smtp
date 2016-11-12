
-define(DEFAULT_OPTIONS, [
		{ssl, false}, % whether to connect on 465 in ssl mode
		{tls, if_available}, % always, never, if_available
		{auth, if_available},
		{hostname, smtp_util:guess_FQDN()},
		{retries, 0} % how many retries per smtp host on temporary failure
	]).


%% Store email when disconnect smtp and resend later
-define(TAB, email_queue). 

%% Max queue of email will be stored when disconnect to smtp server and then resend 
-define(MAXQUEUE, 10). 

-define(RECONNECT, false). %% default no reconnect
