
%% herlka message

-record(herlka_msg, {
    type :: string(),
    timestamp :: number(),
    pid :: number(),
    severity ::	number(),
    payload :: string(),
    logger	::	string(),
    fields :: list({binary(), binary()})
}).