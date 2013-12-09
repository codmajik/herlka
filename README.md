
Overview
---------

herlka (he[rlang]ka) is a [lager](https://github.com/basho/lager) backend for [Mozilla Heka](https://github.com/mozilla-services/heka).


It will sends all log entries (over tcp/udp) to hekad for further processing; It also send gathered metadata as
fields to allow easier toml and lua-sandbox filtering.


Usage
-------
Include this backend into your project's rebar config:

    {herlka, ".*", {git, "https://github.com/codmajik/herlka.git", "master"}}


Configuration
--------------
  Configure heka loggers like so:

    {herlka, [
            {loggers,  [
                {critical_errors_to_heka, [
                    {transport, TRANSPORT_TYPE},
                    {host, IP_OR_HOSTNAME},
                    {port, PORT},
                    {hmac_func, HMAC_FUNC},
                    {hmac_key, HMAC_KEY},
                    {hmac_signer, SIGNER_NAME},
                    {hmac_key_ver, SIGNER_VERSION}
                ]}
            ]}
        ]
    }

Where:

* critical_errors_to_heka is the name to be associated with this heka logger process (change as neccessary)


* TRANSPORT_TYPE - atom udp or tcp specifing the kind of transport we are connecting to
* IP_OR_HOSTNAME - a binary string representing the hosname or ip of the server hosting heka deamon
* PORT - integer representing transport tcp/udp port specified for specific transport (heka can listen alot ports)
* HMAC_FUNC - atom md5 (default) or sha representing for signing function should be used
* HMAC_KEY - binary string representing the hmac key with which all paylod should be signed
* SIGNER_NAME - binary string representing the hmac key name as specified in the heka toml config
* SIGNER_VERSION - integer respresenting hmac key name version as specified in the heka toml config

    Note: HMAC_KEY and SIGNER_NAME and SIGNER_VERSION must all be set else the message would be sent unsigned


To forward log messages to heka a Lager handler would have to be configured like the following:

  {lager, [
      {handlers, [
        {herlka_lager_backend, [
          {logger, critical_errors_to_heka},
          {level, critical},
          {identity, <<"my_super_app">>}
        ]}
      ]}
    ]}

* logger    -   atom representing process name of the heka logger messages should be forwarded to
* level     -   lowerest lager level at which messages should be forwarded to heka logger (see lager log levels)
* identity  -   an arbituary string appending to the heka packet as Type 


