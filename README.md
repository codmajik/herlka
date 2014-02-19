Overview
---------

herlka (he[rlang]ka) is a client for [Mozilla Heka](https://github.com/mozilla-services/heka) with [lager](https://github.com/basho/lager) support.


It will sends all log entries (over tcp/udp) to hekad for further processing, with support for heka fields.

For [lager](https://github.com/basho/lager), it also send gathered metadata as fields to allow easier toml and lua-sandbox filtering.


Usage
-------
Include this backend into your project's rebar config:

    {herlka, ".*", {git, "https://github.com/codmajik/herlka.git", "master"}}


Configuration
--------------
To start helka logger add the follow to your sys.config file

    {herlka, [
            {loggers,  [
                {name_of_logger_process, [
                    {transport, upd | tcp},
                    {host, inet:hostname() | inet:ipaddress()},
                    {port, inet:port()},
                    {cache_proto_buffer_to_dets, atom()},
                    {hmac_func, md5 | sha},
                    {hmac_key, null | binary()},
                    {hmac_signer, null | binary()},
                    {hmac_key_ver, null | integer()}
                ]}
            ]}
        ]
    }

###Notes:
* name_of_logger_process is the name registered for hekad logger process
* messages would only be signed when hmac_key, hmac_signer, hmac_key_ver are all not set to null [atom()]


###Make Lager forward log messages to herlka logger
___

    {lager, [
      {handlers, [
        {herlka_lager_backend, [
          {logger, atom()},
          {level, lager:log_level()},
          {identity, string()}
        ]}
      ]}
    ]}


####Todo
_______

* support tls
* support unix domain socket transport (use afunix - though erlangers usually don't like nif drivers)


####Contact
twitter: @codmajik
email: codmajik@gmail.com




