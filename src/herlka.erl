-module(herlka).

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the shawarma app for inclusion in a supervisor tree
start_link() -> ok.

%% @spec start() -> ok
%% @doc Start the shawarma server.
start() ->
  application:set_env(herlka, loggers, [
      {rlog, [{transport,udp},
        {host,"localhost"},
        {port,5565},
        {hmac_key, <<"xdd908lfcgikauexdi8elogusridaxoalf">>},
        {hmac_signer, <<"ops">>},
        {hmac_key_ver, 1}]},

      {rlog2, [{transport,udp},
        {host,"localhost"},
        {port,5565},
        {hmac_key, <<"haeoufyaiofeugdsnzaogpi.ua,dp.804u">>},
        {hmac_signer, <<"dev">>},
        {hmac_key_ver, 1}]}
    ]),
  ensure_started(?MODULE).


%% @spec stop() -> ok
%% @doc Stop the shawarma server.
stop() ->
  application:stop(?MODULE).

%% ===================================================================
%% Internal Functions
%% ===================================================================

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    { error, { already_started, App } } -> ok;
    {error, {not_started, Dep}} -> 
      ok = ensure_started(Dep),
      ensure_started(App);
    _Error ->
      io:format("startup error: ~p~n", [_Error]),
      erlang:halt()
  end.
