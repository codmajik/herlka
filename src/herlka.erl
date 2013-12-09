-module(herlka).

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the shawarma app for inclusion in a supervisor tree
start_link() -> ok.

%% @spec start() -> ok
%% @doc Start the shawarma server.
start() ->
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
    { error, {already_started, App} } -> ok;
    {error, {not_started, Dep}} -> 
      ok = ensure_started(Dep),
      ensure_started(App);
    _E -> _E
  end.