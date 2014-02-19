-module(herlka).

-export([start/0, start_link/0, stop/0]).

-export([log/2, log/4, log/5, log/6, log/7, log/8]).

-export ([dump_cache_to_file/2, clear_protobuf_cache/1]).

-include("include/herlka.hrl").



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


clear_protobuf_cache(Logger) ->
  herlka_logger:clear_cache(Logger).

dump_cache_to_file(Logger, FilePath) ->
  herlka_logger:dump_to_raw(Logger, FilePath).

-spec log(atom() | pid(), string(), integer(), string()) -> ok.
log(Logger, Type, Severity, LogMsg) ->
  log(Logger, Type, Severity, LogMsg, []).

-spec log(atom() | pid(), string(), integer(), string(), 
  list({string(),string()})) -> ok.
log(Logger, Type, Severity, LogMsg, Metadata) ->
  log(Logger, Type, Severity, LogMsg, Metadata, erlang:now()).

-spec log(atom() | pid(), string(), integer(), string(), 
  list({string(),string()}), calender:datetime() | erlang:timestamp()) -> ok.
log(Logger, Type, Severity, LogMsg, Metadata, Timestamp) ->
  log(Logger, Type, Severity, LogMsg, Metadata, Timestamp,  self()).

-spec log(atom() | pid(), string(), integer(), string(), 
  list({string(),string()}), calender:datetime() | erlang:timestamp(), pid()) -> ok.
log(Logger, Type, Severity, LogMsg, Metadata, Timestamp,  SenderPid) ->
  {ok,DefApp} = application:get_application(),
  log(Logger, Type, Severity, LogMsg, Metadata, Timestamp,  SenderPid, DefApp).

-spec log(atom() | pid(), string(), integer(), string(), 
  list({string(),string()}), calender:datetime() | erlang:timestamp(), 
    pid(), atom())  -> ok.
log(Logger, Type, Severity, LogMsg, Metadata, Timestamp,  SenderPid,  App) ->
  log(Logger, #herlka_msg {
        type = Type,
        timestamp = herlka_utils:extract_epoch(Timestamp),
        pid = herlka_utils:pid_to_int(SenderPid),
        severity = Severity,
        payload = LogMsg,
        logger = App,
        fields = Metadata
      }).

-spec log(atom() | pid(), #herlka_msg{}) -> ok.
log(Logger, #herlka_msg {  type = T,  timestamp=Ts,  pid=P,  
                severity=S,  payload=M, logger=A,  fields=[]  } = HM )  
  when  (is_atom(Logger) orelse is_pid(Logger)), is_binary(T), is_integer(Ts), 
    is_pid(P), is_integer(S), is_binary(M), is_atom(A)  
    ->  herlka_logger:send(Logger, HM);


log(Logger, #herlka_msg {  type = T,  timestamp=Ts,  pid=P,  
                severity=S,  payload=M, logger=A, fields=[{_,_}|_] } = HM )  
  when  (is_atom(Logger) orelse is_pid(Logger)), is_binary(T), is_integer(Ts), 
    is_pid(P), is_integer(S), is_binary(M), is_atom(A)   ->  
    herlka_logger:send(Logger, HM).



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