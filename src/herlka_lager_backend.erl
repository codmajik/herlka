
-module(herlka_lager_backend).

-author("codmajik").

-behaviour(gen_event).

-export([
  init/1
  ,handle_call/2
  ,handle_event/2
  ,handle_info/2
  ,terminate/2
  ,code_change/3
]).

-record(state, {
  logger,
  level,
  identity
}).

-include("../include/herlka.hrl").


init({Level, Identity, Logger}) -> init([{level, Level}, {identity, Identity}, {logger, Logger}]);

init([{_,_}|_]=P) ->
  {ok, #state{
    logger= proplists:get_value(logger,P), 
    level= lager_util:level_to_num(proplists:get_value(level,P)), 
    identity= proplists:get_value(identity,P)
  }}.

handle_call(get_loglevel, #state{ level = Level } = State) ->
  {ok, Level, State};

handle_call({set_loglevel, Level}, State) ->
  {ok, ok, State#state{ level = lager_util:level_to_num(Level) }};

handle_call(_Request, State) ->
  {ok, ok, State}.

%% @private
handle_event({log, Log_Msg}, #state{logger=L, identity=I, level=Lvl}=S) ->
  case lager_util:is_loggable(Log_Msg, Lvl, lager_hekad) of
    true ->
    	{ok,DefApp} = application:get_application(),
    	Prop = lager_msg:metadata(Log_Msg),

    	LMsg = #herlka_msg {
    		type= <<"lager:hekad:", (herlka_utils:any_to_binary(I))/binary >>,
    		timestamp = herlka_utils:extract_epoch(lager_msg:timestamp(Log_Msg)),
    		pid = herlka_utils:pid_to_int(proplists:get_value(pid, Prop, "<0.0.0>")),
    		severity = lager_msg:severity_as_int(Log_Msg),
    		payload = lager_msg:message(Log_Msg),
    		logger = proplists:get_value(application, Prop, DefApp),
    		fields = [ {<<"level">>, lager_msg:severity(Log_Msg)} | Prop]
    	},

    	herlka_logger:send(L, LMsg);
    false -> ok
  end,
  {ok, S};

handle_event(_Event, State) ->
  {ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_R, _S) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

