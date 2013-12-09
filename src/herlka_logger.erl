
-module(herlka_logger).


-author("codmajik").

-behaviour(gen_server).

-export([
  start_link/1,
  start_link/2,

  init/1
  ,handle_call/3
  ,handle_cast/2
  ,handle_info/2
  ,terminate/2
  ,code_change/3
]).


-export([send/2]).


-record(state, {
  host,
  port,
  identity,
  hmac_key,
  hmac_key_ver,
  hmac_signer,
  hmac_func,
  sock,
  type,
  fields
}).


-include("../include/herlka.hrl").
-include("../include/herlka_message_pb.hrl").


start_link(Params) ->
	gen_server:start_link(?MODULE, Params, []).

start_link(Name, Params) ->
	gen_server:start_link({local,Name}, ?MODULE, Params, []).


init([{_,_}|_] = Props) ->
  T = proplists:get_value(transport, Props),
  {ok,Sock} = case T of
  		tcp ->
			gen_tcp:connect(
			    proplists:get_value(host, Props, ""),
			    proplists:get_value(port, Props, 0),
			    [binary, {send_timeout, 5000}, {active, false}, {packet, raw}]
			);

		udp -> gen_udp:open(0)
	end,

{ok,  #state{
    hmac_key        = proplists:get_value(hmac_key, Props),
    hmac_signer     = proplists:get_value(hmac_signer, Props),
    hmac_key_ver    = proplists:get_value(hmac_key_ver, Props),
    hmac_func       = proplists:get_value(hmac_func, Props, md5),
    host            = proplists:get_value(host, Props, "127.0.0.1"),
    port            = proplists:get_value(port, Props, 0),
    sock            = Sock,
    type            = T,
    fields 			= [ #field{ name = herlka_utils:any_to_binary(N), value_string = herlka_utils:any_to_binary(V) } || 
        	{N,V} <- proplists:get_value(extra_fields, Props, [])]
}}.

handle_call({log, #herlka_msg{} = Msg}, _From, S) ->
  {reply, process_event(Msg, S), S};

handle_call(_R, _From, State) ->
  {noreply, State}.

%% @private
handle_cast({log, #herlka_msg{} = Msg}, S) ->  
  process_event(Msg, S),
  {noreply, S};

handle_cast(_Event, State) ->
  {ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, S) when S#state.type == udp ->
  catch gen_udp:close(S#state.sock),
  ok;

terminate(_Reason, S) when S#state.type == tcp ->
  catch gen_tcp:close(S#state.sock),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% public interface function
send(Server, #herlka_msg{} = H) when is_atom(Server); is_pid(Server) ->
	gen_server:cast(Server, {log, H}).



%% internal functions
process_event(#herlka_msg{} = Log_Msg, #state{type=udp}=S) ->
  ok = gen_udp:send(S#state.sock, S#state.host, S#state.port, compose_packet(Log_Msg, S));

process_event(#herlka_msg{} = Log_Msg, #state{type=tcp}=S) ->
  ok = gen_tcp:send(S#state.sock, compose_packet(Log_Msg, S)).

compose_packet(#herlka_msg{ type = Type, timestamp = Ts} = Log_Msg, #state{fields=F}=S) ->
  
  Msg = herlka_message_pb:encode(#message {
    uuid = herlka_utils:uuid_v4(),
    type = herlka_utils:any_to_binary(Type),
    timestamp = Ts,
    hostname = herlka_utils:any_to_binary(node()),
    pid = Log_Msg#herlka_msg.pid,
    severity = Log_Msg#herlka_msg.severity,
    payload = herlka_utils:any_to_binary(Log_Msg#herlka_msg.payload),
    logger = herlka_utils:any_to_binary(Log_Msg#herlka_msg.logger),

    fields = F ++ [ 
    	#field { 
    		name = herlka_utils:any_to_binary(FieldName), 
    		value_string = herlka_utils:any_to_binary(FieldValue) 
    	} || {FieldName,FieldValue} <- Log_Msg#herlka_msg.fields
    ]
  }),


  Hdr = herlka_message_pb:encode(if
    is_atom(S#state.hmac_signer);
    is_atom(S#state.hmac_key);
    is_atom(S#state.hmac_key_ver) ->
      #header{message_length= iolist_size(Msg)};
    true ->
      #header{
        message_length= iolist_size(Msg),
        hmac_hash_function= list_to_existing_atom(string:to_upper(atom_to_list(S#state.hmac_func))),
        hmac_signer= S#state.hmac_signer,
        hmac_key_version= S#state.hmac_key_ver,
        hmac= crypto:hmac_final(crypto:hmac_update(crypto:hmac_init(S#state.hmac_func, S#state.hmac_key), Msg))
      }
      end),

  [<<16#1e:8/unsigned-integer, (iolist_size(Hdr)):8/unsigned-integer>>, Hdr, <<16#1f:8/unsigned-integer>>, Msg].

