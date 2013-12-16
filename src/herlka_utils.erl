-module(herlka_utils).

-compile(export_all).

-define(UUID_FMT, "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b").
-define(UUID_CRAND(E), (crypto:rand_uniform(1, round(math:pow(2, E))) - 1):E).

any_to_binary(V) when is_pid(V) -> any_to_binary(erlang:pid_to_list(V));
any_to_binary(V) when is_integer(V)   -> any_to_binary(integer_to_list(V));
any_to_binary(V) when is_atom(V)   -> any_to_binary(atom_to_list(V));
any_to_binary(V) when is_list(V)   -> list_to_binary(V);
any_to_binary(V) when is_binary(V) -> V.

pid_to_int(P) when is_atom(P) -> 
  case whereis(P) of 
    Pid when is_pid(Pid) -> pid_to_int(Pid);
    _ -> 0
  end;
  
pid_to_int(P) when is_list(P) ->
  [Node, Proc, S] = string:tokens(P,".<>"),
  NodeInt = list_to_integer(Node),
  ProcInt = list_to_integer(Proc),
  SInt = list_to_integer(S),

  <<ID:32/integer>> = <<NodeInt:8/integer, ProcInt:16/integer, SInt:8/integer>>,
  ID;

pid_to_int(P) when is_binary(P) -> pid_to_int(binary_to_list(P));
pid_to_int(P) when is_pid(P) -> pid_to_int(erlang:pid_to_list(P)).

extract_epoch({_Date, _Time}) ->
  [Y,M,D] = string:tokens(lists:flatten(_Date), "-"),

  [H, Mm, Sec | _] = string:tokens(lists:flatten(_Time), ": "),

  {FSec, ESec} = case string:tokens(Sec, ".") of
   [] -> {0, 0};
   [Sec] -> {list_to_integer(Sec), 0};
   [Sec1,MSec] -> {list_to_integer(Sec1), list_to_integer(MSec)}
  end,

  (date_time_to_nano_second_epoch({
    {list_to_integer(Y),list_to_integer(M),list_to_integer(D)},
    {list_to_integer(H),list_to_integer(Mm),FSec}
  }) + ESec) * 1000000;

extract_epoch({_,_,_}=Now) -> 
	date_time_to_nano_second_epoch(calendar:now_to_datetime(Now)) * 1000000.

date_time_to_nano_second_epoch({{_,_,_},{_,_,_}}=DT) ->
	(calendar:datetime_to_gregorian_seconds(DT) - 62167219200) * 1000.



uuid_v4() -> << ?UUID_CRAND(48), 4:4,?UUID_CRAND(12),2:2,?UUID_CRAND(32),?UUID_CRAND(30)>>.

