-module(herlka_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok,Pid} = herlka_sup:start_link(),
    case application:get_env(herlka, loggers) of
    	{ok, [_|_]=H} when is_list(H) -> _ = [herlka_sup:start_logger(N,P) || {N,[{_,_}|_]=P} <- H];
        _ -> ok
    end,
    {ok,Pid}.

stop(_State) ->
    ok.
