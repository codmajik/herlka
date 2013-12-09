
-module(herlka_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_logger/1, start_logger/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
	{ok,{{simple_one_for_one, 5, 10},[?CHILD(herlka_logger,worker)]}}.


start_logger(Name, [{_,_}|_]=Params) when is_atom(Name) ->
  supervisor:start_child(?MODULE, [Name, Params]).

start_logger([{_,_}|_]=Params) ->
  supervisor:start_child(?MODULE, [Params]).


