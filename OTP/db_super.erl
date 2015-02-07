-module(db_super).
-behaviour(supervisor).

%% 8.3

%% API
-export([start_link/0, start/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
	{Answer, _} = start_link(),
	Answer.

%%--------------------------------------------------------------------
init([]) ->
	Server = {db_gen_server_trans, {db_gen_server_trans, start_link, []}, permanent, 30000, worker, [db_gen_server_trans]},
    Children = [Server],
    RestartStrategy = {one_for_one, 5,3600},
    {ok,{RestartStrategy, Children}}.