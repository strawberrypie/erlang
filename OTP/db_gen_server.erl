-module(db_gen_server).

%% Karl-Johan Drougge. 8.1

%% API
-export([start/0, start_link/0]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([write/2, read/1, match/1, delete/1, stop/0]).

-behavior(gen_server).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() -> 
	{Answer, _Pid} = start_link(),
	Answer.

stop() ->
	gen_server:cast(?MODULE, stop).

init([]) ->
	Db = dbBinary:new(),
	{ok, Db}.

handle_call({write, Key, Element}, _, Db) ->
	{reply, ok, dbBinary:write(Key, Element, Db)};

handle_call({read, Key}, _, Db) ->
	{reply, dbBinary:read(Key, Db), Db};

handle_call({match, Element}, _, Db) ->
	{reply, dbBinary:match(Element, Db), Db};

handle_call({delete, Key}, _, Db) ->
	{reply, ok, dbBinary:delete(Key, Db), Db}.


handle_cast(stop, Db) -> {stop, normal, Db};
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({'EXIT', Pid, Reason}, State) ->
	io:format("~p dieded: ~p, ~p~n", [Pid, Reason, State]),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% Client helper functions
write(Key, Element) -> gen_server:call(?MODULE, {write, Key, Element}).
read(Key) -> gen_server:call(?MODULE, {read, Key}).
delete(Key) -> gen_server:call(?MODULE, {delete, Key}).
match(Element) -> gen_server:call(?MODULE, {match, Element}).