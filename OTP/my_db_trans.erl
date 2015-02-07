-module(my_db_trans).
-export([start/0, stop/0, create/0, write/2, delete/1, read/1, match/1, lock/0, unlock/0]).

% Karl-Johan Drougge, 5.3

% Register the process to my_db and call the create function.
% ?MODULE is a macro that returns the current modules name.
start() -> 
	register(my_db, spawn(?MODULE, create, [])),
	ok.

% End the process and destroy db.
stop() -> 
	my_db ! stop,
	ok.

% Initiate the db with an empty tuple.
% And set process flag to true to trap exit 
% if a process this process is linked to
% dies.
create() ->
	process_flag(trap_exit, true),
	loop(free, dbBinary:new()),
	ok.

write(Key, Element) -> 
	handler(write, {Key, Element}).

delete(Key) -> 
	handler(delete, Key).

read(Key) -> 
	handler(read, Key).

match(Element) -> 
	handler(match, Element).

lock() ->
	handler(lock, {just_nothing_just_need_to_send_something_to_have_it_work}).   

unlock() ->
    handler(unlock, {just_nothing_just_need_to_send_something_to_have_it_work}).


% A generic function to handle write/delete/read/match.
handler(Command, Parameter) ->
	% Send a message to my_db which is the registered 
	% process that has the db.
	my_db ! {self(), Command, Parameter},
	receive Reply -> 
		Reply
	end.

% The server that is listening for messages and when received one
% pattern match and see which one the user wants.
% With every pattern matched with write/delete/read/match 
% call itself with the newly created db and start all over.
loop(free, Db) ->
	receive
		stop -> 
			dbBinary:destroy(Db);
		{Pid, write, {Key, Element}} ->
			NewDb = dbBinary:write(Key, Element, Db),
			Pid ! ok,
			loop(free, NewDb);
		{Pid, delete, Key} ->
			NewDb = dbBinary:delete(Key, Db),
			Pid ! ok,
			loop(free, NewDb);
		{Pid, read, Key} ->
			Entry = dbBinary:read(Key, Db),
			Pid ! Entry,
			loop(free, Db);
		{Pid, match, Element} ->
			Entries = dbBinary:match(Element, Db),
			Pid ! Entries,
			loop(free, Db);
		{Pid, lock, _} ->
			% Link the incoming process to db process
			link(Pid),
			Pid ! ok,
			loop(busy, Pid, Db);
		{Pid, _, _} ->
			Pid ! {error,instance}
	end.


% The busy loop when a process has linked and locked
% itself with this process.
loop(busy, Pid, Db) ->
	receive
		% If the process is destroyed we want
		% send the db process back to free.
		{'EXIT', Pid, Reason} ->
			io:format("~p died. Reason: ~p~n", [Pid, Reason]),
			loop(free, Db);
		stop -> 
			dbBinary:destroy(Db);
		{Pid, write, {Key, Element}} ->
			NewDb = dbBinary:write(Key, Element, Db),
			Pid ! ok,
			loop(busy, Pid, NewDb);
		{Pid, delete, Key} ->
			NewDb = dbBinary:delete(Key, Db),
			Pid ! ok,
			loop(busy, Pid, NewDb);
		{Pid, read, Key} ->
			Entry = dbBinary:read(Key, Db),
			Pid ! Entry,
			loop(busy, Pid, Db);
		{Pid, match, Element} ->
			Entries = dbBinary:match(Element, Db),
			Pid ! Entries,
			loop(busy, Pid, Db);
		{Pid, unlock, _} ->
			% Unlink the incoming process.
			unlink(Pid),
			Pid ! ok,
			loop(free, Db);
		{Pid, _, _} ->
			Pid ! {error,instance}
	end.