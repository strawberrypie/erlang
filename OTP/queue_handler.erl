-module(queue_handler).
-export([start/0, init/0, stop/0, lock/0, unlock/0]).

start() -> 
	register(handler, spawn(?MODULE, init, [])).

init() -> 
	process_flag(trap_exit, true), 
	loop(free, no_owner, []).

stop() -> 
	handler ! {self(), kill}, 
	ok.

lock() -> 
	handler ! {self(), lock},
	receive  % blocking receive
		Data -> Data 
	end.

unlock() -> 
	handler ! {self(), unlock}, 
	receive  % blocking receive
		Data -> Data 
	end.

%% The free-loop
loop(free, Owner, Queue) ->
	receive
		{Pid, lock} ->
			catch link(Pid), % Link to trap exit messages
			Pid ! ok, 
			loop(busy, Pid, Queue);
		{_, kill} ->
			exit(normal);
		_ ->
			loop(free, Owner, Queue) 
	end;

%% The busy-loop.
loop(busy, Owner, Queue) ->
	receive
		{Owner, unlock} ->
			unlink(Owner), % Unlink the Owner to the server.
			Owner ! ok,
			case length(Queue) of
				0 -> % If Queues length is 0, go back to the 
					loop(free, 0, Queue);
				_ ->
					hd(Queue) ! ok,
					loop(busy, hd(Queue), tl(Queue))
			end;
		{Pid, lock} ->
			case lists:member(Pid, Queue) of
				true -> Pid ! {error, already_in_queue},
					loop(busy, Owner, Queue);
				false -> catch link(Pid),
					loop(busy, Owner, lists:reverse([Pid | lists:reverse(Queue)])) % Add to end of Queue
			end;
		{'EXIT', Owner, _Reason} ->
			case length(Queue) of
				0 ->
					loop(free, 0, Queue);
				_ -> 
					hd(Queue) ! ok, % hd() = returns the Head from the list.
					loop(busy, hd(Queue), tl(Queue)) % tl() = returns the Tail from the list
			end;
		{'EXIT', Pid, _Reason} ->
			case lists:member(Pid, Queue) of % Check if The incoming pid is a member of Queue-list
				true -> 
					loop(busy, Owner, [Q || Q <- Queue, Q /= Pid]); % Iterate through Queue where Q is not equal to Pid.
				false ->
					loop(busy, Owner, Queue)
			end;

		{_Pid, kill} ->
			exit(normal);
		_ -> 
			loop(busy, Owner, Queue)
	end.

