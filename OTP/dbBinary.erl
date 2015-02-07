-module(dbBinary).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> {'nil'}.
destroy(_) -> ok.

%% If we reach the bottom/end of the "tuple-list".
write(Key, Value, {'nil'}) ->
		{Key, Value, {'nil'}, {'nil'}};
%% If NewKey is less than Key, recursive through LeftBranch to find the right place to write content.
write(NewKey, NewValue, {Key, Value, LeftBranch, RightBranch}) when NewKey < Key ->
		{Key, Value, write(NewKey, NewValue, LeftBranch), RightBranch};
%% If NewKey is greater than Key, recursive through RightBranch to find the right place to write content.
write(NewKey, NewValue, {Key, Value, LeftBranch, RightBranch}) when NewKey > Key ->
		{Key, Value, LeftBranch, write(NewKey, NewValue, RightBranch)};
%% If Key already exist, return the same "tuple-list". 
write(Key, Value, {Key, _, LeftBranch, RightBranch}) ->
		{Key, Value, LeftBranch, RightBranch}.

delete(_, {'nil'}) -> {error,instance};
delete(_, {_, _, {'nil'},{'nil'}}) -> {'nil'};
delete(_, {_, _, LeftBranch,{'nil'}}) -> LeftBranch;
delete(_, {_, _, {'nil'},RightBranch}) -> RightBranch;
% Look if we should go left or right, depending on the Key.
delete(DelKey, {Key, Value, LeftBranch, RightBranch}) when DelKey < Key ->
	{Key, Value, delete(DelKey, LeftBranch), RightBranch};
delete(DelKey, {Key, Value, LeftBranch, RightBranch}) when DelKey > Key ->
	{Key, Value, LeftBranch, delete(DelKey, RightBranch)};
% If we find a match call orginazie to reorganize without the one Value we don't want.
delete(Key, {Key, _, LeftBranch, RightBranch}) -> 
	{Key2, Value2, LeftBranch2, _RightBranch} = organize(LeftBranch),
	{Key2, Value2, LeftBranch2, RightBranch}.
%Reorganize the tree after we delete an item.
organize({Key, Value, {'nil'}, {'nil'}}) ->
	{Key, Value, {'nil'}, {'nil'}};
organize({Key, Value, LeftBranch, {'nil'}}) ->
	{Key, Value, LeftBranch, {'nil'}};
organize({Key, Value, LeftBranch, RightBranch}) ->
	{Key2, Value2, LeftBranch2, RightBranch2} = organize(RightBranch),
	{Key2, Value2, {Key, Value, LeftBranch, RightBranch2}, {Key, Value, LeftBranch2, {'nil'}}}.

%% If we reach the bottom/end of the "tuple-lust" then not found, return error.
read(_, {'nil'}) -> {error, instance};
%% We found the Key, return the value
read(Key, {Key, Value, _, _}) ->
	{ok, Value};
%% If the Key are less than NodeKey we need to search the left branch. 
read(Key, {NodeKey, _, LeftBranch, _}) when Key < NodeKey ->
	read(Key, LeftBranch);
%% If its not the left branch then it might be in the right branch
read(Key, {_, _, _, RightBranch}) ->
	read(Key, RightBranch).

% Need a helper match-method so we can slip in a list to save all the value-matches.
match(Value, Db) -> match(Value, Db, []).

% If we find a match we still need to search deeper in the hiarchy, both left and right.
% Put the Key in the head and then search left and right branch and append the results from
% them.
match(Value, {Key, Value, LeftBranch, RightBranch}, Result) ->
	[Key | match(Value, LeftBranch, match(Value, RightBranch, Result))];

% if no match keep searching
match(Value, {_, _, LeftBranch, RightBranch}, Result) ->
	match(Value, RightBranch, match(Value, LeftBranch, Result));
%if we find a nil-node we send back the Result.
match(_, {'nil'}, Result) ->
	Result.




