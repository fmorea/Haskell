-module(es9).
-compile(export_all).

%the expressions are evaluated in the order in witch they arrive, and the last is the returned one
add(A, B) -> A + B.

factorial(0) -> 1;
factorial(N) -> 
 N*factorial(N - 1).

fact_tail(N, Acc) when N =< 0 ->
 Acc;
fact_tail(N, Acc) ->
 fact_tail(N-1, Acc*N).
 
 % 22> es9:greet(male, "Steve").
% Hello, Mr Steve
% ok

 greet(male, Name) ->
  io:format("Hello, Mr ~s~n", [Name]);
 greet(female, Name) ->
  io:format("Hello, Ms ~s~n",[Name]);
 greet(_, Name) -> 
  io:format("Hello, ~s~n", [Name]).
 
 % es9:cadr([1, 2, 3, 4]).
 % 2
 car([X | _]) -> X.
 cdr([_ | Tail]) -> Tail.
 cadr([_, Y | _]) -> Y.
 
 % es9:map(fun(X) -> X+1 end, [1,2,3,4]).
 % [2, 3, 4, 5]
 map(_, []) -> [];
 map(F, [X | Xs]) -> [F(X) | map(F, Xs)].
 
 foldr(_, Acc, []) -> Acc;
 foldr(F, Acc, [X | Xs]) -> F(X, foldr(F, Acc, Xs)).
 
 % es9:foldl(fun es9:add/2, 0, [1,2,3,4]).
 % 10
 foldl(_, Acc, []) -> Acc;
 foldl(F, Acc, [X | Xs]) -> foldl(F, F(Acc, X), Xs).
 
 % es9:filter(fun(X) -> X < 5 end, [1, 34, 3, 67, 8]).
 % [1,3]
 filter(_, []) -> [];
 filter(F, [X | Xs]) -> 
  case F(X) of
   true -> [X | filter(F, Xs)];
   _ -> filter(F, Xs) end.
 
 % es9:merge([1, 3, 5],[2, 7]).
 % [1, 2, 3, 5, 7]
 merge(Xs, []) -> Xs;
 merge([], Ys) -> Ys;
 merge(Left =[X|Xs], Right = [Y | Ys]) ->
  if
   X =< Y -> [X | merge(Xs, Right)];
   true -> [Y | merge(Left, Ys)] end.
   
merge_sort(L) -> 
 spawn(?MODULE, ms_split, [self(), anything, L]),
 receive
  {_, LSorted} -> LSorted 
  end.
  78
ms_split(Parent, Side, []) -> Parent ! {Side, []};
ms_split(Parent, Side, [X]) -> Parent ! {Side, [X]};
ms_split(Parent, Side, L) -> 
 {Ll, Lr} = lists:split(lists:lenght(L) div 2, L),
 spawn(?MODULE, ms_split, [self(), left, Ll]),
 spawn(?MODULE, ms_split, [self(), right, Lr]),
 ms_merge(Parent, Side, empty, empty).

ms_merge(Parent, Side, Ll, Lr) when (Ll == empty) or (Lr == empty) ->
 receive
  {left, LlSorted} ->
   ms_merge(Parent, Side, LlSorted, Lr);
  {right, LrSorted} ->
   ms_merge(Parent, Side, Ll, LrSorted) 
   end;
ms_merge(Parent, Side, Ll, Lr) ->
 Parent ! {Side, merge(Ll, Lr)}.
   
 

