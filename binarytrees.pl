% --------------------------------- %
% --------------  2  -------------- %
% --------------------------------- %
% -------------- 2.a -------------- %

isBinaryTree(leaf).
isBinaryTree(node(X, Y)) :- 
	isBinaryTree(X),
	isBinaryTree(Y).

% -------------- 2.b -------------- %

nnodes(leaf, N) :- 
	N = 1.
nnodes(node(X, Y), N) :-
	nnodes(X, N1),
	nnodes(Y, N2),
	N is 1 + N1 + N2.
	
% -------------- 2.c -------------- %

isBinaryTree2c(leaf(_)).
isBinaryTree2c(node(X, Y, _)) :- 
	isBinaryTree2c(X),
	isBinaryTree2c(Y).

nnodes2c(leaf(_), N) :- 
	N = 1.
nnodes2c(node(X, Y, _), N) :-
	nnodes2c(X, N1),
	nnodes2c(Y, N2),
	N is 1 + N1 + N2.


% -------------- 2.d -------------- %

makeBinary(N, Tree) :- 
	N == 0, !,
	Tree = leaf(0).
makeBinary(N, Tree) :- 
	NewN is N-1,
	makeBinary(NewN, R1),
	Tree = node(R1, R1, N).
  
% -------------- 2.e -------------- %

repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).

makeTree(N, _, Tree) :- 
	N = 0, !,
	Tree = leaf(0).
makeTree(N, NumberOfChildren, Tree) :-
	NewN is N-1,
	makeTree(NewN, NumberOfChildren, R1),
	repl(R1, NumberOfChildren, R2),
	Tree = node(R2, N).

% --------------------------------- %
% --------------  3  -------------- %
% --------------------------------- %
% -------------- 3.a -------------- %
	
member(X, [X|_]).
member(X, [_|Y]) :- 
	member(X, Y).

% -------------- 3.b -------------- %

subset(_, []).
subset(A, [B1|B2]) :- 
	member(B1, A),
	subset(A, B2).

% -------------- 3.c -------------- %

path(From, From, _, Visited, Path) :-
	append(Visited, [From], Path).
path(From, To, Graph, Visited, Path) :-
	(member(edge(From, Z), Graph) ; member(edge(Z, From), Graph)),
	\+ member(Z, Visited),
	append(Visited, [From], A2),
	path(Z, To, Graph, A2, Path).

% -------------- 3.d -------------- %

filterListLenghts([X|_], Length, Result) :-
	length(X, L),
	L == Length,
	Result = X.
filterListLenghts([_|X], Length, Result) :-
	filterListLenghts(X, Length, Result).
	
longest_paths(From, To, Graph, Paths) :- 
	findall(Res, path(From, To, Graph, [], Res), AllPaths),
	maplist(length, AllPaths, Lengths),
	max_member(MaxLength, Lengths),
	findall(Res, filterListLenghts(AllPaths, MaxLength, Res), Paths).
