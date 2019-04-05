%% Consult the assignment.
:- [assignment4].

/* ---------------------------------------------------------
Tests for Question 1
--------------------------------------------------------- */
:- begin_tests(q1).
test('fourSquares 1') :- 
	findall(Var,fourSquares(20, Var), L),
    permutation(L,[[0, 0, 2, 4],[1, 1, 3, 3]]), !.
:- end_tests(q1).


:- begin_tests(q2).
test('fourSquares 2') :- 
	findall(Var,fourSquares(131, Var), L),
    permutation(L,[[0, 1, 3, 11],[0, 1, 7, 9],[0, 5, 5, 9],[3, 3, 7, 8],[3, 4, 5, 9]]), !.
:- end_tests(q1).


:- begin_tests(q3).
test('fourSquares 3', all(Var == [[0, 0, 0, 1]])) :-
    fourSquares(1, Var).
:- end_tests(q3).

/* To assess running times:

p1T(N,V) :- statistics(runtime,[T0|_]),
	fourSquares(N,V),
	statistics(runtime, [T1|_]),
	T is T1 - T0,
	format('fourSqaures/2 takes ~3d sec.~n', [T]).
*/	


%% Q2


:- begin_tests(q4).
test('disarm 1', [nondet]) :-
	disarm([1,3,3,4,6,10,12],[3,4,7,9,16],[[[1, 3], [4]], [[3, 6], [9]], [[10], [3, 7]], [[4, 12], [16]]]); 	
	disarm([1,3,3,4,6,10,12],[3,4,7,9,16],[[[1, 3], [4]], [[3, 4], [7]], [[12], [3, 9]], [[6, 10], [16]]]).
:- end_tests(q4).


:- begin_tests(q5).
test('disarm 2', all(S == [[]])) :- 
	disarm([],[],S).
:- end_tests(q5).


:- begin_tests(q6).
test('disarm 3', all(S == [[[[1, 2], [3]], [[3, 3], [6]], [[8], [4, 4]], [[5, 5], [10]]]])) :- 
	disarm([1,2,3,3,8,5,5],[3,6,4,4,10],S), !.
:- end_tests(q6).


:- begin_tests(q7).
test('disarm 4', [fail]) :- disarm([1,2,2,3,3,8,5],[3,2,6,4,4,10],S).
:- end_tests(q7).


:- begin_tests(q8).
test('disarm 5', [fail]) :- disarm([1,2,2,3,3,8,5,5,6,7],[3,2,6,4,4,10,1,5,2],S).
:- end_tests(q8).


:- begin_tests(q9).
test('disarm 6', all(S = [[[[1, 2], [3]], [[2, 2], [4]], [[2, 3], [5]], [[5], [1, 4]], [[6], [3, 3]], [[3, 8], [11]]|_]])) :- 
	disarm([1,2,2,116,3,3,5,2,5,8,5,6,6,8,32,2],[3,5,11,4,37,1,4,121,3,3,14],S), !.


:- end_tests(q9).


p2T(S) :- statistics(runtime,[T0|_]),
	p6(S),
	statistics(runtime, [T1|_]),
	T is T1 - T0,
	format('p6/1 takes ~3d sec.~n', [T]).
%p6/1 takes 10.532 sec.
%S = [[[1, 2], [3]], [[2, 2], [4]], [[2, 3], [5]], [[5], [4, 1]], [[6], [3, 3]], [[3, 8], [11]], [[6|...], [...]], [[...|...]|...], [...|...]].


/* ---------------------------------------------------------
Running tests:
--------------------------------------------------------- */
:- run_tests(q1).

:- run_tests(q2).

:- run_tests(q3).

:- run_tests(q4).

:- run_tests(q5).

:- run_tests(q6).

:- run_tests(q7).

:- run_tests(q8).

:- run_tests(q9).
