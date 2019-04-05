/*
CMPUT 325 Winter 2019 Assignment 4
CCID nbombard Student name Nicholas Bombardieri


Question 1 - Four Squares
Given an natural n, return up to 4 square numbers which sum to n.
*/

/*
Count(N, M, X)
If N =< M, returns N, or increments N and repeats
*/

count(N, M, X) :- 
    N =< M,
    X is N.

count(N, M, X) :-
    N =< M,
    count(N+1, M, X).


/*
Foursquares
*/

getFourthSquare(N, [_, _, _, S4]) :-
    count(0, N, S4).

getThirdSquare(N, [S1, S2, S3, S4]) :-
    count(0, N, S3),
    getFourthSquare(N, [S1, S2, S3, S4]).

getSecondSquare(N, [S1, S2, S3, S4]) :-
    count(0, N, S2),
    getThirdSquare(N, [S1, S2, S3, S4]).

getFirstSquare(N, [S1, S2, S3, S4]) :- 
    count(0, N, S1),
    getSecondSquare(N, [S1, S2, S3, S4]).

%Helper call
fourSquaresHelper(N, OutList) :- 
    getFirstSquare(N, [S1, S2, S3, S4]), 
    N is (S1^2 + S2^2 + S3^2 + S4^2),
    msort([S1, S2, S3, S4], OutList).

returnValues([First|_], L) :-
    L = First.

returnValues([_|Rest], L) :-
    returnValues(Rest, L).


%Original Call
fourSquares(N, L) :-
    findall(X, fourSquaresHelper(N, X), OutList),
    sort(OutList, NoDupeList),
    returnValues(NoDupeList, L).

    




