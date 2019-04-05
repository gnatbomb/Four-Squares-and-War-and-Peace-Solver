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
getSquares
fills the values for s1, s2, s3, and s4 one by one.
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

/*
getMaxSquare(N, M) 
Returns M as the rounded square root of N. This helps cut down search time.
*/
getMaxSquare(N, M) :-
    Root is sqrt(N),
    M is round(Root).


/*
fourSquaresHelper(N, OutList)
Finds values for S1-S4, then checks if that set is a square set for N. If so, sorts that set and returns it as outList
*/
fourSquaresHelper(N, OutList) :- 
    getMaxSquare(N, M),
    getFirstSquare(M, [S1, S2, S3, S4]), 
    N is (S1^2 + S2^2 + S3^2 + S4^2),
    msort([S1, S2, S3, S4], OutList).

/*
returnValues(Values, L)
returns the values for L one by one.
*/

returnValues([First|_], L) :-
    L = First.

returnValues([_|Rest], L) :-
    returnValues(Rest, L).



/*
fourSquares(N, L)
finds all appropriate values for the 4 numbers, then removes all duplicate sets by using sort, then calls returnValues to return the values one by one.
*/
fourSquares(N, L) :-
    findall(X, fourSquaresHelper(N, X), OutList),
    sort(OutList, NoDupeList),
    returnValues(NoDupeList, L).

    




