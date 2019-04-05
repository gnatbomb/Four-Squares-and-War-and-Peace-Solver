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

%Return first N
count(N, M, X) :- 
    N =< M,
    X is N.

%Callback on N+1
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
    Root is sqrt(N) + 1,
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
putFirst([First], L)
*/
putFirst([First], L) :-
    L = First.


/*
returnValues(Values, L)
returns the values for L one by one.
*/
returnValues([First|_], L) :-
    L = First.

returnValues([_|Rest], L) :-
    returnValues(Rest, L).


/*
makeCall(List, L)
prints a single value if List only has one member, or all values if List has multiple.
*/
makeCall(List, L) :-
    length(List, 1),
    !,
    putFirst(List, L).

makeCall(List, L) :-
    returnValues(List, L).

/*
fourSquares(N, L)
finds all appropriate values for the 4 numbers, then removes all duplicate sets by using sort, then calls returnValues to return the values one by one.
*/

fourSquares(N, L) :-
    findall(X, fourSquaresHelper(N, X), OutList),
    sort(OutList, NoDupeList),
    makeCall(NoDupeList, L).
    

/*
Question 2 - War and Peace
write disarm(A, B, Solution)
Where A and B are lists of integers,
and Solution is the order which A and B should remove one or two values such that their totals remain the same.

Each step must dismantle more than or as much as the previous step's dismantlement.
*/

/*
sumList(L, Sumsofar, Sum)
returns the total sum of L in Sum
*/




/*
getDisarm(Single, List, StepSolSoFar, StepSol)
Given Single, tries to find two members of List which equal Single. Returns them in StepSol
given
*/ 
%use_module(library(clpfd)).


%SumList returns the sum of the values in a list
sumList([], SumSoFar, SumSoFar).

sumList([First|Rest], SumSoFar, Sum) :-
    SumPlus is SumSoFar + First,
    sumList(Rest, SumPlus, Sum).


%equalSumList returns true if A and B have the same sum of parts.
equalSumList(A, B) :-
    sumList(A, 0, Asum),
    sumList(B, 0, Bsum),
    Bsum is Asum.


%getSumList calls sumList on the first of two elements.
getSumList([L|_], Sum) :-
    sumList(L, 0, Sum).

/*
disarm(A, B, Solution)
begins by checking if a solution is impossible,
    first checks if length of A + B is a multiple of 1.5,
    then checks that A and B have the same sums,
After that, it sorts A and B, then strips their values. Stripping takes all n instances of integer x and puts it into a list
as [[x, n]] to keep track of how many x's we have. This helps reduce pointless backtracking.
Lastly, we get a solution with disarmer, and reverse the order of the solution.

I was able to save a lot of backtracking by always using the largest value left in B and A as my "disarmament goal".
This is because as the largest number, backtracking has fewer options higher in the tree, which means exponentially
fewer backtracking checks further down the tree.
*/
disarm(A, B, Solution) :-
    length(A, X),
    length(B, Y),
    Z is (X + Y) / 1.5,
    R is round(Z),
    F is float(R),
    Z is F,
    msort(A, SA),
    msort(B, SB),
    equalSumList(A, B),
    stripValues(SA, [], StrippedA),
    stripValues(SB, [], StrippedB),
    disarmer(StrippedA, StrippedB, [], Sol),
    flipSol(Sol, [], Solution).


%flipSol flips the solution list that I ended up with.
flipSol([], S, S).

flipSol([F|R], SF, Sol) :-
    append([F], SF, NF),
    flipSol(R, NF, Sol).


%firstPair returns the Value of the first pair of armaments
firstPair([V|_], V).


%getCount returns how many of the specified armaments there are.
getCount([_,C],C).

/*
List Stripping functions
*/
stripV(F, [], PopList, NewL) :-
    append([[F, 1]], PopList, NewL).

stripV(F, [First|Rest], PopList, NewL) :-
    firstPair(First, F),
    getCount(First, N),
    M is N+1,
    append(PopList, [[F, M]], NPopList),
    append(NPopList, Rest, NewL).

stripV(F, [First|Rest], PopList, NewL) :-
    not(firstPair(First, F)),
    append(PopList, [First], NPopList),
    stripV(F, Rest, NPopList, NewL).


stripValue(F, MidL, NewL) :-
    stripV(F, MidL, [], NewL).

stripValues([], M, M).

stripValues([F|R], MidL, StrippedL) :-
    stripValue(F, MidL, Strip),
    stripValues(R, Strip, StrippedL).

%agreater returns true if the first element of A is greater than the first element of B.
agreater([[FA|_]|_],[[FB|_]|_]) :-
    FA > FB.

/*
disarmer
*/
%main loop: base case. Cuts after finding a solution.
disarmer([], [], L, L) :- !.

%main loop: case A > B. Gets the pairing using the first element of A as the disarmament quota.
disarmer(A, B, Lsf, L) :-
    agreater(A, B),
    getPairing(A, B, GP, NewA, NewB),
    noflip(GP, P),
    append(Lsf, [P], NLsf),
    disarmer(NewA, NewB, NLsf, L).

%main loop: case A <= B. Gets the pairing using the first element of B as the disarmament quota.
disarmer(A, B, Lsf, L) :-
    not(agreater(A, B)),
    getPairing(B, A, RevL, NewB, NewA),
    flip(RevL, P),
    append(Lsf, [P], NLsf),
    disarmer(NewA, NewB, NLsf, L).

%flip: helper function which puts brackets in the right places and fixes the order of the pairs.
flip([F|R], L) :-
    append([R], [[F]], L).

%noflip: helper function which puts brackets in the right places.
noflip([F|R], L) :-
    append([[F]], [R], L).


/*
getPairing(One, Two, P, NewOne, NewTwo)
One: the list we will take the quota from. Two: the list we will disarm two battlements. P : the pair of quota + battlements being disarmed.
NewOne: The new armament inventory for the first group.
NewTwo: Then new armament inventory for the second group.
*/
getPairing(One, Two, P, NewOne, NewTwo) :-
    getFirst(One, OneV, _),
    getPair(Two, OneV, [], 0, Pair, NewTwo),
    append([OneV], Pair, P),
    subtractOne(One, NewOne).


%bookkeeping function: decrements the specified battlement inventory by one. Then calls keepOrTrash.
subtractOne([F|Rest], NewOne) :-
    getFirs(F, V, C),
    Nc is C - 1,
    keepOrTrash(V, Nc, Rest, NewOne).

%keepOrTrash: adds the new values to the list. If there are no more of the specified battlement size, removes that from the list.
keepOrTrash(_, 0, Rest, Rest).

keepOrTrash(V, C, Rest, NewOne) :-
    C > 0,
    append([[V, C]], Rest, NewOne).


/*
getPair(Two, Quota, PairSoFar, PairCount, Pair, NewTwo)
Two: the list we are disarming two battlements from. Quota: the amount of military power we are disarming this month.
PairSoFar: what we have disarmed from Two. PairCount: how many battlements from two we have disarmed.
NewTwo: the new inventory count for Two.
*/
%base case: the quota has been met (= 0), and we have dismantled two battlements.
getPair(Two, 0, P, 2, P, Two).

%main loop: first value is < the quota. Removes the battlement and recurses.
getPair([F|R], Quota, PairSoFar, PairCount, Pair, NewTwo) :-
    getFirs(F, V, _),
    V =< Quota,
    Nq is Quota - V,
    subtractOne([F|R], NT),
    append([V], PairSoFar, Npsf),
    Npc is PairCount + 1,
    getPair(NT, Nq, Npsf, Npc, Pair, NewTwo).

%main loop: first value is > the quota. Continues with the next smallest battlements.
getPair([F|R], Quota, PairSoFar, PairCount, Pair, NewTwo) :-
    getPair(R, Quota, PairSoFar, PairCount, Pair, NT),
    append([F], NT, NewTwo).

%helper function for getting values from the inventory.
getFirst([[FA, RA]|_], FA, RA).

%helper function for getting values from the inventory.
getFirs([FA, RA], FA, RA).