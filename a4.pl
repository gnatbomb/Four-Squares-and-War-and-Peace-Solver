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

sumList([], SumSoFar, SumSoFar).

sumList([First|Rest], SumSoFar, Sum) :-
    SumPlus is SumSoFar + First,
    sumList(Rest, SumPlus, Sum).

/*
getDisarm(Single, List, StepSolSoFar, StepSol)
Given Single, tries to find two members of List which equal Single. Returns them in StepSol
given
*/ 
getDisarm(Single, _, StepSolSoFar, StepSolSoFar) :-
    length(StepSolSoFar, X),
    X >= 2,
    Single == 0.

getDisarm(Single, [First|Rest], StepSolSoFar, StepSol) :-
    length(StepSolSoFar, X),
    X < 2,
    NewSingle is Single - First,
    NewSingle >= 0,
    append(StepSolSoFar, [First], NewSolSoFar),
    getDisarm(NewSingle, Rest, NewSolSoFar, StepSol).

getDisarm(Single, [_|Rest], StepSolSoFar, StepSol) :-
    length(StepSolSoFar, X),
    X < 2,
    getDisarm(Single, Rest, StepSolSoFar, StepSol).

/*
singleDisarm(A, B, SolutionSoFar)
A disarms one step, B disarms 2.
*/
singleDisarm([AFirst|_], [BFirst|BRest], PrevDisarm, StepSol) :-
    PrevDisarm =< AFirst,
    getDisarm(AFirst, [BFirst|BRest], [], SolNoFirst),
    append([[AFirst]], [SolNoFirst], StepSol).

singleDisarm([AFirst|ARest], [BFirst|_], PrevDisarm, StepSol) :-
    PrevDisarm =< BFirst,
    getDisarm(BFirst, [AFirst|ARest], [], SolNoSecond),
    append([SolNoSecond], [[BFirst]], StepSol).

singleDisarm([_|ARest], B, PrevDisarm, StepSol) :-
    singleDisarm(ARest, B, PrevDisarm, StepSol).

singleDisarm(A, [_|BRest], PrevDisarm, StepSol) :-
    singleDisarm(A, BRest, PrevDisarm, StepSol).

    
/*
RefreshList([OldFirst|OldRest], RemovedValue, NewListSoFar, NewList)
*/
refreshList([OldFirst|OldRest], RemovedValue, NewListSoFar, NewList) :-
    OldFirst == RemovedValue,
    append(NewListSoFar, OldRest, NewList).

refreshList([OldFirst|OldRest], RemovedValue, NewListSoFar, NewList) :-
    OldFirst \== RemovedValue,
    append(NewListSoFar, [OldFirst], NewSoFar),
    refreshList(OldRest, RemovedValue, NewSoFar, NewList).

/*
refreshCall
*/
refreshCall(A, A, []).

refreshCall(A, NewA, [First|Rest]) :-
    refreshList(A, First, [], MiniNew),
    refreshCall(MiniNew, NewA, Rest).

/*
RefreshLists
*/
refreshLists(A, NewA, B, NewB, [AVals, BVals]) :-
    refreshCall(A, NewA, AVals),
    refreshCall(B, NewB, BVals).

/*
getDisarm
*/
getDisarm([First|_], Value) :-
    sumList(First, 0, Value).

/*
disarmHelper(A, B, SolutionSoFar, Solution)
*/
disarmHelper([], [], _, SolutionSoFar, SolutionSoFar).

disarmHelper(A, B, PrevDisarm, SolutionSoFar, Solution) :-
    singleDisarm(A, B, PrevDisarm, Step),
    refreshLists(A, NewA, B, NewB, Step),
    getDisarm(Step, NewDisarm),
    append(SolutionSoFar, [Step], NewSolutionSoFar),
    disarmHelper(NewA, NewB, NewDisarm, NewSolutionSoFar, Solution).

disarmHelper([FirstA|RestA], B, PrevDisarm, SolutionSoFar, Solution) :-
    singleDisarm(RestA, B, PrevDisarm, Step),
    refreshLists(RestA, NewA, B, NewB, Step),
    getDisarm(Step, NewDisarm),
    append(SolutionSoFar, [Step], NewSolutionSoFar),
    disarmHelper([FirstA|NewA], NewB, NewDisarm, NewSolutionSoFar, Solution).

disarmHelper(A, [FirstB|RestB], PrevDisarm, SolutionSoFar, Solution) :-
    singleDisarm(A, RestB, PrevDisarm, Step),
    refreshLists(A, NewA, RestB, NewB, Step),
    getDisarm(Step, NewDisarm),
    append(SolutionSoFar, [Step], NewSolutionSoFar),
    disarmHelper(NewA, [FirstB|NewB], NewDisarm, NewSolutionSoFar, Solution).

/*
disarm
*/
disarm(A, B, Solution) :-
    msort(A, SortedA),
    msort(B, SortedB),
    disarmHelper(SortedA, SortedB, 0, [], Solution).