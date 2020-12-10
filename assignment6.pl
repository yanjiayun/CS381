/*Group member: Jiayu Yan, Zhixuan Li*/

/* Exercise 1 */
redefine_system_predicate(when).

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

/*(a)*/
schedule(X,Y,Z):-enroll(X,A),where(A,Y),when(A,Z).

/*(b)*/
usage(X,Y):-where(A,X),when(A,Y).

/*(c)*/
conflict(A,B):-where(A,X),when(A,Y),where(B,X),when(B,Y),A\=B.

/*(d)*/
meet(A,B):-enroll(A,X),enroll(B,X),A\=B.
meet(A,B):-enroll(A,X),where(X,C),when(X,Z),enroll(B,Y),where(Y,C),when(Y,D),D is Z+1.


/* Exercise 2 */

/*(a)*/
rdup([],[]).
rdup([A|B],[A|B1]):-subtract(B,[A],B2),rdup(B2,B1).

/*(b)*/
flat([], []) :- !.
flat([L|Ls], FlatL):-!,flat(L, NewL),flat(Ls, NewLs),append(NewL, NewLs, FlatL).
flat(L, [L]).

/*(c)*/
/*The search position cannot be larger than the range of the List!*/
project(Ps, L, L0):- project(1, Ps, L, L0).
project(_, [], _, []).
project(P, [P0|Ps], L, [X|Xs]):- shift(P, L, P0, [X|L0]), project(P0, Ps, [X|L0], Xs).
shift(N, L, N, L) :- !.
shift(N, [_|L], N0, L0) :- N1 is N+1, shift(N1, L, N0, L0).