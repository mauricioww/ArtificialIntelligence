%%   =============    Solved by Marcos Mauricio Carpintero Mendoza    2017630231      ===============


% =======================================================================================
% ==================                Exercise one             ============================
% =======================================================================================
% =======================================================================================


use_module(library(clpfd)).

sum_two(A, B, S) :- S #= A + B.
sum_list(X, Y) :- sum_list(X, Y, 0).
sum_list([X|Xs], [Y|Ys], Acc) :- Acc2 #= Y,
                                sum_two(Acc, X, Acc2),
                                sum_list(Xs, Ys, Acc2).
sum_list([], [], _).                                

% sum_list([1, 4, 6, 7, 2], [1, 5, 11, 18, 20]).
% true.

% sum_list([1, 4, 6, 7], [1, 5, 11, 18]).
% true.

% sum_list([1, 4, 6, 7, 2, 3], [1, 5, 11, 11, 20, 23]).
% false.

% =======================================================================================
% ==================                Exercise one             ============================
% =======================================================================================
% =======================================================================================



% =================================================================
% =================== Facts: Knowledge base =======================
% =================================================================

parent(a, b).
parent(b, d).
parent(b, e).
parent(e, g).
parent(e, h).
parent(e, i).
parent(a, c).
parent(c, f).
parent(f, j).
parent(f, k).

% ==========================================================================================
% =================== Predicates: True or False according the inputs =======================
% ==========================================================================================

% ======= I assume you wanna know if X and Y are siblings, obviously they are different ========

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% ======= I assume you wanna know if a person Y is grandson of X ========

grandchildren(X, Y) :- parent(X, Z), parent(Z, Y).

% ======= I assume you wanna know if a person Y and X are cousins ========

cousin(X, Y) :- parent(Gfather, Father), parent(Father, X), parent(Gfather, Z), parent(Z, Y), 
                Father \= Z .

% ======= I assume you wanna know if a person Y is relative of X  ========

relative(X, Y) :- parent(X, Y).
relative(X, Y) :- parent(X, Z), relative(Z, Y).

% =================== Example of queries =======================

% a) Sibling
%   sibling(d, e) -> true, sibling(k, j) -> true, sibling(f, e) -> false
%   sibling(g, X) -> X=h; X=i.      sibling(X, f) -> false.

% b) grandchildren
%   grandchildren(f, a) -> false, grandchildren(a, f) -> true, grandchildren(b, i) -> true.
%   grandchildren(a, X) -> X=d; X=e, X=f.     grandchildren(c, X) -> X=j; X=k.

% c) Cousin.
%   cousin(d, f) -> true, cousin(g, h) -> false, cousin(f, e) -> true.
%   cosin(X, f) -> X=d; X=e.        cousin(g, X) -> false.

% d) Relative.
%   relative(b, i) -> true, relative(b, j) -> false, relative(f, a) -> false.
%   relative(c, X) -> X=f; X=j; X=k.    relative(b, X) -> X=d; X=e; X=g; X=g, X=i.

% ==========================================================================================
% ==== Queries: According to the input it gets the info based on the previous predicates ====
% ==========================================================================================

siblings_of(X, Siblings) :- bagof(S, sibling(X, S), Siblings).

grandchildren_of(X, GChildrens) :- bagof(G, grandchildren(X, G), GChildrens).

cousins_of(X, Cousins) :- bagof(C, cousin(X, C), Cousins).

relatives_of(X, Relatives) :- bagof(R, relative(X, R), Relatives).

% a) Sibling
%   siblings_of(e, A) -> [d], sibling(g, A) -> [h, i], sibling(a, A) -> false
%   sibling(X, Y) -> X=b, Y=[c]; X=c, Y=[b]; ...; X=i, Y=[h,g].

% b) Grandchildren
%   grandchildren_of(a, X) -> [d, e, f], grandchildren_of(c, X) -> [j, k], grandchildren_of(f, C) -> false.
%   grandchildren_of(X, Y) -> X=a, Y=[d, e, f]; X=b, Y=[g, h, i]; X=c, Y=[j, k].

% c) Cousin.
%   cousins_on(d, X) -> [f], cousins_of(f, X) -> [e, d], cousins_of(b, X) -> false.
%   cousins_of(X, Y) -> X=d, Y=[f]; X=e, Y=[f], X=f, Y=[d,e].

% d) Relative.
%   relatives_of(c, X) -> X=[f,j,k], relatives_of(b, X) -> X=[d, e, g, h, i].
%   relatives_of(X, Y) -> X=a, Y=[b, c, d, e, g ,h|...]; X=b, Y=[d, e, g, h, i], ...



% =======================================================================================
% ==================                Exercise three             ============================
% =======================================================================================
% =======================================================================================

% ==========================================================================================
% ========== Exercises with list as sets. ie: Sets may have repeated elements ============
% ==========================================================================================


% =================== It searchs a element in a set: True if it is in, else False =======================

element_in(E, [_|T]) :- element_in(E, T),!.
element_in(E, [E|_]).

% =============== It returns False or True according to the length you suppose of the set  ================

size(S, N) :- size_real(S, Y), N #= Y. 
size_real([_|S], X) :- X #= 1 + Y, size_real(S, Y).
size_real([], 0).

% ======== I assume that s2 should be subset of s1, so this funct returns if s2 is actually a subset =======

sub_set(S1, [S2|T]) :- element_in(S2, S1), sub_set(S1, T),!.
sub_set(_, []).

% ================  Simple set operations as are named =======================

join(S1, S2, J) :- setof(X, (member(X, S1); member(X, S2)), J).
intersection(S1, S2, I) :- setof(X, (member(X, S1), member(X, S2)), I).
differ(S1, S2, D) :- setof(X, (member(X, S1), not(member(X, S2))), D).