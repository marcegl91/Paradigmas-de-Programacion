show(X, S) :-
    vari(X),
    variNumber(X, N),
    swritef(S, 'x%d', [N]).
show(mvar(X), S) :-
    show(X, S).
show(app(M, N), S) :-
    N=app(_, _),
    show(M, SM),
    show(N, SN),
    swritef(S, '%w (%w)', [SM, SN]).
show(app(M, N), S) :-
    N\=app(_, _),
    show(M, SM),
    show(N, SN),
    swritef(S, '%w %w', [SM, SN]).
show(lambda(X, M), S) :-
    show(X, SX),
    show(M, SM),
    swritef(S, '(λ%w. %w)', [SX, SM]).
show(X) :-
    show(X, S),
    write(S).

variNumber(x, 0).
variNumber(^(X), N) :-
    variNumber(X, M),
    N is M+1.

%Ejemplos:
%M1 = (λx0. x0)
term(1, lambda(x, mvar(x))).
%M2 = (λx0. x0) x1
term(2, app(Id, mvar(^(x)))) :-
    term(1, Id).
%M3 = (λx1. x0)
term(3, lambda(^(x), mvar(x))).
%M4 = (λx1. x0) x2
term(4, app(ConstX, mvar(^(^(x))))) :-
    term(3, ConstX).
%M5 = (λx0. (λx1. x0))
term(5, lambda(x, lambda(^(x), mvar(x)))).
%M6 = (λx0. (λx1. x1))
term(6, lambda(x, lambda(^(x), mvar(^(x))))).
%M7 = (λx0. x0 x0)
term(7, lambda(x, app(mvar(x), mvar(x)))).
%M8 = (λx0. x0 x0) (λx0. x0 x0) -- este término reduce a sí mismo.
term(8, app(Omega, Omega)) :-
    term(7, Omega).
%M9 = (λx1. x0) ((λx0. x0 x0) (λx0. x0 x0)) -- este término admite una reducción infinta y una f.n.
term(9, app(ConstX, OMEGA)) :-
    term(3, ConstX),
    term(8, OMEGA).
% Casos custom
%M10 = caso no valido (para testear)
term(10, lambda(^(x), x)).
%M11 = (λx0. (λx0. x1))
term(11, lambda(x, lambda(x, mvar(^(x))))).
term(12, lambda(^(^(x)), lambda(^(^(x)), mvar(^(x))))).
term(13, lambda(^(x), lambda(^(^(x)), mvar(x)))).

% term(9,M), term(M).

%Ej 1A: vari(?X)
vari(x).
vari(^(X)) :-
    vari(X).

%Ej 1B: term(+M)
%Definir term(-M) para nuestra implementacion genera lo mismo que vari(-M), es decir va instanciando X en mvar(X), mvar(^(x)) y asi siguiendo.
%si le cambiamos el orden a las reglas, prolog se colgaria para ambos casos.
term(mvar(X)) :-
    vari(X).
term(lambda(V, M)) :-
    vari(V),
    term(M).
term(app(M, N)) :-
    term(M),
    term(N).


% borrar(+ListaOriginal, +X, -ListaSinXs)
% borrar([], _, []).
% borrar([X|Xs], X, R) :-
%     borrar(Xs, X, R).
% borrar([Y|Xs], X, [Y|Ys]) :-
%     Y\=X,
%     borrar(Xs, X, Ys).

%Ej 2: fv(+M, -Xs)
%para testear fv(app(mvar(x), mvar(^(x))), Xs).
fv(mvar(X), [X]).
fv(lambda(V, M), Xs) :-
    fv(M, Ys),
    delete(Ys, V, Xs).
fv(app(M, N), Xs) :-
    fv(M, Ys),
    fv(N, Zs),
    union(Ys, Zs, Xs).

%Ej 3: sustFV(+M, +X, +Y, ?MSust)
% term(11, M), sustFV(M, ^(x), ^(^(x)), R), show(R).
sustFV(mvar(X), X, Y, mvar(Y)).
sustFV(mvar(X), Z, _, mvar(X)) :-
    X\=Z.
sustFV(lambda(V, M), X, Y, lambda(V, M2)) :-
    fv(lambda(V, M), Ls),
    member(X, Ls),
    sustFV(M, X, Y, M2).
sustFV(lambda(V, M), X, _, lambda(V, M)) :-
    fv(lambda(V, M), Ls),
    not(member(X, Ls)).
sustFV(app(M, N), X, Y, app(M2, N2)) :-
    sustFV(M, X, Y, M2),
    sustFV(N, X, Y, N2).


%variableFresca(+Ms, +Ns, -X)
variableFresca(Xs, X) :-
    vari(X),
    not(member(X, Xs)), !.

%Ej 4: alphaEq(+M, +N) 
alphaEq(mvar(X), mvar(X)).
alphaEq(app(M, N), app(M1, N1)) :-
    alphaEq(M, M1),
    alphaEq(N, N1).
alphaEq(lambda(V, M), lambda(W, N)) :-
    fv(M, Ms),
    fv(N, Ns),
    union(Ms, Ns, Xs),
    variableFresca(Xs, H),
    not(member(H, Ms)),
    not(member(H, Ns)),
    sustFV(M, V, H, M1),
    sustFV(N, W, H, N1),
    alphaEq(M1, N1).

%variables(mvar(X), [X]).
%variables(lambda(V, M), Xs) :-
%	variables(M, Ms), append(V, Ms, Xs).
%variables(app(M, N), Xs) :-
%	variables(M, Ms),
%	variables(N, Ns),
%	append(Ms, Ns, Xs).

%variableFresca1(M, Z) :-
%	vari(Z),
%	variables(M, Xs),
%	not(member(Z, Xs)).

%Ej 5: sust(+M, +X, +N, ?MSust)
sust(mvar(X), X, N, N).
sust(mvar(Y), X, _, mvar(Y)) :-
    X\=Y.
sust(app(M, P), X, N, app(M1, P1)) :-
    sust(M, X, N, M1),
    sust(P, X, N, P1).
sust(lambda(X, M), X, _, lambda(X, M)). 
sust(lambda(Y, M), X, N, lambda(Z, Q)) :-
    X\=Y,
    fv(M, Ms),
    fv(N, Ns),
    union(Ms, Ns, Xs),
    variableFresca(Xs, Z),
    sust(M, Y, mvar(Z), M1),
    sust(M1, X, N, Q). 

%Ej 6A: betaRedex(+R, ?N)
betaRedex(app(lambda(V, M), S), N) :-
    sust(M, V, S, N).

%Ej 6B: reduce(+M, ?N)
reduce(X, N) :-
    betaRedex(X, N).
reduce(app(M, N), app(M1, N)) :-
    reduce(M, M1).
reduce(lambda(X, M), lambda(X, N)) :-
    reduce(M, N).

reduce(app(M, Q), app(M, N)) :-
    reduce(Q, N).

%Ej 7: formaNormal(+M)
formaNormal(M) :-
    not(reduce(M, _)).

%Ej 8: leftmost(+M, -N)
leftmost(M, N) :-
    reduce(M, N), !.

%Ej 9: formaNormal(+M, -N)
formaNormal(M, M) :-
    formaNormal(M).
formaNormal(M, N) :-
    leftmost(M, N1),
    formaNormal(N1, N).

%Ej 10A: long(+M, ?K)

%Ejecricio 10:
long(mvar(_), 1).
long(app(M, N), L) :-
    ground(app(M, N)),
    long(M, LongM),
    long(N, LongN),
    L is LongN+LongM.
long(lambda(_, M), L) :-
    ground(M),
    long(M, LongM),
    L is LongM+1.

%Ej 10B: long(-M, +K)
long(app(M, N), L) :-
    nonground(app(M, N), _),
    splitN1(LongM, LongN, L),
    long(M, LongM),
    long(N, LongN).
long(lambda(_, M), L) :-
    nonground(M, _),
    Lm1 is L-1,
    L>0,
    long(M, Lm1).

splitN1(A, B, N) :-
    Nm1 is N-1,
    between(1, Nm1, A),
    B is N-A.

%Ej 11A: Extender la definición de fv\2 para que soporte un término parcialmente instanciado 
%fv(-M, +Xs)
:- (discontiguous fv/2).

%Ej 11B: cerrado(?M)
