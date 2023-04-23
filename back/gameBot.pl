:- use_module(library(random)).
:- use_module('constants.pl').

% Tire un nombre entre val_chance_min et val_chance_max.
valeurCarteChance(Val) :- val_chance_min(Min), val_chance_max(Max), random_between(Min, Max, Val).

% Génère une liste de nombres compris entre 2 bornes
elementsEntreBornes(X, X, [X]).
elementsEntreBornes(X, Y, [X|Xs]) :-
    Y >= X,
    Z is X+1,
    elementsEntreBornes(Z, Y, Xs).

% Concatène 2 listes
concatenerListes([], []).
concatenerListes([L|Ls], List) :- concatenerListes(Ls, Liste2), append(L, Liste2, List).

% Génère la liste des cartes secondes disponibles
cartesDisponibles(Cartes) :- val_min_carte(Min), val_max_carte(Max), nb_repetition_cartes(NbRepet),
    elementsEntreBornes(Min, Max, Valeurs),
    length(Temp, NbRepet),
    maplist(=(Valeurs), Temp),
    concatenerListes(Temp, Cartes), !.

% Tire NbCartes au hasard - ne retire pas les cartes de la liste intiale
% Thanks ChatGPT :)
tirerCartes(CartesDisponibles, NbCartes, Cartes) :-
    length(CartesDisponibles, Len),
    ( NbCartes > Len ->
        Cartes = CartesDisponibles
    ; findall(Elem, nth0(_, CartesDisponibles, Elem), Elems),
      random_permutation(Elems, Perm),
      length(Cartes, NbCartes),
      append(Cartes, _, Perm)
    ).