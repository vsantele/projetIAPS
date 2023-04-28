:- use_module(library(random)).
:- use_module('constants.pl').
:- use_module('board.pl').
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

findCountry(italie, Players, [Players, _,_,_]).
findCountry(hollande, Players, [_, Players,_,_]).
findCountry(belgique, Players, [_, _,Players,_]).
findCountry(allemagne, Players, [_, _,_,Players]).

% Sur base d'une liste de joueurs [[p1x, p1y], [p2x, p2y], ...], renvoie l'index du dernier joueur sur le plateau pouvant bouger.
findLatestPlayer(Players, LatestPlayerI, Board) :-
    findall(Player, nth0(PlayerI, Players, Player), PlayersList),
    latestPlayer(LatestPlayerI,LatestPlayer, -1,[1000,1000], PlayerI, Player, Board).

% Retourne le joueurs le plus en retard sur le plateau pouvant bouger et ses coordonnées.
latestPlayer(Player2I,[P2x, P2y], Player1I,[P1x, P1y], Player2I, [P2x,P2y], Board) :-
    P1x > P2x, canMove([P2x,P2y], Board).
latestPlayer(Player1I,[P1x, P1y], Player1I,[P1x, P1y], Player2I, [P2x,P2y], Board) :-
    P1x < P2x, canMove([P1x,P1y], Board).
latestPlayer(Player1I,[P1x, P1y], Player1I,[P1x, P1y], Player2I, [P2x,P2y], Board).

% Sur base de coordonnées, vérifie s'il peut avancer sans risque.
% TODO: Peut-être vérifier en cas de dépassement?
canMove([Px, Py], Board) :-
    chemin(Px, Py, Tx, Ty),
    not(hasPlayer(Tx, Ty, Board)).

% Vérifie s'il y a un joueur sur les coordonnées entrées.
hasPlayer(Px, Py, [Country|Board]) :-
    nth0(_, Country, [Px, Py]).

hasPlayer(Px, Py, [Country]) :-
    nth0(_, Country, [Px, Py]).

hasPlayer(Px, Py, []) :-
    false.