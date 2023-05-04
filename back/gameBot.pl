:- use_module(library(random)).
:- use_module('constants.pl').
:- use_module('board.pl').

% EXemples de plateau de jeu
board([[[10, 1], [10, 2], [10, 3], [20, 1]],[[20, 2], [20, 3], [30, 1], [30, 2]],[[30, 3], [40, 1], [40, 2], [40, 3]],[[50, 1], [50, 2], [50, 3], [60, 1]]]).

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
% https://stackoverflow.com/questions/32918211/find-the-max-element-and-its-index-in-a-list-prolog
findLatestPlayer(Players, LatestPlayerI, Board) :-
    latestPlayer(Players, LPlayer, Board),
    canMove(LPlayer, Board),
    nth1(LatestPlayerI, Players, LPlayer ).

latestPlayer([HPlayer|LPlayers], LPlayer, Board) :-
    latestPlayer(LPlayers, HPlayer, LPlayer, Board).

% https://stackoverflow.com/a/19810489/10171758
latestPlayer([], Player,Player,_Board).
latestPlayer([[P1x,P1y] | LPlayer], [LPx, LPy], LP, Board) :-
    P1x < LPx, canMove([P1x,P1y],Board ) -> latestPlayer(LPlayer, [P1x,P1y], LP, Board)
    ; latestPlayer(LPlayer, [LPx, LPy], LP, Board).


% Retourne le joueurs le plus en retard sur le plateau pouvant bouger et ses coordonnées.
latestPlayer(Player2I,[P2x, P2y], _Player1I,[P1x, _P1y], Player2I, [P2x,P2y], Board) :-
    P1x > P2x, canMove([P2x,P2y], Board).
latestPlayer(Player1I,[P1x, P1y], Player1I,[P1x, P1y], _Player2I, [P2x,_P2y], Board) :-
    P1x < P2x, canMove([P1x,P1y], Board).
latestPlayer(Player1I,[P1x, P1y], Player1I,[P1x, P1y], _Player2I, [_P2x,_P2y], _Board).

% Sur base de coordonnées, vérifie s'il peut avancer sans risque.
% TODO: Peut-être vérifier en cas de dépassement?
canMove([Px, Py], Board) :-
    chemin(Px, Py, Tx, Ty),
    not(hasPlayer(Tx, Ty, Board)).

% Vérifie s'il y a un joueur sur les coordonnées entrées.
hasPlayer(Px, Py, [Country|LCountry]) :-
    hasPlayer(Px,Py, LCountry);
    member([Px, Py], Country).
hasPlayer(Px, Py, [Country]) :-
    member([Px, Py], Country).