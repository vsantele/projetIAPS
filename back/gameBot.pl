:- use_module(library(random)).
:- use_module('constants.pl').
:- use_module('board.pl').

% EXemples de plateau de jeu
board([[[10, 1], [10, 2], [10, 3], [20, 1]],[[20, 2], [20, 3], [30, 1], [30, 2]],[[30, 3], [40, 1], [40, 2], [40, 3]],[[50, 1], [50, 2], [50, 3], [60, 1]]]).

% Plateau de jeu où tous les joueurs sont en [0, 0]
emptyBoard(Board) :- nb_coureurs(NbCoureurs), length(Team, NbCoureurs), maplist(=([0, 0]), Team), countryCount(Count), length(Board, Count), maplist(=(Team), Board).


% Tire un nombre entre val_chance_min et val_chance_max.
valeurCarteChance(Val) :- val_chance_min(Min), val_chance_max(Max), random_between(Min, Max, Val), writeln(Val).

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
tirerCartes(CartesDisponibles, NbCartes, CartesDisponibles, []) :-
    length(CartesDisponibles, Len),
    NbCartes > Len.
tirerCartes(CartesDisponibles, NbCartes, Cartes, NewCartesDisponibles) :-
    length(CartesDisponibles, Len),
    random_permutation(CartesDisponibles, Permutation),
    NbCartes1 is NbCartes + 1,
    Len1 is Len + 1,
    slice(Permutation, 0, NbCartes1, Cartes),
    slice(Permutation, NbCartes, Len1, NewCartesDisponibles).

% ?- slice([a,b,c,d,e], 1, 4, R).
% R = [b, c].
slice(L, From, To, R):-
  length(LFrom, From),
  length([_|LTo], To),
  append(LTo, _, L),
  append(LFrom, R, LTo).

% Récupére les joueurs d'un pays
% findCountry(country, PlayersOfCountry, ListOfAllCountry)
% findCountry(in, out, in)
findCountry(Country, Players, Countries) :- countryIndex(Country, Index), nth1(Index, Countries, Players).

countryIndex(italie, 1).
countryIndex(hollande, 2).
countryIndex(belgique, 3).
countryIndex(allemagne, 4).

% Compte de nombre de pays qui participe à la course
countryCount(Count) :- aggregate_all(count, countryIndex(_, _), Count).

% Ordre des pays
nextCountry(Country, NextCountry) :- countryIndex(Country, Index), NewIndex is Index + 1, countryCount(Count), NewIndex =< Count, countryIndex(NextCountry, Index), !.
nextCountry(Country, NextCountry) :- countryIndex(Country, _), countryIndex(NextCountry, 1), !.


% Sur base d'une liste de joueurs [[p1x, p1y], [p2x, p2y], ...], renvoie l'index du dernier joueur sur le plateau pouvant bouger.
% https://stackoverflow.com/questions/32918211/find-the-max-element-and-its-index-in-a-list-prolog
findLatestPlayer(Players, LatestPlayerI, Board) :-
    latestPlayer(Players, LPlayer, Board),
    canMove(LPlayer, Board),
    nth1(LatestPlayerI, Players, LPlayer).

latestPlayer([HPlayer|LPlayers], LPlayer, Board) :-
    latestPlayer(LPlayers, HPlayer, LPlayer, Board).

% https://stackoverflow.com/a/19810489/10171758
latestPlayer([], Player,Player,_Board).
latestPlayer([[P1x,P1y] | LPlayer], [LPx, LPy], LP, Board) :-
    P1x < LPx, canMove([P1x,P1y],Board ) -> latestPlayer(LPlayer, [P1x,P1y], LP, Board)
    ; latestPlayer(LPlayer, [LPx, LPy], LP, Board).


% Retourne le joueurs le plus en retard sur le plateau pouvant bouger et ses coordonnées.
latestPlayer(Player2I,[P2x, P2y], _Player1I,[P1x, _P1y], Player2I, [P2x,P2y], Board) :-
    P1x > P2x, canMove([P2x,P2y],_Dest, Board).
latestPlayer(Player1I,[P1x, P1y], Player1I,[P1x, P1y], _Player2I, [P2x,_P2y], Board) :-
    P1x < P2x, canMove([P1x,P1y],_Dest, Board).
latestPlayer(Player1I,[P1x, P1y], Player1I,[P1x, P1y], _Player2I, [_P2x,_P2y], _Board).

% Sur base de coordonnées, vérifie s'il peut avancer sans risque.
% TODO: Peut-être vérifier en cas de dépassement?
canMove([Px, Py],[Tx,Ty], Board) :-
    chemin(Px, Py, Tx, Ty),
    not(hasPlayer([Tx, Ty], Board)).

% Vérifie s'il y a un joueur sur les coordonnées entrées.
hasPlayer([Px, Py], [Country|LCountry]) :-
    hasPlayer([Px, Py], LCountry);
    member([Px, Py], Country).
hasPlayer([Px, Py], [Country]) :-
    member([Px, Py], Country).


move([Px,Py], NbSecondes, SecondesRestantes,[Fx, Fy], Board) :-
    NbSecondes > 0,
    canMove([Px, Py], [Tx, Ty], Board),
    NbSecondes1 is NbSecondes - 1,
    move([Tx,Ty], NbSecondes1, SecondesRestantes,[Fx, Fy], Board).
move([Px,Py], NbSecondes, SecondesRestantes,[Fx, Fy], Board) :-
    NbSecondes < 0,
    canMove([Tx, Ty], [Px, Py],  Board),
    NbSecondes1 is NbSecondes + 1,
    move([Tx,Ty], NbSecondes1, SecondesRestantes,[Fx, Fy], Board).
move([Px,Py], 0, 0,[Px,Py], _Board).
move([Px,Py], NbSecondes, NbSecondes,[Px,Py], Board) :- not(canMove([Px,Py], _, Board)).

movePlayer([Px, Py], IPlayer,Country, NbSecondes, Board, NewBoard) :-
    move([Px,Py], NbSecondes, SecondesRestantes,[Fx, Fy], Board),
    (caseChance([Fx,Fy]) -> valeurCarteChance(Val), move([Fx,Fy], Val, _, [Tx, Ty], Board)
    ; move([Fx,Fy], 0, 0, [Tx, Ty], Board)),
    findCountry(Country, Players, Board),
    replace([Tx, Ty], IPlayer, Players, NewPlayers),
    countryIndex(Country,ICountry),
    replace(NewPlayers, ICountry, Board, NewBoard).


% Remplace le IElem dans List par Elem et le renvoit dans NewList
replace(Elem, IElem, List, NewList) :-
    nth1(IElem, List, _, Temp), % Enlève le IElem élément de List qui produit Temp
    nth1(IElem, NewList, Elem, Temp). % "Génére" un tableau NewList de (Temp elements + 1) éléments où Elem sera à la position IElem


