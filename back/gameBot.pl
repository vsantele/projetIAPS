:- use_module(library(random)).
:- use_module('constants.pl').
:- use_module('board.pl').

% Plateau de jeu où tous les joueurs sont en [0, 0]
emptyPlayersPositions(PlayersPositions) :- nb_coureurs(NbCoureurs), length(Team, NbCoureurs), maplist(=([0, 0]), Team), countryCount(Count), length(PlayersPositions, Count), maplist(=(Team), PlayersPositions).


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
initCards(Cartes) :- val_min_carte(Min), val_max_carte(Max), nb_repetition_cartes(NbRepet),
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
nextCountry(Country, NextCountry) :- countryIndex(Country, Index), NewIndex is Index + 1, countryCount(Count), NewIndex =< Count, countryIndex(NextCountry, NewIndex), !.
nextCountry(Country, NextCountry) :- countryIndex(Country, _), countryIndex(NextCountry, 1), !.


% Sur base d'une liste de joueurs [[p1x, p1y], [p2x, p2y], ...], renvoie l'index du dernier joueur sur le plateau pouvant bouger.
% https://stackoverflow.com/questions/32918211/find-the-max-element-and-its-index-in-a-list-prolog
findLatestPlayer(Players, LatestPlayerI, PlayersPositions) :-
    latestPlayer(Players, LPlayer, PlayersPositions),
    canMove(LPlayer,_DestCoord, PlayersPositions),
    nth1(LatestPlayerI, Players, LPlayer).

latestPlayer([HPlayer|LPlayers], LPlayer, PlayersPositions) :-
    canMove(HPlayer,_Dest, PlayersPositions) -> latestPlayer(LPlayers, HPlayer, LPlayer, PlayersPositions)
    ; latestPlayer(LPlayers, LPlayer, PlayersPositions).

% https://stackoverflow.com/a/19810489/10171758
latestPlayer([], Player,Player,_PlayersPositions).
latestPlayer([[P1x,P1y] | LPlayer], [LPx, LPy], LP, PlayersPositions) :-
    P1x < LPx, canMove([P1x,P1y],_Dest, PlayersPositions ) -> latestPlayer(LPlayer, [P1x,P1y], LP, PlayersPositions)
    ; latestPlayer(LPlayer, [LPx, LPy], LP, PlayersPositions).


% Retourne le joueurs le plus en retard sur le plateau pouvant bouger et ses coordonnées.
latestPlayer(Player2I,[P2x, P2y], _Player1I,[P1x, _P1y], Player2I, [P2x,P2y], PlayersPositions) :-
    P1x > P2x, canMove([P2x,P2y],_Dest, PlayersPositions).
latestPlayer(Player1I,[P1x, P1y], Player1I,[P1x, P1y], _Player2I, [P2x,_P2y], PlayersPositions) :-
    P1x < P2x, canMove([P1x,P1y],_Dest, PlayersPositions).
latestPlayer(Player1I,[P1x, P1y], Player1I,[P1x, P1y], _Player2I, [_P2x,_P2y], _PlayersPositions).

% Sur base de coordonnées, vérifie s'il peut avancer sans risque.
% TODO: Peut-être vérifier en cas de dépassement?
canMove([Px, Py],[Tx,Ty], _PlayersPositions) :-
    chemin(Px, Py, Tx, Ty),
    caseFin(Tx).
canMove([Px, Py],[Tx,Ty], PlayersPositions) :-
    chemin(Px, Py, Tx, Ty),
    not(hasPlayer([Tx, Ty], PlayersPositions)).

canMoveBackward([Px, Py],[Tx,Ty], PlayersPositions) :-
    chemin(Tx, Ty, Px, Py),
    not(hasPlayer([Tx, Ty], PlayersPositions)).


% Vérifie s'il y a un joueur sur les coordonnées entrées.
hasPlayer([Px, Py], [Country|LCountry]) :-
    hasPlayer([Px, Py], LCountry);
    member([Px, Py], Country).
hasPlayer([Px, Py], [Country]) :-
    member([Px, Py], Country).


move([Px,Py], NbSecondes, SecondesRestantes,[Fx, Fy], PlayersPositions) :-
    NbSecondes > 0,
    canMove([Px, Py], [Tx, Ty], PlayersPositions),
    NbSecondes1 is NbSecondes - 1,
    move([Tx,Ty], NbSecondes1, SecondesRestantes,[Fx, Fy], PlayersPositions).
move([Px,Py], NbSecondes, SecondesRestantes,[Fx, Fy], PlayersPositions) :-
    NbSecondes < 0,
    canMoveBackward([Px, Py], [Tx, Ty], PlayersPositions),
    NbSecondes1 is NbSecondes + 1,
    move([Tx,Ty], NbSecondes1, SecondesRestantes,[Fx, Fy], PlayersPositions).
move([Px,Py], 0, 0,[Px,Py], _PlayersPositions).
move([Px,Py], NbSecondes, NbSecondes,[Px,Py], PlayersPositions) :- NbSecondes > 0, not(canMove([Px,Py], _, PlayersPositions)).
move([Px,Py], NbSecondes, NbSecondes,[Px,Py], PlayersPositions) :- not(canMoveBackward([Px,Py], _, PlayersPositions)).

movePlayer([Px, Py], IPlayer,Country, NbSecondes, PlayersPositions, NewPlayersPositions) :-
    move([Px,Py], NbSecondes, SecondesRestantes,[Fx, Fy], PlayersPositions),
    (caseChance([Fx,Fy]) -> valeurCarteChance(Val), move([Fx,Fy], Val, _, [Tx, Ty], PlayersPositions)
    ; move([Fx,Fy], 0, 0, [Tx, Ty], PlayersPositions)),
    findCountry(Country, Players, PlayersPositions),
    replace([Tx, Ty], IPlayer, Players, NewPlayers),
    countryIndex(Country,ICountry),
    replace(NewPlayers, ICountry, PlayersPositions, NewPlayersPositions).


% Remplace le IElem dans List par Elem et le renvoit dans NewList
replace(Elem, IElem, List, NewList) :-
    nth1(IElem, List, _, Temp), % Enlève le IElem élément de List qui produit Temp
    nth1(IElem, NewList, Elem, Temp). % "Génére" un tableau NewList de (Temp elements + 1) éléments où Elem sera à la position IElem

% Trouver le pays qui a la plus grande carte seconde
biggestCard([], Country) :- countryIndex(Country, 1).
biggestCard(List, Country) :-
    flatten(List, FlatList), % transforme [[1, 2, 3], [4, 5, 6]] -> [1, 2, 3, 4, 5, 6]
    max_list(FlatList, Max),
    nth1(Index, List, SubList),
    member(Max, SubList),
    countryIndex(Country, Index), !.

% Etat intial de la partie
initGame([CurrentCountry, PlayersPositions, CountriesCards, NewCards]) :-
    emptyPlayersPositions(PlayersPositions),
    initCards(Cards),
    initCountriesCards(CountriesCards, Cards, NewCards),
    biggestCard(CountriesCards, CurrentCountry).

% Tire 5 cartes
%   Cards : cartes disponibles pour le tirage (IN)
%   NewCards : cartes restantes (OUT)
%   CountryCards : cartes tirées (OUT)
initCountryCards(CountryCards, Cards, NewCards) :-
    tirerCartes(Cards,5, CountryCards, NewCards).

% Tires 5 cartes au hasard pour chaque pays
%   CountriesCard : tableau de cartes pour chaque pays (OUT)
%   Cards : cartes disponibles pour le tirage (IN)
%   NewCards : cartes restantes après le tirage (OUT)
initCountriesCards(CountriesCards, Cards, NewCards) :-
    countryCount(Count),
    initCountriesCards(CountriesCards, Cards, NewCards, Count).

initCountriesCards([], Cards, Cards, 0) :- !.
initCountriesCards([CountryCards|LCountriesCards], Cards, NewCards, Count) :-
    initCountryCards(CountryCards, Cards, NewCards1),
    Count1 is Count - 1,
    initCountriesCards(LCountriesCards, NewCards1, NewCards, Count1).

checkCountryCards([], NewCountryCards, Cards, NewCards) :-
    initCountryCards(NewCountryCards, Cards, NewCards).

checkCountryCards(CountryCards, CountryCards, Cards, Cards).

% Tire une carte au hasard dans une liste
%   Cards : cartes disponibles (IN)
%   CardPicked : carte tirée (OUT)
%   NewCards : cartes restantes (OUT)
pickCard(Cards, 0, CardPicked, NewCards, State) :-
    pickBestCard(State, CardPicked),
    select(CardPicked, Cards, NewCards).
pickCard(Cards, SelectedCard, SelectedCard, NewCards, _) :-
    select(SelectedCard, Cards, NewCards).

% Supprime une occurence dans List des éléments de [E|Es]
% et renvoi la liste finale dans newList
removeDuplicates(List, [], List).
removeDuplicates(List, [E|Es], NewList) :-
    select(E, List, ListOut),
    removeDuplicates(ListOut, Es, NewList).
removeDuplicates(List, [_|Es], NewList) :-
    removeDuplicates(List, Es, NewList).

% Génére une nouvelle pile de carte : initCards \ {cartes possédées par les joueurs}
% Pour tester : fillCards([1,1,2,2,3,4,5,6,7,8,9,10],[[1,2,3],[5, 10, 28],[]],R).
fillCards(CountriesCards, NewCards) :-
    initCards(DefaultCards),
    fillCards(DefaultCards, CountriesCards, NewCards).

fillCards([], _, []) :- !.
fillCards(Cards, [], Cards) :- !.
fillCards(Cards, [CountryCards|CountriesCards], CardsStack) :-
    removeDuplicates(Cards, CountryCards, CardsOut),
    fillCards(CardsOut, CountriesCards, CardsStack), !.

% La pile de cartes disponibles < nb_cartesSecondeTirer(X) (besoin de recréer une pile de cartes)
play([CurrentCountry, PlayersPositions, CountriesCards, Cards, SelectedCard], StateOut) :-
    length(Cards, NbCards),
    nb_cartesSecondeTirer(NbCardsPick),
    NbCards < NbCardsPick,
    fillCards(CountriesCards, NewCards),
    play([CurrentCountry, PlayersPositions, CountriesCards, NewCards, SelectedCard], StateOut).

% Game state : [CurrentCountry, PlayersPositions, CountriesCards, Cards, SelectedCard]
play([CurrentCountry, PlayersPositions, CountriesCards, Cards, SelectedCard], [NextCountry, NewPlayersPositions, NewCountriesCards, NewCards, Card]) :-
    countryIndex(CurrentCountry, ICurrentCountry),
    findCountry(CurrentCountry, Players, PlayersPositions), % Sélectionne les joueurs du CurrentCountry
    findLatestPlayer(Players, LatestPlayerI, PlayersPositions),
    nth1(LatestPlayerI, Players, [Px, Py]),
    findCountry(CurrentCountry, CountryCards, CountriesCards), % sélectionne les cartes du CurrentCountry
    pickCard(CountryCards, SelectedCard, Card, CountryCards1, [CurrentCountry, PlayersPositions, CountriesCards, Cards, SelectedCard]),
    checkCountryCards(CountryCards1, NewCountryCards, Cards, NewCards), % Vérifie si il reste des cartes pour le CurrentCountry
    replace(NewCountryCards, ICurrentCountry, CountriesCards, NewCountriesCards), % replaceNewCountryCards à l'index ICurrentCountry dans la liste CountriesCards par la valeur NewPlayersPositions
    movePlayer([Px, Py], LatestPlayerI, CurrentCountry, Card, PlayersPositions, NewPlayersPositions),
    nextCountry(CurrentCountry, NextCountry), !.

% Aucun joueur ne peut jouer dans le CurrentCountry, on passe donc au NextCountry
play([CurrentCountry, PlayersPositions, CountriesCards, Cards, Card], [NextCountry, PlayersPositions, CountriesCards, Cards, Card]) :-
    nextCountry(CurrentCountry, NextCountry).



gameLoop(S, SOut) :-
    gameOver(S),
    SOut = S.

gameLoop(S, SOut) :-
    play(S, S1),
    gameLoop(S1, SOut).


testGame(StateOut) :-
    initGame(State),
    gameLoop(State, StateOut).


gameOver([_Country, _PlayersPositions, _CountriesCards, Cards, _SelectedCard]) :- gameOverCards(Cards).
gameOver([_Country, PlayersPositions, _CountriesCards, _Cards, _SelectedCard]) :- gameOverPlayer(PlayersPositions).

gameOverCards([]).

% return true if all players are in [1000,0]
gameOverPlayer([]).

gameOverPlayer([Country|PlayersPositions]) :-
    caseFin(EndCase),
    maplist(=([EndCase,_]), Country),
    gameOver(PlayersPositions).

% ================ AI ==================

heuristics(OldState, NewState, Country, Heuristic) :-
    [_, OldPlayersPos, _, _, _] = OldState,
    findCountry(Country, OldCountryPlayersPos, OldPlayersPos),
    findall(Score, (
        member(Player, OldCountryPlayersPos),
        nth1(IPlayer, OldCountryPlayersPos, Player),
        heuristics(OldState, NewState, IPlayer, Country, Score)
        )
    , Scores),
    sum_list(Scores, Heuristic).

heuristics([_OC, OldPlayersPos, _OCC, _OCa, _OSC ], [_NC, NewPlayersPos, _NCC, _NCa, _NSC ], IPlayer, Country, Heuristic) :-
    findCountry(Country, OldCountryPlayersPos, OldPlayersPos),
    nth1(IPlayer, OldCountryPlayersPos, [OX1, OY1]),
    findCountry(Country, NewCountryPlayersPos, NewPlayersPos),
    nth1(IPlayer, NewCountryPlayersPos, [NX1, NY1]),
    heuristicEnd(NX1, HeuristicEnd),
    distance([OX1, OY1], [NX1, NY1], HeuristicDistance),
    Heuristic is HeuristicDistance + HeuristicEnd.

heuristicEnd(X, Heuristic) :-
    caseFin(X),
    Heuristic is 1000, !.
heuristicEnd(_, 0).

distance([X1, Y1], [X2, Y2], D) :-
    D is truncate(X2/10) - truncate(X1/10).

% MINIMAX

% findall(X,countryIndex(X,_),R).


removeDuplicates(List, Result) :-
    sort(List, Result).

pickBestCard(State, BestCard) :-
    minMax(State,State, 8, _,_, BestCard, _).

minMax(StateInit, [CurrentCountry, PlayersPositions, CountriesCards, Cards,_], Depth, Alpha, Beta, BestMove, BestScores ) :-
    Depth > 0,
    findCountry(CurrentCountry, Players, PlayersPositions),
    findCountry(CurrentCountry, CountryCards, CountriesCards),
    removeDuplicates(CountryCards, CountryCardsOut),
    (
        findLatestPlayer(Players, LatestPlayerI, PlayersPositions) ->
        findall([Move, Scores], (
            member(SelectedCard, CountryCardsOut),
            play([CurrentCountry, PlayersPositions, CountriesCards, Cards, SelectedCard], StateOut),
            [Country,_,_,_,Move] = StateOut,
            minMax(StateInit, StateOut, Depth-1, _,_,_, Scores)
        ),Moves)
        ;  nextCountry(CurrentCountry, Country), minMax(StateInit, [Country, PlayersPositions, CountriesCards, Cards,_], Depth-1, _,_,_, Scores), Moves = [[0, Scores]]
    ),
    % trace,
    bestMove(Moves, CurrentCountry, Depth, Alpha, Beta, BestMove, BestScores), !.

minMax(StateInit, State, Depth, _, _, _, Scores) :-
    findall(Score, (
        countryIndex(Country, _),
        heuristics(StateInit, State, Country, Score)
     ), Scores).

bestMove([[Move, Scores]], _, _, _, _, Move, Scores) :- !.

bestMove([[Move, Scores]|Moves], Country, Depth, Alpha, Beta, BestMove, BestScores) :-
    bestMove(Moves, Country, Depth, Alpha, Beta, Move1, Scores1),
    betterOf(Move, Scores, Move1, Scores1, Country, Depth, Alpha, Beta, BestMove, BestScores).

betterOf(_,Scores1, Move2, Scores2, Country, Depth, Alpha, Beta, Move2, Scores2) :-
    findCountry(Country, Score1, Scores1),
    findCountry(Country, Score2, Scores2),
    Score1 < Score2, !.

betterOf(Move1,Scores1, Move2, Scores2, Country, Depth, Alpha, Beta, Move1, Scores1) :- !.

bestMoveCountry([[Move, Score]], _, Move, Score).

bestMoveCountry([[Move, Score]|Moves], Depth, BestMove, BestScore) :-
    bestMoveCountry(Moves, Depth, Move1, Score1),
    betterOfCountry(Move, Score, Move1, Score1,BestMove, BestScore).
bestMoveCountry([[]|Moves], _, BestMove, BestScore) :-
    bestMoveCountry(Moves, _, BestMove, BestScore).
bestMoveCountry([[]], _, -1, -10).


betterOfCountry(_,Score1, Move2, Score2, Move2, Score2) :-
    Score1 < Score2, !.
betterOfCountry(Move1,Score1, _, _, Move1, Score1) :- !.

% minMax(State, IPlayer, Depth, Alpha, Beta, BestMove, BestScore)
% minMax(State, IPlayer,Country, Depth, Alpha, Beta, BestMove, BestScore) :-
%     Depth > 0,
%     findall([Move, Score], (move(State, IPlayer,Country, Move, NewState), minMax(NewState, IPlayer,Country, Depth-1, Alpha, Beta, _, Score)), Moves),
%     bestMove(Moves, IPlayer, Depth, Alpha, Beta, BestMove, BestScore), !.

% minMax(State, IPlayer, Country,_, _, _, _, Score) :-
%     heuristics(State, State, IPlayer, Country, Score).

% bestMove([[Move, Score]], _, _, _, _, Move, Score) :- !.

% bestMove([[Move, Score]|Moves], IPlayer, Depth, Alpha, Beta, BestMove, BestScore) :-
%     bestMove(Moves, IPlayer, Depth, Alpha, Beta, Move1, Score1),
%     betterOf(Move, Score, Move1, Score1, IPlayer, Depth, Alpha, Beta, BestMove, BestScore).

% betterOf(Move0, Score0, _, Score1, IPlayer, Depth, _, _, Move0, Score0) :-
%     minToMove(IPlayer, Depth),
%     Score0 > Score1, !.

% betterOf(Move0, Score0, _, Score1, IPlayer, Depth, _, _, Move0, Score0) :-
%     maxToMove(IPlayer, Depth),
%     Score0 < Score1, !.

% betterOf(_, _, Move1, Score1, IPlayer, Depth, Alpha, Beta, Move1, Score1) :-
%     minToMove(IPlayer, Depth),
%     Score1 > Alpha,
%     Score1 < Beta, !.

% betterOf(_, _, Move1, Score1, IPlayer, Depth, Alpha, Beta, Move1, Score1) :-
%     maxToMove(IPlayer, Depth),
%     Score1 < Beta,
%     Score1 > Alpha, !.

% betterOf(Move0, Score0, _, Score1, IPlayer, Depth, Alpha, Beta, Move0, Score0) :-
%     minToMove(IPlayer, Depth),
%     Score0 > Alpha,
%     Score0 > Beta, !.

% betterOf(Move0, Score0, _, Score1, IPlayer, Depth, Alpha, Beta, Move0, Score0) :-
%     maxToMove(IPlayer, Depth),
%     Score0 < Beta,
%     Score0 < Alpha, !.

% betterOf(_, _, Move1, Score1, IPlayer, Depth, Alpha, Beta, Move1, Score1) :-
%     minToMove(IPlayer, Depth),
%     Score1 > Alpha,
%     Score1 > Beta, !.

% betterOf(_, _, Move1, Score1, IPlayer, Depth, Alpha, Beta, Move1, Score1) :-
%     maxToMove(IPlayer, Depth),
%     Score1 < Beta,
%     Score1 < Alpha, !.

% betterOf(Move0, Score0, _, Score1, IPlayer, Depth, Alpha, Beta, Move0, Score0) :-
%     minToMove(IPlayer, Depth),
%     Score0 > Alpha,
%     Score0 < Beta, !.

% betterOf(Move0, Score0, _, Score1, IPlayer, Depth, Alpha, Beta, Move0, Score0) :-
%     maxToMove(IPlayer, Depth),
%     Score0 < Beta,
%     Score0 > Alpha, !.

% minToMove(IPlayer, Depth) :-
%     Depth mod 2 =:= 0,
%     IPlayer = 1, !.

% maxToMove(IPlayer, Depth) :-
%     Depth mod 2 =:= 0,
%     IPlayer = 2, !.

% minToMove(IPlayer, Depth) :-
%     Depth mod 2 =:= 1,
%     IPlayer = 2, !.

% maxToMove(IPlayer, Depth) :-
%     Depth mod 2 =:= 1,
%     IPlayer = 1, !.
