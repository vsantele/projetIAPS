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
    canMove([Tx, Ty], [Px, Py],  PlayersPositions),
    NbSecondes1 is NbSecondes + 1,
    move([Tx,Ty], NbSecondes1, SecondesRestantes,[Fx, Fy], PlayersPositions).
move([Px,Py], 0, 0,[Px,Py], _PlayersPositions).
move([Px,Py], NbSecondes, NbSecondes,[Px,Py], PlayersPositions) :- not(canMove([Px,Py], _, PlayersPositions)).

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

% Etat intial de la partie
initGame([CurrentCountry, PlayersPositions, CountryCards, NewCards]) :-
    emptyPlayersPositions(PlayersPositions),
    countryIndex(CurrentCountry, 1),
    initCards(Cards),
    initCountriesCards(CountryCards, Cards, NewCards).

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
pickCard(Cards, CardPicked, NewCards) :-
    tirerCartes(Cards, 1, [CardPicked|_], NewCards).

fillCards(CountriesCards, NewCards) :-
    initCards(NewCards1),
    % TODO: remove duplicates
    NewCards = NewCards1.

% La pile de cartes disponibles < nb_cartesSecondeTirer(X) (besoin de recréer une pile de cartes)
play([CurrentCountry, PlayersPositions, CountriesCards, Cards], StateOut) :-
    length(Cards, NbCards),
    nb_cartesSecondeTirer(NbCardsPick),
    NbCards < NbCardsPick,
    fillCards(CountriesCards, NewCards),
    play([CurrentCountry, PlayersPositions, CountriesCards, NewCards], StateOut).

% Game state : [CurrentCountry, PlayersPositions, CountriesCards, Cards]
play([CurrentCountry, PlayersPositions, CountriesCards, Cards], [NextCountry, NewPlayersPositions, NewCountriesCards, NewCards]) :-
    countryIndex(CurrentCountry, ICurrentCountry),
    findCountry(CurrentCountry, Players, PlayersPositions), % Sélectionne les joueurs du CurrentCountry
    findLatestPlayer(Players, LatestPlayerI, PlayersPositions),
    nth1(LatestPlayerI, Players, [Px, Py]),
    findCountry(CurrentCountry, CountryCards, CountriesCards), % sélectionne les cartes du CurrentCountry
    pickCard(CountryCards, Card, CountryCards1),
    checkCountryCards(CountryCards1, NewCountryCards, Cards, NewCards), % Vérifie si il reste des cartes pour le CurrentCountry
    replace(NewCountryCards, ICurrentCountry, CountriesCards, NewCountriesCards), % replaceNewCountryCards à l'index ICurrentCountry dans la liste CountriesCards par la valeur NewPlayersPositions
    movePlayer([Px, Py], LatestPlayerI, CurrentCountry, 1, PlayersPositions, NewPlayersPositions),
    nextCountry(CurrentCountry, NextCountry).

% Aucun joueur ne peut jouer dans le CurrentCountry, on passe donc au NextCountry
play([CurrentCountry, PlayersPositions, CountriesCards, Cards], [NextCountry, PlayersPositions, CountriesCards, Cards]) :-
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


gameOver([_Country, _PlayersPositions, _CountriesCards, Cards]) :- gameOverCards(Cards).
gameOver([_Country, PlayersPositions, _CountriesCards, _Cards]) :- gameOverPlayer(PlayersPositions).

gameOverCards([]).

% return true if all players are in [1000,0]
gameOverPlayer([]).

gameOverPlayer([Country|PlayersPositions]) :-
    caseFin(EndCase),
    maplist(=([EndCase,_]), Country),
    gameOver(PlayersPositions).

