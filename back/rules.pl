:- use_module('constants.pl').


% Format: Numéro * 10 + (A=1 , B=2 , C=3, D=4, 0), Position par rapport au numéro (numéro = 1, puis 2, etc)]
% Si la route est coupé par des arbres, l'arbre est considéré comme une position. Exemple case 23, les positions valident sont 1, 2 et 4. 3 étant un arbres.
caseChance(91, 1).
caseChance(101, 1).
caseChance(110, 1).
caseChance(120, 1).
caseChance(150, 2).
caseChance(160, 2).
caseChance(190, 3).
caseChance(210, 3).
caseChance(240, 1).
caseChance(261, 1).
caseChance(280, 1).
caseChance(300, 1).
caseChance(320, 1).
caseChance(340, 1).
caseChance(480, 2).
caseChance(570, 2).
caseChance(660, 1).
caseChance(660, 2).
caseChance(740, 1).
caseChance(903, 3).

sprint(1, 280, [1,1], [0,0]).
sprint(2, 360, [4,3], [1,1]).
sprint(3, 760,[ 4,2], [0,0]).

% equipe(nom, ordre, isHuman, [positionJ1, PositionJ2, PositionJ3]).
equipe(italie, 1,0, [[0,0],[0,0],[0,0]]).
equipe(hollande, 2,1, [[0,0],[0,0],[0,0]]).
equipe(belgique, 3,0, [[0,0],[0,0],[0,0]]).
equipe(allemagne, 4,1, [[0,0],[0,0],[0,0]]).

