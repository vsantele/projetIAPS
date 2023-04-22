:- use_module('constants.pl').


% Format: [Numéro * 10 + (A=1 , B=2 , C=3, D=4, 0), Position par rapport au numéro (numéro = 1, puis 2, etc)]
% Si la route est coupé par des arbres, l'arbre est considéré comme une position. Exemple case 23, les positions valident sont 1, 2 et 4. 3 étant un arbres.

casesEchange([[130,2] ,[140,2], [180,2], [200,2], [200,3], [220,3], [260,4], [274, 4], [274,4], [300,4], [320,4], [360,2], [460,1], [460,2], [610,2], [620, 2], [710,2], [720,2], [810,1], [810,2], [850,1], [850,3], [860,3], [892, 2], [902,3]]).

casesChances([[91,1], [101,1], [110,1], [120,1], [150,2], [160,2], [190,3], [210,3], [240,1], [261, 1], [280,1], [300,1], [320,1], [340,1], [480,2], [570,2], [660,1], [660,2], [740,1], [903,3]]).

sprint(1, 280, [1,1], [0,0]).
sprint(2, 360, [4,3], [1,1]).
sprint(3, 760,[ 4,2], [0,0]).

