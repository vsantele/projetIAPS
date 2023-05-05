:- module(board, [chemin/4,voisin/4, caseChance/1 ]).
% chemin(X1, Y1, X2, Y2) où X est le numéro de la case * 10 + (A=1,B=2,C=3) et Y la position par rapport à la case numéro. Exemple: 10, 1, 20, 1 signifie que la case 1 à la position 1 est reliée à la case 2 position 2.

:- discontiguous chemin/4, voisin/4.

chemin(10, 1, 20, 1).
chemin(10, 1, 20, 2).
voisin(10, 1, 10, 2).
chemin(10, 2, 20, 2).
chemin(10, 2, 20, 3).
voisin(10, 2, 10, 3).
chemin(10, 2, 20, 1).
chemin(10, 3, 20, 3).
chemin(10, 3, 20, 2).
chemin(20, 1, 30, 1).
chemin(20, 1, 30, 2).
voisin(20, 1, 20, 2).
chemin(20, 2, 30, 2).
chemin(20, 2, 30, 3).
voisin(20, 2, 20, 3).
chemin(20, 2, 30, 1).
chemin(20, 3, 30, 3).
chemin(20, 3, 30, 2).
chemin(30, 1, 40, 1).
chemin(30, 1, 40, 2).
voisin(30, 1, 30, 2).
chemin(30, 2, 40, 2).
chemin(30, 2, 40, 3).
voisin(30, 2, 30, 3).
chemin(30, 2, 40, 1).
chemin(30, 3, 40, 3).
chemin(30, 3, 40, 2).
chemin(40, 1, 50, 1).
chemin(40, 1, 50, 2).
voisin(40, 1, 40, 2).
chemin(40, 2, 50, 2).
chemin(40, 2, 50, 3).
voisin(40, 2, 40, 3).
chemin(40, 2, 50, 1).
chemin(40, 3, 50, 3).
chemin(40, 3, 50, 2).
chemin(50, 1, 60, 1).
chemin(50, 1, 60, 2).
voisin(50, 1, 50, 2).
chemin(50, 2, 60, 2).
chemin(50, 2, 60, 3).
voisin(50, 2, 50, 3).
chemin(50, 2, 60, 1).
chemin(50, 3, 60, 3).
chemin(50, 3, 60, 2).
chemin(60, 1, 70, 1).
chemin(60, 1, 70, 2).
voisin(60, 1, 60, 2).
chemin(60, 2, 70, 2).
chemin(60, 2, 70, 3).
voisin(60, 2, 60, 3).
chemin(60, 2, 70, 1).
chemin(60, 3, 70, 3).
chemin(60, 3, 70, 2).
chemin(70, 1, 80, 1).
chemin(70, 1, 80, 2).
voisin(70, 1, 70, 2).
chemin(70, 2, 80, 2).
chemin(70, 2, 80, 3).
voisin(70, 2, 70, 3).
chemin(70, 2, 80, 1).
chemin(70, 3, 80, 3).
chemin(70, 3, 80, 2).
chemin(110, 1, 120, 1).
chemin(110, 1, 120, 2).
voisin(110, 1, 110, 2).
chemin(110, 2, 120, 2).
chemin(110, 2, 120, 1).
chemin(120, 1, 130, 1).
chemin(120, 1, 130, 2).
voisin(120, 1, 120, 2).
chemin(120, 2, 130, 2).
chemin(120, 2, 130, 1).
chemin(130, 1, 140, 1).
chemin(130, 1, 140, 2).
voisin(130, 1, 130, 2).
chemin(130, 2, 140, 2).
chemin(130, 2, 140, 1).
chemin(140, 1, 150, 1).
chemin(140, 1, 150, 2).
voisin(140, 1, 140, 2).
chemin(140, 2, 150, 2).
chemin(140, 2, 150, 1).
chemin(150, 1, 160, 1).
chemin(150, 1, 160, 2).
voisin(150, 1, 150, 2).
chemin(150, 2, 160, 2).
chemin(150, 2, 160, 1).
chemin(160, 1, 170, 1).
chemin(160, 1, 170, 2).
voisin(160, 1, 160, 2).
chemin(160, 2, 170, 2).
chemin(160, 2, 170, 1).
chemin(170, 1, 180, 1).
chemin(170, 1, 180, 2).
voisin(170, 1, 170, 2).
chemin(170, 2, 180, 2).
chemin(170, 2, 180, 1).
chemin(190, 1, 200, 1).
chemin(190, 1, 200, 2).
voisin(190, 1, 190, 2).
chemin(190, 2, 200, 2).
chemin(190, 2, 200, 3).
voisin(190, 2, 190, 3).
chemin(190, 2, 200, 1).
chemin(190, 3, 200, 3).
chemin(190, 3, 200, 2).
chemin(200, 1, 210, 1).
chemin(200, 1, 210, 2).
voisin(200, 1, 200, 2).
chemin(200, 2, 210, 2).
chemin(200, 2, 210, 3).
voisin(200, 2, 200, 3).
chemin(200, 2, 210, 1).
chemin(200, 3, 210, 3).
chemin(200, 3, 210, 2).
chemin(210, 1, 220, 1).
chemin(210, 1, 220, 2).
voisin(210, 1, 210, 2).
chemin(210, 2, 220, 2).
chemin(210, 2, 220, 3).
voisin(210, 2, 210, 3).
chemin(210, 2, 220, 1).
chemin(210, 3, 220, 3).
chemin(210, 3, 220, 2).
chemin(230, 1, 240, 1).
chemin(230, 1, 240, 2).
voisin(230, 1, 230, 2).
chemin(230, 2, 240, 2).
chemin(230, 2, 240, 1).
chemin(230, 4, 240, 4).
chemin(240, 1, 250, 1).
chemin(240, 1, 250, 2).
voisin(240, 1, 240, 2).
chemin(240, 2, 250, 2).
chemin(240, 2, 250, 1).
chemin(240, 4, 250, 4).
chemin(280, 1, 290, 1).
chemin(280, 1, 290, 2).
voisin(280, 1, 280, 2).
chemin(280, 2, 290, 2).
chemin(280, 2, 290, 1).
chemin(280, 4, 290, 4).
chemin(290, 1, 300, 1).
chemin(290, 1, 300, 2).
voisin(290, 1, 290, 2).
chemin(290, 2, 300, 2).
chemin(290, 2, 300, 1).
chemin(290, 4, 300, 4).
chemin(300, 1, 310, 1).
chemin(300, 1, 310, 2).
voisin(300, 1, 300, 2).
chemin(300, 2, 310, 2).
chemin(300, 2, 310, 1).
chemin(300, 4, 310, 4).
chemin(310, 1, 320, 1).
chemin(310, 1, 320, 2).
voisin(310, 1, 310, 2).
chemin(310, 2, 320, 2).
chemin(310, 2, 320, 1).
chemin(310, 4, 320, 4).
chemin(320, 1, 330, 1).
chemin(320, 1, 330, 2).
voisin(320, 1, 320, 2).
chemin(320, 2, 330, 2).
chemin(320, 2, 330, 1).
chemin(320, 4, 330, 4).
chemin(330, 1, 340, 1).
chemin(330, 1, 340, 2).
voisin(330, 1, 330, 2).
chemin(330, 2, 340, 2).
chemin(330, 2, 340, 1).
chemin(330, 4, 340, 4).
chemin(340, 1, 350, 1).
chemin(340, 1, 350, 2).
voisin(340, 1, 340, 2).
chemin(340, 2, 350, 2).
chemin(340, 2, 350, 1).
chemin(360, 1, 370, 1).
chemin(360, 1, 370, 2).
voisin(360, 1, 360, 2).
chemin(360, 2, 370, 2).
chemin(360, 2, 370, 1).
chemin(370, 1, 380, 1).
chemin(370, 1, 380, 2).
voisin(370, 1, 370, 2).
chemin(370, 2, 380, 2).
chemin(370, 2, 380, 1).
chemin(380, 1, 390, 1).
chemin(380, 1, 390, 2).
voisin(380, 1, 380, 2).
chemin(380, 2, 390, 2).
chemin(380, 2, 390, 1).
chemin(390, 1, 400, 1).
chemin(390, 1, 400, 2).
voisin(390, 1, 390, 2).
chemin(390, 2, 400, 2).
chemin(390, 2, 400, 1).
chemin(400, 1, 410, 1).
chemin(400, 1, 410, 2).
voisin(400, 1, 400, 2).
chemin(400, 2, 410, 2).
chemin(400, 2, 410, 1).
chemin(410, 1, 420, 1).
chemin(410, 1, 420, 2).
voisin(410, 1, 410, 2).
chemin(410, 2, 420, 2).
chemin(410, 2, 420, 1).
chemin(420, 1, 430, 1).
chemin(420, 1, 430, 2).
voisin(420, 1, 420, 2).
chemin(420, 2, 430, 2).
chemin(420, 2, 430, 1).
chemin(430, 1, 440, 1).
chemin(430, 1, 440, 2).
voisin(430, 1, 430, 2).
chemin(430, 2, 440, 2).
chemin(430, 2, 440, 1).
chemin(440, 1, 450, 1).
chemin(440, 1, 450, 2).
voisin(440, 1, 440, 2).
chemin(440, 2, 450, 2).
chemin(440, 2, 450, 1).
chemin(450, 1, 460, 1).
chemin(450, 1, 460, 2).
voisin(450, 1, 450, 2).
chemin(450, 2, 460, 2).
chemin(450, 2, 460, 1).
chemin(460, 1, 470, 1).
chemin(460, 1, 470, 2).
voisin(460, 1, 460, 2).
chemin(460, 2, 470, 2).
chemin(460, 2, 470, 1).
chemin(470, 1, 480, 1).
chemin(470, 1, 480, 2).
voisin(470, 1, 470, 2).
chemin(470, 2, 480, 2).
chemin(470, 2, 480, 1).
chemin(480, 1, 490, 1).
chemin(480, 1, 490, 2).
voisin(480, 1, 480, 2).
chemin(480, 2, 490, 2).
chemin(480, 2, 490, 1).
chemin(490, 1, 500, 1).
chemin(490, 1, 500, 2).
voisin(490, 1, 490, 2).
chemin(490, 2, 500, 2).
chemin(490, 2, 500, 1).
chemin(500, 1, 510, 1).
chemin(500, 1, 510, 2).
voisin(500, 1, 500, 2).
chemin(500, 2, 510, 2).
chemin(500, 2, 510, 1).
chemin(510, 1, 520, 1).
chemin(510, 1, 520, 2).
voisin(510, 1, 510, 2).
chemin(510, 2, 520, 2).
chemin(510, 2, 520, 1).
chemin(520, 1, 530, 1).
chemin(520, 1, 530, 2).
voisin(520, 1, 520, 2).
chemin(520, 2, 530, 2).
chemin(520, 2, 530, 1).
chemin(530, 1, 540, 1).
chemin(530, 1, 540, 2).
voisin(530, 1, 530, 2).
chemin(530, 2, 540, 2).
chemin(530, 2, 540, 1).
chemin(540, 1, 550, 1).
chemin(540, 1, 550, 2).
voisin(540, 1, 540, 2).
chemin(540, 2, 550, 2).
chemin(540, 2, 550, 1).
chemin(550, 1, 560, 1).
chemin(550, 1, 560, 2).
voisin(550, 1, 550, 2).
chemin(550, 2, 560, 2).
chemin(550, 2, 560, 1).
chemin(560, 1, 570, 1).
chemin(560, 1, 570, 2).
voisin(560, 1, 560, 2).
chemin(560, 2, 570, 2).
chemin(560, 2, 570, 1).
chemin(570, 1, 580, 1).
chemin(570, 1, 580, 2).
voisin(570, 1, 570, 2).
chemin(570, 2, 580, 2).
chemin(570, 2, 580, 1).
chemin(580, 1, 590, 1).
chemin(580, 1, 590, 2).
voisin(580, 1, 580, 2).
chemin(580, 2, 590, 2).
chemin(580, 2, 590, 1).
chemin(590, 1, 600, 1).
chemin(590, 1, 600, 2).
voisin(590, 1, 590, 2).
chemin(590, 2, 600, 2).
chemin(590, 2, 600, 1).
chemin(600, 1, 610, 1).
chemin(600, 1, 610, 2).
voisin(600, 1, 600, 2).
chemin(600, 2, 610, 2).
chemin(600, 2, 610, 1).
chemin(610, 1, 620, 1).
chemin(610, 1, 620, 2).
voisin(610, 1, 610, 2).
chemin(610, 2, 620, 2).
chemin(610, 2, 620, 1).
chemin(650, 1, 660, 1).
chemin(650, 1, 660, 2).
voisin(650, 1, 650, 2).
chemin(650, 2, 660, 2).
chemin(650, 2, 660, 1).
chemin(660, 1, 670, 1).
chemin(660, 1, 670, 2).
voisin(660, 1, 660, 2).
chemin(660, 2, 670, 2).
chemin(660, 2, 670, 1).
chemin(670, 1, 680, 1).
chemin(670, 1, 680, 2).
voisin(670, 1, 670, 2).
chemin(670, 2, 680, 2).
chemin(670, 2, 680, 1).
chemin(680, 1, 690, 1).
chemin(680, 1, 690, 2).
voisin(680, 1, 680, 2).
chemin(680, 2, 690, 2).
chemin(680, 2, 690, 1).
chemin(690, 1, 700, 1).
chemin(690, 1, 700, 2).
voisin(690, 1, 690, 2).
chemin(690, 2, 700, 2).
chemin(690, 2, 700, 1).
chemin(700, 1, 710, 1).
chemin(700, 1, 710, 2).
voisin(700, 1, 700, 2).
chemin(700, 2, 710, 2).
chemin(700, 2, 710, 1).
chemin(710, 1, 720, 1).
chemin(710, 1, 720, 2).
voisin(710, 1, 710, 2).
chemin(710, 2, 720, 2).
chemin(710, 2, 720, 1).
chemin(730, 1, 740, 1).
chemin(740, 1, 750, 1).
chemin(760, 1, 770, 1).
chemin(760, 1, 770, 2).
voisin(760, 1, 760, 2).
chemin(760, 2, 770, 2).
chemin(760, 2, 770, 1).
chemin(770, 1, 780, 1).
chemin(770, 1, 780, 2).
voisin(770, 1, 770, 2).
chemin(770, 2, 780, 2).
chemin(770, 2, 780, 1).
chemin(780, 1, 790, 1).
chemin(780, 1, 790, 2).
voisin(780, 1, 780, 2).
chemin(780, 2, 790, 2).
chemin(780, 2, 790, 1).
chemin(790, 1, 800, 1).
chemin(790, 1, 800, 2).
voisin(790, 1, 790, 2).
chemin(790, 2, 800, 2).
chemin(790, 2, 800, 1).
chemin(800, 1, 810, 1).
chemin(800, 1, 810, 2).
voisin(800, 1, 800, 2).
chemin(800, 2, 810, 2).
chemin(800, 2, 810, 1).
chemin(810, 1, 820, 1).
chemin(810, 1, 820, 2).
voisin(810, 1, 810, 2).
chemin(810, 2, 820, 2).
chemin(810, 2, 820, 1).
chemin(820, 1, 830, 1).
chemin(820, 1, 830, 2).
voisin(820, 1, 820, 2).
chemin(820, 2, 830, 2).
chemin(820, 2, 830, 1).
chemin(840, 1, 850, 1).
chemin(840, 3, 850, 3).
chemin(850, 1, 860, 1).
chemin(850, 3, 860, 3).
chemin(860, 1, 870, 1).
chemin(860, 3, 870, 3).
chemin(870, 1, 880, 1).
chemin(870, 3, 880, 3).
chemin(910, 1, 920, 1).
chemin(910, 3, 920, 3).
chemin(920, 1, 930, 1).
chemin(920, 3, 930, 3).
chemin(930, 1, 940, 1).
chemin(930, 3, 940, 3).

% CUSTOM
chemin(80, 1, 91, 1).
chemin(80, 1, 93, 2).
chemin(80, 2, 91, 1).
chemin(80, 2, 93, 2).
chemin(80, 3, 93, 2).
voisin(80, 1, 80, 2).
voisin(80, 2, 80, 3).
voisin(91, 1, 92, 2).
voisin(91, 1, 93, 2).
chemin(93, 2, 92, 2).
chemin(91, 1, 101, 1).
chemin(91, 1, 103, 2).
chemin(92, 2, 103, 2).
chemin(92, 2, 101, 1).
voisin(101, 1, 102, 2).
voisin(101, 1, 103, 2).
chemin(103, 2, 102, 2).
chemin(101, 1, 110, 1).
chemin(101, 1, 110, 2).
chemin(102, 2, 110, 2).
chemin(102, 2, 110, 1).
chemin(180, 1, 190, 1).
chemin(180, 1, 190, 2).
chemin(180, 1, 190, 3).
chemin(180, 2, 190, 3).
chemin(180, 2, 190, 2).
voisin(180, 1, 180, 2).
voisin(180, 2, 180, 3).
chemin(220, 1, 230, 1).
chemin(220, 1, 230, 2).
chemin(220, 2, 230, 1).
chemin(220, 2, 230, 2).
chemin(220, 3, 230, 4).
voisin(220, 1, 220, 2).
voisin(220, 2, 220, 3).
voisin(250, 1, 250, 2).
voisin(250, 2, 250, 3).
chemin(250, 1, 261, 1).
chemin(250, 1, 262, 2).
chemin(250, 2, 261, 1).
chemin(250, 2, 262, 2).
chemin(250, 4, 264, 4).
chemin(264, 4, 263, 4).
chemin(261, 1, 271, 1).
chemin(261, 1, 272, 2).
chemin(262, 2, 271, 1).
chemin(262, 2, 272, 2).
chemin(263, 4, 262, 4).
chemin(263, 4, 262, 4).
chemin(262, 4, 273, 4).
chemin(273, 4, 280, 4).
chemin(271, 1, 280, 1).
chemin(271, 1, 280, 2).
chemin(272, 2, 280, 1).
chemin(272, 2, 280, 2).
voisin(261, 1, 262, 2).
voisin(261, 1, 263, 2).
voisin(271, 1, 272, 2).
voisin(271, 1, 273, 2).
voisin(271, 1, 280, 1).
voisin(272, 2, 280, 1).
voisin(272, 2, 280, 2).
chemin(340, 1, 350, 1).
chemin(340, 1, 350, 2).
chemin(340, 4, 350, 3).
voisin(350, 1, 350, 2).
voisin(350, 2, 350, 3).
chemin(350, 1, 360, 1).
chemin(350, 2, 360, 1).
chemin(350, 2, 360, 2).
chemin(350, 3, 360, 1).
chemin(350, 3, 360, 2).
voisin(620, 1, 620, 2).
chemin(620, 1, 631, 1).
chemin(620, 1, 633, 2).
chemin(620, 2, 631, 1).
chemin(620, 2, 633, 2).
voisin(631, 1, 632, 2).
voisin(631, 1, 633, 2).
chemin(631, 1, 641, 1).
chemin(631, 1, 643, 2).
chemin(632, 2, 641, 1).
chemin(632, 2, 643, 2).
chemin(633, 2, 632, 2).
voisin(641, 1, 642, 2).
voisin(641, 1, 643, 2).
chemin(643, 2, 643, 2).
chemin(643, 2, 642, 2).
chemin(642, 2, 650, 2).
chemin(642, 2, 650, 1).
chemin(641, 1, 650, 1).
chemin(641, 1, 650, 2).
voisin(720, 1, 720, 2).
chemin(720, 1, 730, 1).
chemin(720, 2, 730, 1).
chemin(750, 1, 760, 1).
chemin(750, 1, 760, 2).
voisin(830, 1, 830, 2).
chemin(830, 1, 840, 1).
chemin(830, 1, 840, 3).
chemin(830, 2, 840, 1).
chemin(830, 2, 840, 3).
chemin(870, 1, 880, 1).
chemin(870, 3, 880, 3).
chemin(880, 1, 891, 1).
chemin(880, 3, 893, 3).
chemin(891, 1, 901, 1).
chemin(893, 3, 892, 3).
chemin(892, 3, 903, 3).
chemin(891, 1, 901, 1).
chemin(903, 3, 902, 3).
chemin(902, 3, 910, 3).
chemin(901, 1, 910, 1).
chemin(940, 1, 950, 1).
chemin(940, 1, 950, 2).
chemin(940, 3, 950, 2).
chemin(940, 3, 950, 3).
voisin(950, 1, 950, 2).
voisin(950, 2, 950, 3).
voisin(X1,Y1,X2,Y2) :- voisin(X2,Y2,X1,Y1).
voisin(X1,Y1,X2,Y2) :- chemin(X1,Y1,X2,Y2).

chemin(0,0,10,1).
chemin(0,0,10,2).
chemin(0,0,10,3).
chemin(950,1, Fin,0) :- caseFin(Fin).
chemin(950,2, Fin,0) :- caseFin(Fin).
chemin(950,3, Fin,0) :- caseFin(Fin).

caseFin(1000).


caseChance([91, 1]).
caseChance([101, 1]).
caseChance([110, 1]).
caseChance([120, 1]).
caseChance([150, 2]).
caseChance([160, 2]).
caseChance([190, 3]).
caseChance([210, 3]).
caseChance([240, 1]).
caseChance([261, 1]).
caseChance([280, 1]).
caseChance([300, 1]).
caseChance([320, 1]).
caseChance([340, 1]).
caseChance([480, 2]).
caseChance([570, 2]).
caseChance([660, 1]).
caseChance([660, 2]).
caseChance([740, 1]).
caseChance([903, 3]).