:- use_module('constants.pl').
% MOT CLES
% -------------- %
mclef(commence, 1).
mclef(commence, 2).
mclef(commence, 3).
% -------------- %
mclef(but, 1).
% -------------- %
mclef(ordre, 1).
mclef(ordre, 2).
mclef(ordre, 3).
% -------------- %
mclef(equipes, 1).
% -------------- %
mclef(coureurs, 1).
% -------------- %
mclef(maillot, 1).
% -------------- %
mclef(etape, 1).
mclef(etapes, 2).
% -------------- %
mclef(seconde, 1).
mclef(seconde, 2).
mclef(seconde, 3).
mclef(seconde, 4).
mclef(seconde, 5).
mclef(seconde, 6).
mclef(seconde, 7).
mclef(seconde, 8).
mclef(seconde, 9).
mclef(seconde, 10).
mclef(seconde, 11).
% -------------- %
mclef(deplacer, 1).
mclef(deplacer, 2).
mclef(deplacer, 3).
mclef(deplacer, 4).
mclef(deplacer, 5).
mclef(deplacer, 6).
% -------------- %
mclef(cases, 1).
mclef(cases, 2).
mclef(cases, 3).
mclef(cases, 4).
mclef(cases, 5).
mclef(case, 1).
% -------------- %
mclef(chance, 1).
mclef(chance, 2).
% -------------- %
mclef(fleche, 1).
mclef(fleche, 2).
% -------------- %
mclef(descente, 1).
mclef(descente, 2).
mclef(descente, 3).
% -------------- %
mclef(montee, 1).
mclef(montee, 2).
mclef(montee, 3).
% -------------- %
mclef(aspiration, 1).
mclef(aspiration, 2).
mclef(aspiration, 3).
% -------------- %
mclef(vitesse, 1).
mclef(vitesse, 2).
mclef(vitesse, 3).
% -------------- %
mclef(sprint, 1).
mclef(sprint, 2).
% -------------- %
mclef(serie, 1).
mclef(serie, 2).
mclef(serie, 3).
mclef(serie, 4).
mclef(serie, 5).
mclef(serie, 6).
% -------------- %
mclef(accidentee, 1).
% -------------- %
mclef(depasser, 1).
% -------------- %
mclef(equipe,100).


% QUESTIONS/REPONSES
% ---------------------------------------------------------------- %
regle_rep(commence, 1,
    [[comment, commence, t, on, le, jeu]],
    [[pour, commencer, le, 'jeu,', chaque, joueur, doit, tirer, X, carte, 'seconde.', 'Celui', qui, tire, la, plus,
    grande, carte, commence, le, 'jeu.']]) :- nb_cartesSecondeCommence(X).

regle_rep(commence, 2,
    [[qui, commence, le, jeu]],
    [['c\'est', au, joueur, ayant, la, plus, haute, carte, seconde, de, 'commencer.']]).

regle_rep(commence, 3,
    [[sur, quelle, case, commence, on, le, jeu]],
    [[le, jeu, commence, sur, la, case, 'depart.']]).
% ---------------------------------------------------------------- %
regle_rep(but, 1,
    [[quel, est, le, but, du, jeu]],
    [[le, but, du, jeu, est, de, recolter, le, plus, de, 'points.']]).
% ---------------------------------------------------------------- %
regle_rep(ordre, 1,
    [[dans, quel, ordre, jouent, les, equipes]],
    [['l\'ordre', de, jeu, est, le, 'suivant :', 'Italie,', 'Hollande,', 'Belgique', et, 'Allemagne.']]).

regle_rep(ordre, 2,
    [[dans, quel, ordre, deplace, t, on, les, coureurs]],
    [[on, deplace, les, coureurs, du, dernier, au, premier, dans, le, sens, de, la, 'course.']]).

regle_rep(ordre, 3,
    [[dans, quel, ordre, place, t, on, les, coureurs, accidentes]],
    [['l\'ordre', des, coureurs, accidentes, est, le, 'suivant :', du, coureur, qui, a, provoque, la, chute, au,
    coureur, qui, a, rejoint, plus, tard, le, lieu, de, 'l\'accident.']]).
% ---------------------------------------------------------------- %
regle_rep(equipes, 1,
    [[combien, d, equipes, compose, le, jeu]],
    [[le, jeu, se, compose, de, X, 'equipes.']]) :- nb_equipes(X).
% ---------------------------------------------------------------- %
regle_rep(coureurs, 1,
    [[combien, de, coureurs, compte, une, equipe]],
    [[chaque, equipe, compte, X, 'coureurs.']]) :- nb_coureurs(X).
% ---------------------------------------------------------------- %
regle_rep(maillot, 1,
    [[qui, peut, obtenir, le, maillot, jaune]],
    [[le, coureur, le, plus, rapide, de, 'l\'ensemble', des, etapes, peut, porter, le, maillot, 'jaune.']]).
% ---------------------------------------------------------------- %
regle_rep(etape, 1,
    [[qu, est, ce, qu, une, etape]],
    [[une, etape, represente, un, parcours, de, 'plateau.']]).

regle_rep(etapes, 2,
    [[combien, y, a, t, il, d, etapes]],
    [[il, y, a, X, 'etapes,', une, etape, de, plaine, et, une, etape, de, 'montagne.']]) :- nb_etapes(X).
% ---------------------------------------------------------------- %
regle_rep(seconde, 1,
    [[avec, combien, de, cartes, seconde, commence, t, on, le, jeu]],
    [[chaque, joueur, commence, avec, X, cartes, 'seconde.']]) :- nb_cartesSecondeCommence(X).

regle_rep(seconde, 2,
    [[quand, puis, je, a, nouveau, tirer, des, cartes, seconde]],
    [[quand, vous, 'n\'avez', plus, de, cartes, 'seconde.']]).

regle_rep(seconde, 3,
    [[combien, de, cartes, seconde, dois, je, tirer]],
    [[quand, un, joueur, 'n\'a', plus, de, carte, 'seconde,', il, doit, tirer, X, cartes, 'seconde.']])
    :- nb_cartesSecondeTirer(X).

regle_rep(seconde, 4,
    [[combien, y, a, t, il, de, cartes, seconde, dans, le, jeu]],
    [[il, y, a, X, cartes, seconde, dans, le, 'jeu.']]) :- nb_cartesSecondeTotale(X).

regle_rep(seconde, 5,
    [[combien, de, cartes, seconde, differentes, y, a, t, il, dans, le, jeu]],
    [[il, y, a, X, cartes, seconde, differentes, dans, le, 'jeu.']]) :- val_max_carte(X).

regle_rep(seconde, 6,
    [[combien, de, fois, une, carte, seconde, est, presente, dans, le, jeu]],
    [[chaque, carte, seconde, est, presente, X, fois, dans, le, 'jeu.']]) :- nb_repetition_cartes(X).

regle_rep(seconde, 7,
    [[quel, est, la, carte, seconde, la, plus, basse, du, jeu]],
    [[la, carte, seconde, X, est, la, plus, basse, du, 'jeu.']]) :- val_min_carte(X).

regle_rep(seconde, 8,
    [[quel, est, la, carte, seconde, la, plus, haute, du, jeu]],
    [[la, carte, seconde, X, est, la, plus, haute, du, 'jeu.']]) :- val_max_carte(X).

regle_rep(seconde, 9,
    [[a, quoi, servent, les, cartes, seconde]],
    [[elles, servent, a, deplacer, les, 'coureurs.']]).

regle_rep(seconde, 10,
    [[a, quoi, servent, les, nombres, sur, les, cartes, seconde]],
    [[ils, indiquent, le, nombre, de, deplacements, 'qu\'un', coureur, doit, faire, sur, le, 'plateau.']]).

regle_rep(seconde, 11,
    [[peut, on, choisir, la, carte, seconde, que, l, on, souhaite, pour, deplacer, un, coureur]],
    [['oui,', on, peut, choisir, la, carte, que, 'l\'on', 'veut.']]).
% ---------------------------------------------------------------- %
regle_rep(deplacer, 1,
    [[puis, je, deplacer, un, coureur, sur, une, case, occupe, par, un, autre, coureur]],
    [['non,', on, ne, peut, pas, deplacer, un, coureur, sur, une, case, deja, 'occupee.']]).

regle_rep(deplacer, 2,
    [[dans, quel, ordre, doit, on, deplacer, les, coureurs, de, son, equipe]],
    [[il, faut, 'd\'abord', deplacer, le, dernier, coureur, en, course, puis, 'l\'avant', dernier, et, ainsi, de,
    'suite.']]).

regle_rep(deplacer, 3,
    [[peut, on, deplacer, plusieurs, coureurs, par, tour]],
    [['non,', on, ne, peut, deplacer, 'qu\'un', coureur, par, 'tour.']]).

regle_rep(deplacer, 4,
    [[sur, quelle, cases, peut, on, deplacer, un, coureur]],
    [[on, peut, deplacer, un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case, devant, en,
    'diagonale.']]).

regle_rep(deplacer, 5,
    [[peut, on, deplacer, un, coureur, en, arriere]],
    [['non,', on, peut, seulement, deplacer, un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case,
    devant, en, 'diagonale.']]).

regle_rep(deplacer, 6,
    [[peut, on, deplacer, un, coureur, sur, le, cote]],
    [['non,', on, peut, seulement, deplacer, un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case,
    devant, en, 'diagonale.']]).
% ---------------------------------------------------------------- %
regle_rep(cases, 1,
    [[a, quoi, correspondent, les, nombres, sur, les, cases]],
    [[ils, indiquent, le, sens, de, deplacement, des, coureurs, et, les, cases, numerotees, sont, prioritaires, sur,
    les, 'autres.', 'Il', faut, savoir, 'qu\'il', y, a, une, sous, priorite, dans, les, virages, en, suivant,
    'l\'ordre', des, lettres, de, 'l\'alphabet.']]).

regle_rep(cases, 2,
    [[a, quoi, correspondent, les, lettres, sur, certaines, cases]],
    [[elles, indiquent, les, cases, de, virages, et, representent, 'l\'ordre', de, priorite, dans, les, 'virages.']]).

regle_rep(cases, 3,
    [[a, quoi, correspondent, les, double, fleches, sur, certaines, cases]],
    [[elles, 'n\'ont', aucune, utilite, dans, ce, 'jeu.']]).

regle_rep(cases, 4,
    [[a, quoi, correspondent, les, points, d, interrogation, sur, certaines, cases]],
    [[ils, indiquent, les, cases, chance, et, octroient, un, tirage, aleatoire, supplementaire, pour, le, coureur, qui,
    's\'arrete', 'dessus.']]).

regle_rep(cases, 5,
    [[a, quoi, correspondent, les, cases, jaune]],
    [[elles, indiquent, la, zone, 'd\'arrivee', et, permettent, 'd\'ameliorer', le, score, des, 'coureurs.']]).

regle_rep(case, 1,
    [[qu, est, ce, qu, une, case, prioritaire]],
    [[il, 's\'agit', des, cases, numerotees, et, elles, permettent, 'd\'indiquer', le, coureur, qui, a, la, priorite,
    sur, les, coureurs, a, cote, de, 'lui.']]).
% ---------------------------------------------------------------- %
regle_rep(chance, 1,
    [[que, fait],1,[ case, chance]],
    [[elle, octroie, un, tirage, aleatoire, supplementaire, entre, '-3', et, 3, secondes, pour, le, coureur, qui,
    's\'arrete', 'dessus.']]).

regle_rep(chance, 2,
    [[est, ce, qu, une, carte, chance, peut, provoquer, une, chute, en, serie]],
    [['non,', une, carte, chance, ne, peut, pas, provoquer, de, chute, en, 'serie.']]).
% ---------------------------------------------------------------- %
regle_rep(fleche, 1,
    [[que, represente, la, fleche, rouge, sur, le, plateau]],
    [[elle, indique, une, case, de, 'montee.']]).

regle_rep(fleche, 2,
    [[que, represente, la, fleche, bleu, sur, le, plateau]],
    [[elle, indique, une, case, de, 'descente.']]).
% ---------------------------------------------------------------- %
regle_rep(descente, 1,
    [[comment, reconnait, on, les, cases, de, descente]],
    [[il, 's\'agit', des, cases, sur, lesquelles, se, trouvent, des, fleches, 'bleu.']]).

regle_rep(descente, 2,
    [[quel, est, l, avantage, de, la, descente]],
    [[le, phenomene, 'd\'aspiration', en, descente, vous, permet, 'd\'obtenir', une, seconde, supplementaire, a, celle,
    deja, octroyee, et, vous, pouvez, doubler, le, coureur, devant, vous, de, maximum, 1, 'case.']]).

regle_rep(descente, 3,
    [[quel, est, le, desavantage, de, la, descente]],
    [[il, 'n\'y', a, pas, de, desavantage, en, 'descente.']]).
% ---------------------------------------------------------------- %
regle_rep(montee, 1,
    [[comment, reconnait, on, les, cases, de, montee]],
    [[il, 's\'agit', des, cases, sur, lesquelles, se, trouvent, des, fleches, 'rouge.']]).

regle_rep(montee, 2,
    [[quel, est, l, avantage, de, la, montee]],
    [[il, 'n\'y', a, pas, 'd\'avantage', en, 'montee.']]).

regle_rep(montee, 3,
    [[quel, est, le, desavantage, de, la, montee]],
    [[les, cartes, seconde, jouees, en, montee, sont, divises, par, 2, et, il, 'n\'y', a, pas, de, phenomene,
    'd\'aspiration.']]).
% ---------------------------------------------------------------- %
regle_rep(aspiration, 1,
    [[quel, ce, que, le, phenomene, d, aspiration]],
    [[il, 's\'agit', 'd\'un', avantage, pour, un, coureur, quand, il, se, trouve, au, sein, 'd\'un', peloton, '(pas',
    en, 'tete)', ou, derriere, un, autre, 'coureur.', 'Si', son, deplacement, lui, permet, de, se, retrouver, aussi, au,
    sein, 'd\'un', 'peloton,', derriere, ou, a, cote, 'd\'un', autre, 'coureur,', il, beneficie, 'd\'une', seconde,
    'supplementaire.', 'Attention,', en, cas, de, 'descente,', le, coureur, beneficie, 'd\'un', supplement, de, 2,
    secondes, au, lieu, de, 1, 'seconde.']]).

regle_rep(aspiration, 2,
    [[est, ce, que, je, beneficie, du, phenomene, d, aspiration, si, je, suis, en, tete, de, peloton]],
    [['non,', on, beneficie, du, phenomene, 'd\'aspiration', seulement, au, sein, 'd\'un', peloton, '(pas', en, 'tete)',
    ou, derriere, un, autre, 'coureur.']]).

regle_rep(aspiration, 3,
    [[est, ce, que, le, phenomene, d, aspiration, est, obligatoire]],
    [['non,', il, 'n\'est', pas, 'obligatoire.']]).
% ---------------------------------------------------------------- %
regle_rep(vitesse, 1,
    [[quel, ce, que, le, phenomene, de, prise, de, vitesse]],
    [[il, 's\'agit', 'd\'un', avantage, pour, un, coureur, quand, il, se, trouve, au, sein, 'd\'un', peloton, '(pas',
    en, 'tete)', ou, derriere, un, autre, 'coureur.', 'Si', son, deplacement, lui, permet, de, se, retrouver, aussi, au,
    sein, 'd\'un', 'peloton,', derriere, ou, a, cote, 'd\'un', autre, 'coureur,', il, beneficie, 'd\'une', seconde,
    'supplementaire.', 'Attention,', en, cas, de, 'descente,', le, coureur, beneficie, 'd\'un', supplement, de, 2,
    secondes, au, lieu, de, 1, 'seconde.']]).

regle_rep(vitesse, 2,
    [[est, ce, que, je, beneficie, du, phenomene, de, prise, de, vitesse, si, je, suis, en, tete, de, peloton]],
    [['non,', on, beneficie, du, phenomene, de, prise, de, vitesse, seulement, au, sein, 'd\'un', peloton, '(pas', en,
    'tete)', ou, derriere, un, autre, 'coureur.']]).

regle_rep(vitesse, 3,
    [[est, ce, que, le, phenomene, de, prise, de, vitesse, est, obligatoire]],
    [['non,', il, 'n\'est', pas, 'obligatoire.']]).
% ---------------------------------------------------------------- %
regle_rep(sprint, 1,
    [[qu, est, ce, qu, un, sprint]],
    [[il, 's\'agit', de, la, banniere, verte, sur, le, 'plateau.', 'Lorsqu\'un', ou, plusieurs, coureurs, passent, la,
    'banniere,', ils, beneficient, de, secondes, 'et/ou', de, points, de, 'bonification.', 'Ils', se, calculent, a, la,
    fin, de, chaque, 'etape. Les', secondes, de, bonification, sont, retirees, du, temps, du, coureur, et, les, points,
    de, bonification, sont, ajoutes, au, score, de, son, 'equipe.']]).

regle_rep(sprint, 2,
    [[quel, est, l, avantage, d, un, sprint]],
    [[il, permet, a, un, ou, plusieurs, coureurs, de, beneficier, de, secondes, 'et/ou', de, points, de,
    'bonification.', 'Ils', se, calculent, a, la, fin, de, chaque, 'etape.', 'Les', secondes, de, bonification, sont,
    retirees, du, temps, du, coureur, et, les, points, de, bonification, sont, ajoutes, au, score, de, son, 'equipe.']]).
% ---------------------------------------------------------------- %
regle_rep(serie, 1,
    [[qu, est, ce, qu, une, chute, en, serie]],
    [[il, est, interdit, de, rentrer, en, contact, avec, 'd\'autres', coureurs, dans, le, jeu, mais, parfois, un,
    coureur, est, contraint, de, 's\'arreter', sur, une, case, ou, se, trouve, deja, un, 'coureur.', 'Dans', ce, 'cas,',
    il, provoque, automatiquement, une, chute, en, 'serie.', 'Les', coureurs, concernes, doivent, se, defausser,
    'd\'une', carte, seconde, puis, passer, un, 'tour.', 'Une', chute, en, serie, a, lieu, sur, toute, la, largeur, de,
    la, route, et, implique, les, coureurs, 'suivants :', le, coureur, ayant, provoque, la, chute, en, 'serie;', le,
    coureur, avec, lequel, il, rentre, en, 'contact;', tous, les, coureurs, situes, sur, les, cases, adjacentes,
    portant, le, meme, 'numero,', y, compris, les, lettres, 'A,', 'B,', 'C', 'etc.;', tous, les, coureurs, 'qui,', lors,
    de, ce, tour, de, 'jeu,', ne, peuvent, pas, freiner, a, temps, et, arrivent, sur, une, des, cases, affectees, par,
    'l\'accident.']]).

regle_rep(serie, 2,
    [[qu, est, ce, qui, provoque, une, chute, en, serie]],
    [[une, chute, en, serie, est, provoquee, par, le, contact, entre, 2, 'coureurs,', 'c\'est-a-dire,', 'qu\'ils',
    's\'arretent', sur, la, meme, 'case.']]).

regle_rep(serie, 3,
    [[est, ce, qu, une, chute, en, serie, affecte, la, route, de, l, autre, cote, du, terre, plein]],
    [['non,', elle, ne, 'l\'affecte', 'pas.']]).

regle_rep(serie, 4,
    [[que, doit, on, faire, en, cas, de, chute, en, serie]],
    [[il, faut, placer, les, coureurs, impliques, sur, le, bord, de, la, route, a, 'l\'endroit', de, 'l\'accident', et,
    les, placer, dans, 'l\'ordre', de, 'chute :', du, coureur, qui, a, provoque, la, chute, au, coureur, qui, a,
    rejoint, plus, tard, le, lieu, de, 'l\'accident.', 'Une', fois, que, tous, ces, coureurs, auront, passe, leur,
    'tour,', ils, repartiront, dans, cet, 'ordre.']]).

regle_rep(serie, 5,
    [[dois, je, obligatoirement, eviter, de, provoquer, une, chute, en, serie, si, cela, est, possible]],
    [['oui,', il, est, interdit, de, provoquer, une, chute, en, serie, si, elle, est, 'evitable.']]).

regle_rep(serie, 6,
    [[sur, quel, case, dois, je, me, placer, en, cas, de, chute, en, serie, inevitable]],
    [[il, est, obligatoire, de, se, placer, sur, la, case, vide, la, plus, eloignee, de, la, case, 'prioritaire.']]).
% ---------------------------------------------------------------- %
regle_rep(accidentee, 1,
    [[puis, je, traverser, une, zone, accidentee]],
    [['oui,', si, les, coureurs, accidentes, sont, sur, le, cote, de, la, 'route.']]).
% ---------------------------------------------------------------- %
regle_rep(depasser, 1,
    [[puis, je, passer, sur, le, bas, cote, de, la, route, pour, depasser]],
    [['oui,', uniquement, si, toute, la, largueur, de, la, route, est, occupee, par, des, coureurs, et, que, le,
    coureur, possede, assez, de, secondes, pour, passer, le, groupe, de, 'coureurs.']]).
% ---------------------------------------------------------------- %
regle_rep(equipe, 100,
    [[combien], 3, [coureurs], 5, [equipe]],
    [[chaque, equipe, compte, X, 'coureurs.']]) :- nb_coureurs(X).