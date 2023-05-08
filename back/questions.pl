:- use_module('constants.pl').

% MOT CLES
% -------------- %
mclef(fleche, 1).
mclef(fleche, 2).
mclef(fleche, 3).
% -------------- %
mclef(ordre, 1).
mclef(ordre, 2).
mclef(accidente, 3).
% -------------- %
mclef(combien, 1).
mclef(combien, 2).
mclef(combien, 3).
mclef(combien, 4).
mclef(combien, 5).
mclef(combien, 6).
mclef(combien, 7).
mclef(combien, 8).
% -------------- %
mclef(carte, 1).
mclef(carte, 2).
mclef(carte, 3).
mclef(carte, 4).
mclef(carte, 5).
mclef(carte, 6).
mclef(carte, 7).
% -------------- %
mclef(case, 1).
mclef(case, 2).
mclef(case, 3).
mclef(case, 4).
mclef(case, 5).
mclef(case, 6).
mclef(case, 7).
mclef(case, 8).
mclef(case, 9).
% -------------- %
mclef(descente, 1).
mclef(descente, 2).
% -------------- %
mclef(montee, 1).
mclef(montee, 2).
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
mclef(sprint, 3).
% -------------- %
mclef(chute, 1).
mclef(chute, 2).
mclef(chute, 3).
mclef(chute, 4).
mclef(chute, 5).
mclef(chute, 6).
% -------------- %
mclef(coureur, 1).
mclef(coureur, 2).
mclef(coureur, 3).
mclef(coureur, 4).
% -------------- %
mclef(jeu, 1).
mclef(jeu, 2).
mclef(jeu, 3).
mclef(jeu, 4).
% -------------- %
mclef(maillot, 1).
% -------------- %
mclef(etape, 1).
% -------------- %
mclef(accidente, 1).
% -------------- %
mclef(cote, 2).
% -------------- %


% QUESTIONS/REPONSES
% ---------------------------------------------------------------- %
regle_rep(fleche, 1,
    [[fleche, rouge]],
    [[les, fleches, rouge, indiquent, les, cases, de, 'montee.']]).

regle_rep(fleche, 2,
    [[fleche, bleue]],
    [[les, fleches, bleues, indiquent, les, cases, de, 'descente.']]).

regle_rep(fleche, 3,
    [[double, fleche]],
    [[les, doubles, fleches, 'n\'ont', aucune, utilite, dans, ce, 'jeu.']]).
% ---------------------------------------------------------------- %
regle_rep(ordre, 1,
    [[ordre], 10, [equipe]],
    [['l\'ordre', de, jeu, est, le, 'suivant :', 'Italie,', 'Hollande,', 'Belgique', et, 'Allemagne.']]).

regle_rep(ordre, 2,
    [[ordre], 10, [accidente]],
    [['l\'ordre', des, coureurs, accidentes, est, le, 'suivant :', du, coureur, qui, a, provoque, la, chute, au,
    coureur, qui, a, rejoint, plus, tard, le, lieu, de, 'l\'accident.']]).

regle_rep(ordre, 3,
    [[ordre], 10, [coureur]],
    [[il, faut, 'd\'abord', deplacer, le, dernier, coureur, en, course, puis, 'l\'avant', dernier, et, ainsi, de,
    'suite.']]).
% ---------------------------------------------------------------- %
regle_rep(combien, 1,
    [[combien], 10, [equipe]],
    [[le, jeu, se, compose, de, X, 'equipes.']]) :- nb_equipes(X).

regle_rep(combien, 2,
    [[combien], 10, [coureur]],
    [[chaque, equipe, compte, X, 'coureurs.']]) :- nb_coureurs(X).

regle_rep(combien, 3,
    [[combien], 10, [etape]],
    [[il, y, a, X, 'etapes,', une, etape, de, plaine, et, une, etape, de, 'montagne.']]) :- nb_etapes(X).

regle_rep(combien, 4,
    [[combien], 10, [carte, seconde], 10, [commence]],
    [[chaque, joueur, commence, avec, X, cartes, 'seconde.']]) :- nb_cartesSecondeCommence(X).

regle_rep(combien, 5,
    [[combien], 10, [carte, seconde], 10, [tirer]],
    [[quand, un, joueur, 'n\'a', plus, de, carte, 'seconde,', il, doit, tirer, X, cartes, 'seconde.']])
    :- nb_cartesSecondeTirer(X).

regle_rep(combien, 6,
    [[combien], 10, [carte, seconde], 10, [jeu]],
    [[il, y, a, X, cartes, seconde, dans, le, 'jeu.']]) :- nb_cartesSecondeTotale(X).

regle_rep(combien, 7,
    [[combien], 10, [carte, seconde], 10, [differentes]],
    [[il, y, a, X, cartes, seconde, differentes, dans, le, 'jeu.']]) :- val_max_carte(X).

regle_rep(combien, 8,
    [[combien, de, fois], 10, [carte, seconde]],
    [[chaque, carte, seconde, est, presente, X, fois, dans, le, 'jeu.']]) :- nb_repetition_cartes(X).
% ---------------------------------------------------------------- %
regle_rep(carte, 1,
    [[carte, seconde], 10, [la, plus, basse]],
    [[la, carte, seconde, X, est, la, plus, basse, du, 'jeu.']]) :- val_min_carte(X).

regle_rep(carte, 2,
    [[carte, seconde], 10, [la, plus, haute]],
    [[la, carte, seconde, X, est, la, plus, haute, du, 'jeu.']]) :- val_max_carte(X).

regle_rep(carte, 3,
    [[carte, seconde], 10, [deplacer], 10, [coureur]],
    [['oui,', on, peut, choisir, la, carte, que, 'l\'on', 'veut.']]).

regle_rep(carte, 4,
    [[carte, chance], 10, [chute]],
    [['non,', une, carte, chance, ne, peut, pas, provoquer, de, chute, en, 'serie.']]).

regle_rep(carte, 5,
    [[quand], 10, [tirer], 10, [carte, seconde]],
    [[quand, vous, 'n\'avez', plus, de, cartes, 'seconde.']]).

regle_rep(carte, 6,
    [[nombre], 10, [carte, seconde]],
    [[ils, indiquent, le, nombre, de, deplacements, 'qu\'un', coureur, doit, faire, sur, le, 'plateau.']]).

regle_rep(carte, 7,
    [[carte, seconde]],
    [[elles, servent, a, deplacer, les, 'coureurs.']]).
% ---------------------------------------------------------------- %
regle_rep(case, 1,
    [[point], 10, [interrogation], 10, [case]],
    [[ils, indiquent, les, cases, chance, et, octroient, un, tirage, aleatoire, supplementaire, pour, le, coureur, qui,
    's\'arrete', 'dessus.']]).

regle_rep(case, 2,
    [[case], 10, [jaune]],
    [[elles, indiquent, la, zone, 'd\'arrivee', et, permettent, 'd\'ameliorer', le, score, des, 'coureurs.']]).

regle_rep(case, 3,
    [[case, prioritaire]],
    [[il, 's\'agit', des, cases, numerotees, et, elles, permettent, 'd\'indiquer', le, coureur, qui, a, la, priorite,
    sur, les, coureurs, a, cote, de, 'lui.']]).

regle_rep(case, 4,
    [[case, chance]],
    [[elle, octroie, un, tirage, aleatoire, supplementaire, entre, '-3', et, 3, secondes, pour, le, coureur, qui,
    's\'arrete', 'dessus.']]).

regle_rep(case, 5,
    [[case], 10, [descente]],
    [[il, 's\'agit', des, cases, sur, lesquelles, se, trouvent, des, fleches, 'bleue.']]).

regle_rep(case, 6,
    [[case], 10, [montee]],
    [[il, 's\'agit', des, cases, sur, lesquelles, se, trouvent, des, fleches, 'rouge.']]).

regle_rep(case, 7,
    [[nombre], 10, [case]],
    [[ils, indiquent, le, sens, de, deplacement, des, coureurs, et, les, cases, numerotees, sont, prioritaires, sur,
    les, 'autres.', 'Il', faut, savoir, 'qu\'il', y, a, une, sous, priorite, dans, les, virages, en, suivant,
    'l\'ordre', des, lettres, de, 'l\'alphabet.']]).

regle_rep(case, 8,
    [[lettre], 10, [case]],
    [[elles, indiquent, les, cases, de, virages, et, representent, 'l\'ordre', de, priorite, dans, les, 'virages.']]).

regle_rep(case, 9,
    [[case], 10, [coureur]],
    [[on, peut, deplacer, un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case, devant, en,
    'diagonale.']]).
% ---------------------------------------------------------------- %
regle_rep(descente, 1,
    [[avantage], 10, [descente]],
    [[le, phenomene, 'd\'aspiration', en, descente, vous, permet, 'd\'obtenir', une, seconde, supplementaire, a, celle,
    deja, octroyee, et, vous, pouvez, doubler, le, coureur, devant, vous, de, maximum, 1, 'case.']]).

regle_rep(descente, 2,
    [[desavantage], 10, [descente]],
    [[il, 'n\'y', a, pas, de, desavantage, en, 'descente.']]).
% ---------------------------------------------------------------- %
regle_rep(montee, 1,
    [[avantage], 10, [montee]],
    [[il, 'n\'y', a, pas, 'd\'avantage', en, 'montee.']]).

regle_rep(montee, 2,
    [[desavantage], 10, [montee]],
    [[les, cartes, seconde, jouees, en, montee, sont, divises, par, 2, et, il, 'n\'y', a, pas, de, phenomene,
    'd\'aspiration.']]).
% ---------------------------------------------------------------- %
regle_rep(aspiration, 1,
    [[quoi], 10, [aspiration]],
    [[il, 's\'agit', 'd\'un', avantage, pour, un, coureur, quand, il, se, trouve, au, sein, 'd\'un', peloton, '(pas',
    en, 'tete)', ou, derriere, un, autre, 'coureur.', 'Si', son, deplacement, lui, permet, de, se, retrouver, aussi, au,
    sein, 'd\'un', 'peloton,', derriere, ou, a, cote, 'd\'un', autre, 'coureur,', il, beneficie, 'd\'une', seconde,
    'supplementaire.', 'Attention,', en, cas, de, 'descente,', le, coureur, beneficie, 'd\'un', supplement, de, 2,
    secondes, au, lieu, de, 1, 'seconde.']]).

regle_rep(aspiration, 2,
    [[aspiration], 10, [tete]],
    [['non,', on, beneficie, du, phenomene, 'd\'aspiration', seulement, au, sein, 'd\'un', peloton, '(pas', en, 'tete)',
    ou, derriere, un, autre, 'coureur.']]).

regle_rep(aspiration, 3,
    [[aspiration], 10, [obligatoire]],
    [['non,', il, 'n\'est', pas, 'obligatoire.']]).
% ---------------------------------------------------------------- %
regle_rep(vitesse, 1,
    [[quoi], 10, [vitesse]],
    [[il, 's\'agit', 'd\'un', avantage, pour, un, coureur, quand, il, se, trouve, au, sein, 'd\'un', peloton, '(pas',
    en, 'tete)', ou, derriere, un, autre, 'coureur.', 'Si', son, deplacement, lui, permet, de, se, retrouver, aussi, au,
    sein, 'd\'un', 'peloton,', derriere, ou, a, cote, 'd\'un', autre, 'coureur,', il, beneficie, 'd\'une', seconde,
    'supplementaire.', 'Attention,', en, cas, de, 'descente,', le, coureur, beneficie, 'd\'un', supplement, de, 2,
    secondes, au, lieu, de, 1, 'seconde.']]).

regle_rep(vitesse, 3,
    [[vitesse], 10, [tete]],
    [['non,', on, beneficie, du, phenomene, de, prise, de, vitesse, seulement, au, sein, 'd\'un', peloton, '(pas', en,
    'tete)', ou, derriere, un, autre, 'coureur.']]).

regle_rep(vitesse, 3,
    [[vitesse], 10, [obligatoire]],
    [['non,', il, 'n\'est', pas, 'obligatoire.']]).
% ---------------------------------------------------------------- %
regle_rep(sprint, 1,
    [[quoi], 10, [sprint]],
    [[il, 's\'agit', de, la, banniere, verte, sur, le, 'plateau.', 'Lorsqu\'un', ou, plusieurs, coureurs, passent, la,
    'banniere,', ils, beneficient, de, secondes, 'et/ou', de, points, de, 'bonification.', 'Ils', se, calculent, a, la,
    fin, de, chaque, 'etape. Les', secondes, de, bonification, sont, retirees, du, temps, du, coureur, et, les, points,
    de, bonification, sont, ajoutes, au, score, de, son, 'equipe.']]).

regle_rep(sprint, 2,
    [[avantage], 10, [sprint]],
    [[il, permet, a, un, ou, plusieurs, coureurs, de, beneficier, de, secondes, 'et/ou', de, points, de,
    'bonification.', 'Ils', se, calculent, a, la, fin, de, chaque, 'etape.', 'Les', secondes, de, bonification, sont,
    retirees, du, temps, du, coureur, et, les, points, de, bonification, sont, ajoutes, au, score, de, son, 'equipe.']]).

regle_rep(sprint, 3,
    [[desavantage], 10, [sprint]],
    [[il, 'n\'y', a, pas, de, desavantage, lors, 'd\'un', 'sprint.']]).
% ---------------------------------------------------------------- %
regle_rep(chute, 1,
    [[quoi], 10, [chute]],
    [[il, est, interdit, de, rentrer, en, contact, avec, 'd\'autres', coureurs, dans, le, jeu, mais, parfois, un,
    coureur, est, contraint, de, 's\'arreter', sur, une, case, ou, se, trouve, deja, un, 'coureur.', 'Dans', ce, 'cas,',
    il, provoque, automatiquement, une, chute, en, 'serie.', 'Les', coureurs, concernes, doivent, se, defausser,
    'd\'une', carte, seconde, puis, passer, un, 'tour.', 'Une', chute, en, serie, a, lieu, sur, toute, la, largeur, de,
    la, route, et, implique, les, coureurs, 'suivants :', le, coureur, ayant, provoque, la, chute, en, 'serie;', le,
    coureur, avec, lequel, il, rentre, en, 'contact;', tous, les, coureurs, situes, sur, les, cases, adjacentes,
    portant, le, meme, 'numero,', y, compris, les, lettres, 'A,', 'B,', 'C', 'etc.;', tous, les, coureurs, 'qui,', lors,
    de, ce, tour, de, 'jeu,', ne, peuvent, pas, freiner, a, temps, et, arrivent, sur, une, des, cases, affectees, par,
    'l\'accident.']]).

regle_rep(chute, 2,
    [[provoque], 10, [chute]],
    [[une, chute, en, serie, est, provoquee, par, le, contact, entre, 2, 'coureurs,', 'c\'est-a-dire,', 'qu\'ils',
    's\'arretent', sur, la, meme, 'case.']]).

regle_rep(chute, 3,
    [[chute], 15, [terre, plein]],
    [['non,', elle, 'n\'affecte', pas, 'l\'autre', cote, du, terre, 'plein.']]).

regle_rep(chute, 4,
    [[faire], 10, [chute]],
    [[il, faut, placer, les, coureurs, impliques, sur, le, bord, de, la, route, a, 'l\'endroit', de, 'l\'accident', et,
    les, placer, dans, 'l\'ordre', de, 'chute :', du, coureur, qui, a, provoque, la, chute, au, coureur, qui, a,
    rejoint, plus, tard, le, lieu, de, 'l\'accident.', 'Une', fois, que, tous, ces, coureurs, auront, passe, leur,
    'tour,', ils, repartiront, dans, cet, 'ordre.']]).

regle_rep(chute, 5,
    [[eviter], 10, [chute]],
    [['oui,', il, est, interdit, de, provoquer, une, chute, en, serie, si, elle, est, 'evitable.']]).

regle_rep(chute, 6,
    [[case], 10, [chute], 10, [inevitable]],
    [[il, est, obligatoire, de, se, placer, sur, la, case, vide, la, plus, eloignee, de, la, case, 'prioritaire.']]).
% ---------------------------------------------------------------- %
regle_rep(coureur, 1,
    [[coureur], 10, [case], 10, [occupe]],
    [['non,', on, ne, peut, pas, deplacer, un, coureur, sur, une, case, deja, 'occupee.']]).

regle_rep(coureur, 2,
    [[coureur], 10, [cote]],
    [['non,', on, peut, seulement, deplacer, un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case,
    devant, en, 'diagonale.']]).

regle_rep(coureur, 3,
    [[coureur], 10, [arriere]],
    [['non,', on, peut, seulement, deplacer, un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case,
    devant, en, 'diagonale.']]).

regle_rep(coureur, 4,
    [[plusieurs, coureur], 10, [tour]],
    [['non,', on, ne, peut, deplacer, 'qu\'un', coureur, par, 'tour.']]).
% ---------------------------------------------------------------- %
regle_rep(jeu, 1,
    [[comment], 10, [commence], 10, [jeu]],
    [[pour, commencer, le, 'jeu,', chaque, joueur, doit, tirer, X, carte, 'seconde.', 'Celui', qui, tire, la, plus,
    grande, carte, commence, le, 'jeu.']]) :- nb_cartesSecondeCommence(X).

regle_rep(jeu, 2,
    [[case], 10, [commence], 10, [jeu]],
    [[le, jeu, commence, sur, la, case, 'depart.']]).

regle_rep(jeu, 3,
    [[but], 10, [jeu]],
    [[le, but, du, jeu, est, de, recolter, le, plus, de, 'points.']]).

regle_rep(jeu, 4,
    [[qui], 10, [commence], 10, [jeu]],
    [['c\'est', au, joueur, ayant, la, plus, haute, carte, seconde, de, 'commencer.']]).
% ---------------------------------------------------------------- %
regle_rep(maillot, 1,
    [[qui], 10, [maillot, jaune]],
    [[le, coureur, le, plus, rapide, de, 'l\'ensemble', des, etapes, peut, porter, le, maillot, 'jaune.']]).
% ---------------------------------------------------------------- %
regle_rep(etape, 1,
    [[quoi], 10, [etape]],
    [[une, etape, represente, un, parcours, de, 'plateau.']]).
% ---------------------------------------------------------------- %
regle_rep(accidente, 1,
    [[traverser], 10, [accidente]],
    [['oui,', si, les, coureurs, accidentes, sont, sur, le, cote, de, la, 'route.']]).
% ---------------------------------------------------------------- %
regle_rep(cote, 2,
    [[depasser], 10, [bas, cote]],
    [['oui,', uniquement, si, toute, la, largueur, de, la, route, est, occupee, par, des, coureurs, et, que, le,
    coureur, possede, assez, de, secondes, pour, passer, le, groupe, de, 'coureurs.']]).
% ---------------------------------------------------------------- %