:- use_module('constants.pl').

% MOT CLES
% -------------- %
mclef(fleche, 1).
mclef(fleche, 2).
mclef(fleche, 3).
% -------------- %
mclef(ordre, 1).
mclef(ordre, 2).
mclef(ordre, 3).
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
    [['Les', 'flèches', rouges, indiquent, les, cases, de, 'montée.']]).

regle_rep(fleche, 2,
    [[fleche, bleue]],
    [['Les', 'flèches', bleues, indiquent, les, cases, de, 'descente.']]).

regle_rep(fleche, 3,
    [[double, fleche]],
    [['Les', doubles, 'flèches', 'n\'ont', aucune, 'utilité', dans, ce, 'jeu.']]).
% ---------------------------------------------------------------- %
regle_rep(ordre, 1,
    [[ordre], 10, [equipe]],
    [['L\'ordre', de, jeu, est, le, 'suivant :', 'Italie,', 'Hollande,', 'Belgique', et, 'Allemagne.']]).

regle_rep(ordre, 2,
    [[ordre], 10, [accidente]],
    [['L\'ordre', des, coureurs, 'accidentés', est, le, 'suivant :', du, coureur, qui, a, 'provoqué', la, chute, au,
    coureur, qui, a, rejoint, plus, tard, le, lieu, de, 'l\'accident.']]).

regle_rep(ordre, 3,
    [[ordre], 10, [coureur]],
    [['Il', faut, 'd\'abord', 'déplacer', le, dernier, coureur, en, course, puis, 'l\'avant-dernier', et, ainsi, de,
    'suite.']]).
% ---------------------------------------------------------------- %
regle_rep(combien, 1,
    [[combien], 10, [equipe]],
    [['Le', jeu, se, compose, de, X, 'équipes.']]) :- nb_equipes(X).

regle_rep(combien, 2,
    [[combien], 10, [coureur]],
    [['Chaque', 'équipe', compte, X, 'coureurs.']]) :- nb_coureurs(X).

regle_rep(combien, 3,
    [[combien], 10, [etape]],
    [['Il', y, a, X, 'étapes,', une, 'étape', de, plaine, et, une, 'étape', de, 'montagne.']]) :- nb_etapes(X).

regle_rep(combien, 4,
    [[combien], 10, [carte, seconde], 10, [commence]],
    [['Chaque', joueur, commence, avec, X, cartes, 'seconde.']]) :- nb_cartesSecondeCommence(X).

regle_rep(combien, 5,
    [[combien], 10, [carte, seconde], 10, [tirer]],
    [['Quand', un, joueur, 'n\'a', plus, de, carte, 'seconde,', il, doit, tirer, X, cartes, 'seconde.']])
    :- nb_cartesSecondeTirer(X).

regle_rep(combien, 6,
    [[combien], 10, [carte, seconde], 10, [jeu]],
    [['Il', y, a, X, cartes, seconde, dans, le, 'jeu.']]) :- nb_cartesSecondeTotale(X).

regle_rep(combien, 7,
    [[combien], 10, [carte, seconde], 10, [differentes]],
    [['Il', y, a, X, cartes, seconde, 'différentes', dans, le, 'jeu.']]) :- val_max_carte(X).

regle_rep(combien, 8,
    [[combien, de, fois], 10, [carte, seconde]],
    [['Chaque', carte, seconde, est, 'présente', X, fois, dans, le, 'jeu.']]) :- nb_repetition_cartes(X).
% ---------------------------------------------------------------- %
regle_rep(carte, 1,
    [[carte, seconde], 10, [la, plus, basse]],
    [['La', carte, seconde, X, est, la, plus, basse, du, 'jeu.']]) :- val_min_carte(X).

regle_rep(carte, 2,
    [[carte, seconde], 10, [la, plus, haute]],
    [['La', carte, seconde, X, est, la, plus, haute, du, 'jeu.']]) :- val_max_carte(X).

regle_rep(carte, 3,
    [[carte, seconde], 10, [deplacer], 10, [coureur]],
    [['Oui,', on, peut, choisir, la, carte, que, 'l\'on', 'veut.']]).

regle_rep(carte, 4,
    [[carte, chance], 10, [chute]],
    [['Non,', une, carte, chance, ne, peut, pas, provoquer, de, chute, en, 'série.']]).

regle_rep(carte, 5,
    [[quand], 10, [tirer], 10, [carte, seconde]],
    [['Quand', vous, 'n\'avez', plus, de, cartes, 'seconde.']]).

regle_rep(carte, 6,
    [[nombre], 10, [carte, seconde]],
    [['Ils', indiquent, le, nombre, de, 'déplacements', 'qu\'un', coureur, doit, faire, sur, le, 'plateau.']]).

regle_rep(carte, 7,
    [[carte, seconde]],
    [['Elles', servent, 'à', 'déplacer', les, 'coureurs.']]).
% ---------------------------------------------------------------- %
regle_rep(case, 1,
    [[point], 10, [interrogation], 10, [case]],
    [['Ils', indiquent, les, cases, chance, et, octroient, un, tirage, 'aléatoire', 'supplémentaire', pour, le, coureur,
    qui, 's\'arrête', 'dessus.']]).

regle_rep(case, 2,
    [[case], 10, [jaune]],
    [['Elles', indiquent, la, zone, 'd\'arrivée', et, permettent, 'd\'améliorer', le, score, des, 'coureurs.']]).

regle_rep(case, 3,
    [[case, prioritaire]],
    [['Il', 's\'agit', des, cases, 'numérotées', et, elles, permettent, 'd\'indiquer', le, coureur, qui, a, la,
    'priorité', sur, les, coureurs, 'à', 'côté', de, 'lui.']]).

regle_rep(case, 4,
    [[case, chance]],
    [['Elles', octroient, un, tirage, 'aléatoire', 'supplémentaire', entre, '-3', et, 3, secondes, pour, le, coureur,
    qui, 's\'arrête', 'dessus.']]).

regle_rep(case, 5,
    [[case], 10, [descente]],
    [['Il', 's\'agit', des, cases, sur, lesquelles, se, trouvent, des, 'flèches', 'bleues.']]).

regle_rep(case, 6,
    [[case], 10, [montee]],
    [['Il', 's\'agit', des, cases, sur, lesquelles, se, trouvent, des, 'flèches', 'rouges.']]).

regle_rep(case, 7,
    [[nombre], 10, [case]],
    [['Ils', indiquent, le, sens, de, 'déplacement', des, coureurs, et, les, cases, 'numérotées', sont, prioritaires,
    sur, les, 'autres.', 'Il', faut, savoir, 'qu\'il', y, a, une, sous, 'priorité', dans, les, virages, en, suivant,
    'l\'ordre', des, lettres, de, 'l\'alphabet.']]).

regle_rep(case, 8,
    [[lettre], 10, [case]],
    [['Elles', indiquent, les, cases, de, virages, et, 'représentent', 'l\'ordre', de, 'priorité', dans, les,
    'virages.']]).

regle_rep(case, 9,
    [[case], 10, [coureur]],
    [['On', peut, 'déplacer', un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case, devant, en,
    'diagonale.']]).
% ---------------------------------------------------------------- %
regle_rep(descente, 1,
    [[avantage], 10, [descente]],
    [['Le', 'phénomène', 'd\'aspiration', en, descente, vous, permet, 'd\'obtenir', une, seconde, 'supplémentaire', 'à',
    celle, 'déjà', 'octroyée', et, vous, pouvez, doubler, le, coureur, devant, vous, de, maximum, 1, 'case.']]).

regle_rep(descente, 2,
    [[desavantage], 10, [descente]],
    [['Il', 'n\'y', a, pas, de, 'désavantage', en, 'descente.']]).
% ---------------------------------------------------------------- %
regle_rep(montee, 1,
    [[avantage], 10, [montee]],
    [['Il', 'n\'y', a, pas, 'd\'avantage', en, 'montée.']]).

regle_rep(montee, 2,
    [[desavantage], 10, [montee]],
    [['Les', cartes, seconde, "jouées", en, 'montée', sont, 'divisées', par, 2, et, il, 'n\'y', a, pas, de, 'phénomène',
    'd\'aspiration.']]).
% ---------------------------------------------------------------- %
regle_rep(aspiration, 1,
    [[quoi], 10, [aspiration]],
    [['Il', 's\'agit', 'd\'un', avantage, pour, un, coureur, quand, il, se, trouve, au, sein, 'd\'un', peloton, '(pas',
    en, 'tête)', ou, 'derrière', un, autre, 'coureur.', 'Si', son, 'déplacement', lui, permet, de, se, retrouver, aussi,
    au, sein, 'd\'un', 'peloton,', 'derrière', ou, 'à', 'côté', 'd\'un', autre, 'coureur,', il, 'bénéficie', 'd\'une',
    seconde, 'supplémentaire.', 'Attention,', en, cas, de, 'descente,', le, coureur, 'bénéficie', 'd\'un', 'supplément',
    de, 2, secondes, au, lieu, de, 1, 'seconde.']]).

regle_rep(aspiration, 2,
    [[aspiration], 10, [tete]],
    [['Non,', on, 'bénéficie', du, 'phénomène', 'd\'aspiration', seulement, au, sein, 'd\'un', peloton, '(pas', en,
    'tête)', ou, 'derrière', un, autre, 'coureur.']]).

regle_rep(aspiration, 3,
    [[aspiration], 10, [obligatoire]],
    [['Non,', il, 'n\'est', pas, 'obligatoire.']]).
% ---------------------------------------------------------------- %
regle_rep(vitesse, 1,
    [[quoi], 10, [vitesse]],
    [['Il', 's\'agit', 'd\'un', avantage, pour, un, coureur, quand, il, se, trouve, au, sein, 'd\'un', peloton, '(pas',
    en, 'tête)', ou, 'derrière', un, autre, 'coureur.', 'Si', son, 'déplacement', lui, permet, de, se, retrouver, aussi,
    au, sein, 'd\'un', 'peloton,', 'derrière', ou, 'à', 'côté', 'd\'un', autre, 'coureur,', il, 'bénéficie', 'd\'une',
    seconde, 'supplémentaire.', 'Attention,', en, cas, de, 'descente,', le, coureur, 'bénéficie', 'd\'un', 'supplément',
    de, 2, secondes, au, lieu, de, 1, 'seconde.']]).

regle_rep(vitesse, 3,
    [[vitesse], 10, [tete]],
    [['Non,', on, 'bénéficie', du, 'phénomène', de, prise, de, vitesse, seulement, au, sein, 'd\'un', peloton, '(pas',
    en, 'tête)', ou, 'derrière', un, autre, 'coureur.']]).

regle_rep(vitesse, 3,
    [[vitesse], 10, [obligatoire]],
    [['Non,', il, 'n\'est', pas, 'obligatoire.']]).
% ---------------------------------------------------------------- %
regle_rep(sprint, 1,
    [[quoi], 10, [sprint]],
    [['Il', 's\'agit', de, la, 'bannière', verte, sur, le, 'plateau.', 'Lorsqu\'un', ou, plusieurs, coureurs, passent,
    la, 'bannière,', ils, 'bénéficient', de, secondes, 'et/ou', de, points, de, 'bonification.', 'Ils', se, calculent,
    'à', la, fin, de, chaque, 'étape.', 'Les', secondes, de, bonification, sont, 'retirées', du, temps, du, coureur, et,
    les, points, de, bonification, sont, 'ajoutés', au, score, de, son, 'équipe.']]).

regle_rep(sprint, 2,
    [[avantage], 10, [sprint]],
    [['Il', permet, 'à', un, ou, plusieurs, coureurs, de, 'bénéficier', de, secondes, 'et/ou', de, points, de,
    'bonification.', 'Ils', se, calculent, 'à', la, fin, de, chaque, 'étape.', 'Les', secondes, de, bonification, sont,
    'retirées', du, temps, du, coureur, et, les, points, de, bonification, sont, 'ajoutés', au, score, de, son,
    'équipe.']]).

regle_rep(sprint, 3,
    [[desavantage], 10, [sprint]],
    [['Il', 'n\'y', a, pas, de, 'désavantage', lors, 'd\'un', 'sprint.']]).
% ---------------------------------------------------------------- %
regle_rep(chute, 1,
    [[quoi], 10, [chute]],
    [['Il', est, interdit, de, rentrer, en, contact, avec, 'd\'autres', coureurs, dans, le, jeu, mais, parfois, un,
    coureur, est, contraint, de, 's\'arrêter', sur, une, case, 'où', se, trouve, 'déjà', un, 'coureur.', 'Dans', ce,
    'cas,', il, provoque, automatiquement, une, chute, en, 'série.', 'Les', coureurs, 'concernés', doivent, se,
    'défausser', 'd\'une', carte, seconde, puis, passer, un, 'tour.', 'Une', chute, en, 'série', a, lieu, sur, toute,
    la, largeur, de, la, route, et, implique, les, coureurs, 'suivants :', le, coureur, ayant, 'provoqué', la, chute,
    en, 'série;', le, coureur, avec, lequel, il, rentre, en, 'contact;', tous, les, coureurs, 'situés', sur, les, cases,
    adjacentes, portant, le, 'même', 'numéro,', y, compris, les, lettres, 'A,', 'B,', 'C', 'etc.;', tous, les, coureurs,
    'qui,', lors, de, ce, tour, de, 'jeu,', ne, peuvent, pas, freiner, 'à', temps, et, arrivent, sur, une, des, cases,
    'affectées', par, 'l\'accident.']]).

regle_rep(chute, 2,
    [[provoque], 10, [chute]],
    [['Une', chute, en, 'série', est, 'provoquée', par, le, contact, entre, 2, 'coureurs,', 'c\'est-à-dire,', 'qu\'ils',
    's\'arrêtent', sur, la, 'même', 'case.']]).

regle_rep(chute, 3,
    [[chute], 15, [terre, plein]],
    [['Non,', elle, 'n\'affecte', pas, 'l\'autre', 'côté', du, terre, 'plein.']]).

regle_rep(chute, 4,
    [[faire], 10, [chute]],
    [['Il', faut, placer, les, coureurs, 'impliqués', sur, le, bord, de, la, route, 'à', 'l\'endroit', de,
    'l\'accident', et, les, placer, dans, 'l\'ordre', de, 'chute :', du, coureur, qui, a, 'provoqué', la, chute, au,
    coureur, qui, a, rejoint, plus, tard, le, lieu, de, 'l\'accident.', 'Une', fois, que, tous, ces, coureurs, auront,
    'passé', leur, 'tour,', ils, repartiront, dans, cet, 'ordre.']]).

regle_rep(chute, 5,
    [[eviter], 10, [chute]],
    [['Oui,', il, est, interdit, de, provoquer, une, chute, en, 'série', si, elle, est, 'évitable.']]).

regle_rep(chute, 6,
    [[case], 10, [chute], 10, [inevitable]],
    [['Il', est, obligatoire, de, se, placer, sur, la, case, vide, la, plus, 'éloignée', de, la, case, 'prioritaire.']]).
% ---------------------------------------------------------------- %
regle_rep(coureur, 1,
    [[coureur], 10, [case], 10, [occupe]],
    [['Non,', on, ne, peut, pas, 'déplacer', un, coureur, sur, une, case, 'déjà', 'occupée.']]).

regle_rep(coureur, 2,
    [[coureur], 10, [cote]],
    [['Non,', on, peut, seulement, 'déplacer', un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case,
    devant, en, 'diagonale.']]).

regle_rep(coureur, 3,
    [[coureur], 10, [arriere]],
    [['Non,', on, peut, seulement, 'déplacer', un, coureur, sur, la, case, directement, devant, lui, ou, sur, la, case,
    devant, en, 'diagonale.']]).

regle_rep(coureur, 4,
    [[plusieurs, coureur], 10, [tour]],
    [['Non,', on, ne, peut, 'déplacer', 'qu\'un', coureur, par, 'tour.']]).
% ---------------------------------------------------------------- %
regle_rep(jeu, 1,
    [[comment], 10, [commence], 10, [jeu]],
    [['Pour', commencer, le, 'jeu,', chaque, joueur, doit, tirer, X, cartes, 'seconde.', 'Celui', qui, tire, la, plus,
    grande, carte, commence, le, 'jeu.']]) :- nb_cartesSecondeCommence(X).

regle_rep(jeu, 2,
    [[case], 10, [commence], 10, [jeu]],
    [['Le', jeu, commence, sur, la, case, 'départ.']]).

regle_rep(jeu, 3,
    [[but], 10, [jeu]],
    [['Le', but, du, jeu, est, de, 'récolter', le, plus, de, 'points.']]).

regle_rep(jeu, 4,
    [[qui], 10, [commence], 10, [jeu]],
    [['C\'est', au, joueur, ayant, la, plus, haute, carte, seconde, de, 'commencer.']]).
% ---------------------------------------------------------------- %
regle_rep(maillot, 1,
    [[qui], 10, [maillot, jaune]],
    [['Le', coureur, le, plus, rapide, de, 'l\'ensemble', des, 'étapes', peut, porter, le, maillot, 'jaune.']]).
% ---------------------------------------------------------------- %
regle_rep(etape, 1,
    [[quoi], 10, [etape]],
    [['Une', 'étape', 'représente', un, parcours, de, 'plateau.']]).
% ---------------------------------------------------------------- %
regle_rep(accidente, 1,
    [[traverser], 10, [accidente]],
    [['Oui,', si, les, coureurs, 'accidentés', sont, sur, le, 'côté', de, la, 'route.']]).
% ---------------------------------------------------------------- %
regle_rep(cote, 2,
    [[depasser], 10, [bas, cote]],
    [['Oui,', uniquement, si, toute, la, largueur, de, la, route, est, 'occupée', par, des, coureurs, et, que, le,
    coureur, 'possède', assez, de, secondes, pour, passer, le, groupe, de, 'coureurs.']]).
% ---------------------------------------------------------------- %