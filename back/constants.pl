:- module(constants, [nb_etapes/1, nb_coureurs/1, nb_equipes/1, nb_cartesSecondeCommence/1, nb_cartesSecondeTirer/1, nb_cartesSecondeRecurrence/1, nb_cartesSecondeTotale/1, val_max_carte/1, val_min_carte/1, nb_repetition_cartes/1, val_chance_min/1, val_chance_max/1, val_penalite_par_tour/1 ]).

nb_etapes(2). % Nombre d'étapes
nb_coureurs(3). % Nombre de coureurs
nb_equipes(4). % Nombre d'équipes
nb_cartesSecondeCommence(5). % Nombre de carte seconde par équipe au démarrage
nb_cartesSecondeTirer(5). % Nombre de carte seconde par équipe lors de la pioche
nb_cartesSecondeTotale(Total) :- nb_repetition_cartes(NbRepet), val_min_carte(Min), val_max_carte(Max), NbCartes is 1+ (Max - Min), Total is NbCartes * NbRepet.

val_min_carte(1). % valeur de la carte seconde la plus petite
val_max_carte(12). % valeur de la carte seconde la plus grande
nb_repetition_cartes(8). % Nombre d'occurence de chaque carte

val_chance_min(-3). % valeur de la carte chance la plus petite
val_chance_max(3). % valeur de la carte chance la plus grande

val_penalite_par_tour(10).