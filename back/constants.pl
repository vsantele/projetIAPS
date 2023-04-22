:- module(constants, [nb_etapes/1, nb_coureurs/1, nb_equipes/1, nb_cartesSecondeCommence/1, nb_cartesSecondeTirer/1, carteSecondePlusBasse/1, carteSecondePlusHaute/1, nb_cartesSecondeRecurrence/1, nb_cartesSecondeTotale/1, val_max_carte/1, val_min_carte/1, nb_repetition_cartes/1, val_chance_min/1, val_chance_max/1 ]).

nb_etapes(2).
nb_coureurs(3).
nb_equipes(4).
nb_cartesSecondeCommence(5).
nb_cartesSecondeTirer(5).
carteSecondePlusBasse(1).
carteSecondePlusHaute(12).
nb_cartesSecondeRecurrence(8).
nb_cartesSecondeTotale(Total) :- nb_repetition_cartes(NbRepet), val_min_carte(Min), val_max_carte(Max), NbCartes is 1+ (Max - Min), Total is NbCartes * NbRepet.

val_min_carte(1).
val_max_carte(12).
nb_repetition_cartes(8).

val_chance_min(-3).
val_chance_max(3).