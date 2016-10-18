% Auteur: Équipe 6 - Gilles Portelance, Thierry Bellevue
% Jeu du plus grand, plus petit
% Date: 2016-10-14

% jeu.pl

:- use_module(deck).

par_paquet(8).

% Retourne la sauvegarde du jeu
retourner_sauvegarde(ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable) :-
    sauvegarde_jeu([ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable]).
    
% Place la première carte sur la table du jeu
premiere_carte_sur_table(Carte) :-
              deck:paquetrestant(PaquetRestant),
              random_member(Carte, PaquetRestant),
              delete(PaquetRestant, Carte, NouveauPaquetRestant),
              assert(cartesurtable(Carte)),
              retract(deck:paquetrestant(_)),
              assert(deck:paquetrestant(NouveauPaquetRestant)).
              
% Efface toutes les données sauvegardées - Refactor, dispose
redemarrer_jeu(_) :-
    retractall(deck:paquetjoueur1(_)),
    retractall(deck:paquetjoueur2(_)),
    retractall(deck:paquetrestant(_)),
    retractall(cartesurtable(_)),
    retractall(sauvegarde_jeu(_)).
    
% Démarrer le jeu - Création des paquets, et mise en place de la carte sur table
demarrer_jeu :-
    redemarrer_jeu(_),
    par_paquet(ParPaquet),
    distribuer_cartes(ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant),
    premiere_carte_sur_table(CarteSurTable),
    assert(sauvegarde_jeu([ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable])),
    write("************************* DÉMARRAGE DU JEU *********************************"),
    nl,
    write("L'adversaire a 8 cartes en main"), nl,
    write("Sur la table : "), afficher_carte(CarteSurTable), nl,
    write("Votre main : "), afficher_paquet(PaquetJoueur1),
    nl,
    write("**********************************************************"),
    nl,
    write("Voulez-vous déposer une carte? Exemple: deposer_carte(2, trefle) ou piger_une_carte?").
    
    
deposer_carte(Carte) :- deck:paquetjoueur1(P),
                        member(Carte, P),
                        (cartesurtable(C), carte_plus_grande(C, Carte);  cartesurtable(C), carte_plus_petite(C, Carte)),
                        write("La nouvelle carte sur table est:"), nl, afficher_carte(Carte).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exemples d'utilisation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ?- redemarrer_jeu(_).
% True.
%%
% ?- demarrer_jeu(8, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable).
% PaquetJoueur1 = [carte(3, coeur), carte(3, trefle), carte(d, carreau), carte(4, coeur), carte(a, carreau), carte(9, carreau), carte(8, carreau), carte(7, carreau)],
% PaquetJoueur2 = [carte(5, pique), carte(v, trefle), carte(10, trefle), carte(7, pique), carte(10, carreau), carte(7, trefle), carte(5, trefle), carte(2, carreau)],
% NbCartesRestantes = 38,
% PaquetRestant = [carte(j, j), carte(j, j), carte(2, coeur), carte(2, pique), carte(2, trefle), carte(3, carreau), carte(3, pique), carte(4, carreau), carte(..., ...)|...],
% CarteSurTable = carte(2, pique) ;
%%
% ?- cartesurtable(N).
% N = carte(2, pique) ;
%%
% ?- paquetjoueur1(P).
% P = [carte(3, coeur), carte(3, trefle), carte(d, carreau), carte(4, coeur), carte(a, carreau), carte(9, carreau), carte(8, carreau), carte(7, carreau)] ;
%%
% ?- paquetjoueur2(P).
% P = [carte(5, pique), carte(v, trefle), carte(10, trefle), carte(7, pique), carte(10, carreau), carte(7, trefle), carte(5, trefle), carte(2, carreau)] ;
%%
% ?- carte_plus_grande(carte(j,j), carte(2, carreau)).
% True.
%%
% ?- carte_plus_grande(carte(2, carreau), carte(j, j)).
% False.
%%
