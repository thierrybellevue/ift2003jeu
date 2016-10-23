% Auteur: Équipe 6 - Gilles Portelance, Thierry Bellevue
% Jeu du plus grand, plus petit
% Date: 2016-10-14

% jeu.pl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exemples d'utilisation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pour démarrer le jeu
% ?- demarrer_jeu.
%
%% Pour déposer une carte
% ?- deposer_carte(7, pique).
%
%% Pour piger une carte
% ?- piger_une_carte.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(deck).

par_paquet(8).

% Ajout dans une liste
add(X, [], [X]).
add(X, Y, Z) :- Z = [X|Y].

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
redemarrer_jeu :-
    retractall(deck:paquetjoueur1(_)),
    retractall(deck:paquetjoueur2(_)),
    retractall(deck:paquetrestant(_)),
    retractall(cartesurtable(_)),
    retractall(sauvegarde_jeu(_)).
    
% Démarrer le jeu - Création des paquets, et mise en place de la carte sur table
demarrer_jeu :-
    redemarrer_jeu,
    par_paquet(ParPaquet),
    distribuer_cartes(ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant),
    premiere_carte_sur_table(CarteSurTable),
    assert(sauvegarde_jeu([ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable])),
    write("************************* DÉMARRAGE DU JEU *********************************"),
    nl,
    write("L'adversaire a 8 cartes en main"), nl,
    afficher_carte_sur_table,
    afficher_ma_main,
    nl,
    write("**********************************************************"),
    nl,
    write("Voulez-vous déposer une carte? Exemple: deposer_carte(2, trefle) ou piger_une_carte?"), nl.

% Permet d'afficher la carte sur table
afficher_carte_sur_table :- cartesurtable(C), write("Sur la table : "), afficher_carte(C), nl.

% Permet d'afficher la main du joueur
afficher_ma_main :-
    write("Votre main : "), deck:paquetjoueur1(P), afficher_paquet(P).

% Permet d'afficher la main de l'adversaire
afficher_sa_main :-
    write("La main de votre adversaire : "), deck:paquetjoueur2(P), afficher_paquet(P).

% Permer de déposer une carte
deposer_carte(X, Y) :- deck:paquetjoueur1(P),
                        deck:une_carte(X, Y, Carte),
                        member(Carte, P),
                        (cartesurtable(C), carte_plus_grande(C, Carte);  cartesurtable(C), carte_plus_petite(C, Carte)),
                        delete(P, Carte, NouveauPaquet),
                        retract(cartesurtable(_)),
                        assert(cartesurtable(Carte)),
                        retract(deck:paquetjoueur1(_)),
                        assert(deck:paquetjoueur1(NouveauPaquet)),
                        length(NouveauPaquet, Compte),
                        (Compte =:= 0, write("Vous avez gagné."), nl,
                        write("Partie terminée!"),
                        redemarrer_jeu;
                        write("Vous avez maintenant "), write(Compte), write(" carte(s) en main."), nl,
                        write("La nouvelle carte sur table est:"), nl, afficher_carte(Carte), nl,
                        adversaire_joue, nl).

% L'adversaire dépose une carte sur la table
adversaire_depose_carte(Carte) :- deck:paquetjoueur2(P),
                        delete(P, Carte, NouveauPaquet),
                        retract(cartesurtable(_)),
                        assert(cartesurtable(Carte)),
                        retract(deck:paquetjoueur2(_)),
                        assert(deck:paquetjoueur2(NouveauPaquet)),
                        length(NouveauPaquet, Compte),
                        (Compte =:= 0, write("L'adversaire a gagné."), nl,
                        write("Partie terminée!"),
                        redemarrer_jeu;
                        write("L'adversaire dépose une carte."), nl, write("Il a maintenant "), write(Compte), write(" carte(s) en main."), nl,
                        %afficher_sa_main,
                        write("La nouvelle carte sur table est:"), nl, afficher_carte(Carte), nl,
                        afficher_ma_main).
                        
% L'adversaire joue son tour
adversaire_joue :- deck:paquetjoueur2(P),
                   trouver_une_carte_a_jouer(P).

% Piger une carte
piger_une_carte :- deck:piger_une_carte(Carte), !,
                   deck:paquetrestant(P),
                   length(P, ComptePaquetRestant),
                   (ComptePaquetRestant =:= 0,
                   write("Partie terminée!"),nl;
                   write("Il reste "), write(ComptePaquetRestant), write(" carte(s) à piger."), nl,
                   deck:paquetjoueur1(Paquet),
                   add(Carte, Paquet, NouveauPaquet),
                   retract(deck:paquetjoueur1(_)),
                   assert(deck:paquetjoueur1(NouveauPaquet)),
                   length(NouveauPaquet, Compte),
                   write("Vous avez pigé une carte."), nl, write("Vous avez maintenant "), write(Compte), write(" carte(s) en main."), nl,
                   afficher_ma_main, nl,
                   cartesurtable(CarteSurTable),
                   write("Sur la table : "), afficher_carte(CarteSurTable), nl,
                   adversaire_joue, nl).

% Adversaire pige une carte
adversaire_pige_carte :- deck:piger_une_carte(Carte), !,
                         deck:paquetrestant(P),
                         length(P, ComptePaquetRestant),
                         (ComptePaquetRestant =:= 0,
                         write("Partie terminée!"),nl,
                         redemarrer_jeu;
                         write("Il reste "), write(ComptePaquetRestant), write(" carte(s) à piger."), nl,
                         deck:paquetjoueur2(Paquet),
                         add(Carte, Paquet, NouveauPaquet),
                         retract(deck:paquetjoueur2(_)),
                         assert(deck:paquetjoueur2(NouveauPaquet)),
                         length(NouveauPaquet, Compte),
                         write("L'adversaire a pigé une carte."), nl, write("Il a maintenant "), write(Compte), write(" carte(s) en main."), nl,
                         afficher_sa_main,
                         cartesurtable(CarteSurTable),
                         write("Sur la table : "), afficher_carte(CarteSurTable), nl,
                         afficher_ma_main).

% Permet à l'adversaire de trouver une carte à jouer
trouver_une_carte_a_jouer([Carte|Paquet]) :-
                                      (((cartesurtable(C), carte_plus_grande(C, Carte);  cartesurtable(C), carte_plus_petite(C, Carte)),
                                      adversaire_depose_carte(Carte);
                                      trouver_une_carte_a_jouer(Paquet));
                                      adversaire_pige_carte).
