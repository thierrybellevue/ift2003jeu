% Auteur: Équipe 6 - Sandra Bessette, Gilles Portelance, Thierry Bellevue
% Jeu du plus grand, plus petit
% Date: 2016-10-14


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicats necessaires au jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Attribution des valeurs pour chacune des cartes
valeur(a, 1).
valeur(2, 2).
valeur(3, 3).
valeur(4, 4).
valeur(5, 5).
valeur(6, 6).
valeur(7, 7).
valeur(8, 8).
valeur(9, 9).
valeur(10, 10).
valeur(v, 11).
valeur(d, 12).
valeur(r, 13).
valeur(j, 99).

% Création du paquet (Deck) principal
nouveau_paquet(Paquet) :-
        Suites = [trefle, coeur, pique, carreau],
        Cartes = [2, 3, 4, 5, 6, 7, 8, 9, 10, v, d, r, a],
        setof(carte(Carte, Suite), (member(Suite, Suites), member(Carte, Cartes)), A),
        append([carte(j, j), carte(j, j)], A, Paquet).


% Piger une carte dans un paquet quelconque
piger_une_carte(X, Paquet) :-  random_member(X, Paquet).

% Création d'un paquet (PaquetFinal) d'un certain nombre de cartes (Compte) à partir d'un autre paquet (PaquetDebut) ! Sélection au hasard !
creer_paquet(Compte, PaquetDebut, PaquetFinal) :-
  random_permutation(PaquetDebut, Brouille),
  append(PaquetFinal, _, Brouille),
  length(PaquetFinal, Compte).

% Distribuer les cartes ParPaquet = 8
distribuer_cartes(ParPaquet, Joueur1, Joueur2, NbCartesRestantes, PaquetRestant) :-
      nouveau_paquet(PaquetDebut),
      creer_paquet(ParPaquet, PaquetDebut, Joueur1),
      subtract(PaquetDebut, Joueur1, Resultat),
      creer_paquet(ParPaquet, Resultat, Joueur2),
      subtract(Resultat, Joueur2, PaquetRestant),
      length(PaquetRestant, NbCartesRestantes),
      assert(paquetjoueur1(Joueur1)),
      assert(paquetjoueur2(Joueur2)),
      assert(paquetrestant(PaquetRestant)).

% Retourne la sauvegarde du jeu
retourner_sauvegarde(ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable) :-
    sauvegarde_jeu([ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable]).
    
% Place la première carte sur la table du jeu
premiere_carte_sur_table(Carte) :-
              paquetrestant(PaquetRestant),
              random_member(Carte, PaquetRestant),
              delete(PaquetRestant, Carte, NouveauPaquetRestant),
              assert(cartesurtable(Carte)),
              retract(paquetrestant(_)),
              assert(paquetrestant(NouveauPaquetRestant)).
              
% Efface toutes les données sauvegardées - Refactor, dispose
redemarrer_jeu(_) :-
    retractall(paquetjoueur1(_)),
    retractall(paquetjoueur2(_)),
    retractall(paquetrestant(_)),
    retractall(cartesurtable(_)),
    retractall(sauvegarde_jeu(_)).
    
% Démarrer le jeu - Création des paquets, et mise en place de la carte sur table
demarrer_jeu(ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable) :-
    redemarrer_jeu(_),
    distribuer_cartes(ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant),
    premiere_carte_sur_table(CarteSurTable),
    assert(sauvegarde_jeu([ParPaquet, PaquetJoueur1, PaquetJoueur2, NbCartesRestantes, PaquetRestant, CarteSurTable])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicats de service
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Vérification d'une carte plus grande qu'une autre
carte_plus_grande(carte(X,_), carte(Y,_)) :-
               valeur(X, A),
               valeur(Y, B),
               A > B.

% Vérification d'une carte plus petite qu'une autre
carte_plus_petite(carte(X,_), carte(Y,_)) :-
               valeur(X, A),
               valeur(Y, B),
               A < B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicats non utilisés pour le moment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sélectionner une carte au hasard (Dans un nouveau paquet) - Pas utilisé dans la méthode principale
piger_une_carte(X) :- nouveau_paquet(CI), random_member(X, CI).

% Carte !!! Pas encore utilisé !!!
carte(Suite, Rang, Valeur) :-
        suites(Suite), rang(Rang), valeur(Valeur).
        
% Distribuer toutes les cartes entre deux joueurs !!! Pas utilisé !!!
distribuer_toutes_cartes([],[],[]).
distribuer_toutes_cartes([P1,P2|Paquet],[P1|J1],[P2|J2]) :-
    dealer(Paquet,J1,J2).

% Distribuer toutes les cartes entre deux joueurs !!! Pas utilisé !!!
partager_cartes(Joueur1,Joueur2) :-
    nouveau_paquet(Paquet),
    distribuer_toutes_cartes(Paquet, Joueur1, Joueur2).
    
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
