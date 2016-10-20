% Auteur: Équipe 6 - Gilles Portelance, Thierry Bellevue
% Jeu du plus grand, plus petit
% Date: 2016-10-17

% deck.pl

:- module(deck,[  valeur/2,
                  nouveau_paquet/1, % -Paquet,
                  piger_une_carte/2, % -Carte, -Paquet
                  creer_paquet/3, % +Compte, -PaquetDebut, -PaquetFinal
                  distribuer_cartes/5, % +ParPaquet, -Joueur1, -Joueur2, -NbCartesRestantes, -PaquetRestant
                  carte_plus_grande/2, % +Carte, +Carte
                  carte_plus_petite/2, % +Carte, +Carte
                  carte_meme_suite/2, % +Carte, +Carte
                  afficher_carte/1, % Carte
                  afficher_paquet/1, % Paquet
                  piger_une_carte/1 % Carte
                 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicats necessaires au jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Attribution des valeurs pour chacune des cartes
valeur(as, 1).
valeur(2, 2).
valeur(3, 3).
valeur(4, 4).
valeur(5, 5).
valeur(6, 6).
valeur(7, 7).
valeur(8, 8).
valeur(9, 9).
valeur(10, 10).
valeur(valet, 11).
valeur(dame, 12).
valeur(roi, 13).
valeur(joker, 99).

% Création du paquet (Deck) principal
nouveau_paquet(Paquet) :-
        Suites = [carreau, coeur, trefle, pique],
        Cartes = [2, 3, 4, 5, 6, 7, 8, 9, 10, valet, dame, roi, as],
        setof(carte(Carte, Suite), (member(Suite, Suites), member(Carte, Cartes)), A),
        append([carte(joker, joker), carte(joker, joker)], A, Paquet).

une_carte(X, Y, M) :- M = carte(X, Y).

% Piger une carte dans un paquet quelconque
piger_une_carte(X, Paquet) :-  random_member(X, Paquet).

% Création d'un paquet (PaquetFinal) d'un certain nombre de cartes (Compte) à partir d'un autre paquet (PaquetDebut) ! Sélection au hasard !
creer_paquet(Compte, PaquetDebut, PaquetFinal) :-
  random_permutation(PaquetDebut, Brouille),
  append(PaquetFinal, _, Brouille),
  length(PaquetFinal, Compte).

% Distribuer les cartes ParPaquet = 8
distribuer_cartes(ParPaquet, Joueur1, Joueur2, NbCartesRestantes, PaquetRestant) :-
      nouveau_paquet(P1),
      nouveau_paquet(P2),
      append(P1, P2, PaquetDebut),
      creer_paquet(ParPaquet, PaquetDebut, Joueur1),
      subtract(PaquetDebut, Joueur1, Resultat),
      creer_paquet(ParPaquet, Resultat, Joueur2),
      subtract(Resultat, Joueur2, PaquetRestant),
      length(PaquetRestant, NbCartesRestantes),
      assert(paquetjoueur1(Joueur1)),
      assert(paquetjoueur2(Joueur2)),
      assert(paquetrestant(PaquetRestant)).
      
% afficher_carte(+Card)
afficher_carte(carte(Carte, Suite)) :-
        format('~a de ~a', [Carte, Suite]).
        
% afficher le paquet du joueur
afficher_paquet([]).
afficher_paquet([X|List]) :- afficher_carte(X), !, write(", "), afficher_paquet(List).
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicats de service
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Vérification d'une carte plus grande qu'une autre
carte_plus_grande(carte(X,_), carte(Y,_)) :-
               valeur(X, A),
               valeur(Y, B),
               (A =:= (B + 1); A =:= 13, B =:= 1; A =:= 99; B =:= 99; A =:= B).

% Vérification d'une carte plus petite qu'une autre
carte_plus_petite(carte(X,_), carte(Y,_)) :-
               valeur(X, A),
               valeur(Y, B),
               (A =:= (B - 1); A =:= 1, B =:= 13; A =:= 99; B =:= 99; A =:= B).
               
% Vérification d'une carte même suite
carte_meme_suite(carte(_,X), carte(_,Y)) :- X = Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicats non utilisés pour le moment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sélectionner une carte au hasard (Dans un nouveau paquet) - Pas utilisé dans la méthode principale
piger_une_carte(X) :- paquetrestant(P),
                      length(P, Compte),
                      (Compte =:= 0,
                      write("Il n'y a plus de carte à piger."), nl,
                      paquetjoueur1(P1),
                      paquetjoueur2(P2),
                      length(P1, Compte1),
                      length(P2, Compte2),
                      (Compte1 > Compte2, write("Vous avez gagné."); Compte1 < Compte2, write("L'adversaire a gagné"); Compte1 =:= Compte2, write("Ex-aequo")), nl;
                      random_member(X, P),
                      delete(P, X, NouveauPaquet),
                      retract(paquetrestant(_)),
                      assert(paquetrestant(NouveauPaquet))).

% Carte !!! Pas encore utilisé !!!
%carte(Suite, Rang, Valeur) :-
%        suites(Suite), rang(Rang), valeur(Valeur).

% Distribuer toutes les cartes entre deux joueurs !!! Pas utilisé !!!
distribuer_toutes_cartes([],[],[]).
distribuer_toutes_cartes([P1,P2|Paquet],[P1|J1],[P2|J2]) :-
    dealer(Paquet,J1,J2).

% Distribuer toutes les cartes entre deux joueurs !!! Pas utilisé !!!
partager_cartes(Joueur1,Joueur2) :-
    nouveau_paquet(Paquet),
    distribuer_toutes_cartes(Paquet, Joueur1, Joueur2).