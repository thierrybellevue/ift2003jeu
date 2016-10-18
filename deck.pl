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
                  afficher_carte/1, % Carte
                  afficher_paquet/1 % Paquet
                 ]).

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
        Suites = [carreau, coeur, trefle, pique],
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