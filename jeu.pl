% Auteur: Équipe 6 - Sandra Bessette, Gilles Portelance, Thierry Bellevue
% Jeu du plus grand, plus petit
% Date: 2016-10-14

% Nouveau paquet
nouveau_paquet(Paquet) :-
        Suites = [trefle, coeur, pique, carreau],
        Cartes = [2, 3, 4, 5, 6, 7, 8, 9, 10, v, d, r, a],
        setof(carte(Carte, Suite), (member(Suite, Suites), member(Carte, Cartes)), A),
        append([carte(j, j), carte(j, j)], A, Paquet).

% Carte
carte(Suite, Rang, Valeur) :-
        suites(Suite), rang(Rang), valeur(Valeur).

% Vérification si la carte A est plus grande que la carte B
carte_plus_grande(carte(X,_), carte(Y,_)) :-
               valeur(X, A),
               valeur(Y, B),
               A > B.

% Vérification si la carte A est plus petite que la carte B
carte_plus_petite(carte(X,_), carte(Y,_)) :-
               valeur(X, A),
               valeur(Y, B),
               A < B.

% Sélectionner une carte au hasard
piger_une_carte(X) :- nouveau_paquet(CI), random_member(X, CI).

% Piger une carte dans un paquet
piger_une_carte(X, Paquet) :-  random_member(X, Paquet).

% Sélectionner
creer_paquet(Compte, PaquetDebut, PaquetFinal) :-
  random_permutation(PaquetDebut, Brouille),
  append(PaquetFinal, _, Brouille),
  length(PaquetFinal, Compte).

% Distribuer les cartes ParPaquet = 8
distribuer_cartes(ParPaquet, Joueur1, Joueur2, NbCartesRestantes) :-
      nouveau_paquet(PaquetDebut),
      creer_paquet(ParPaquet, PaquetDebut, Joueur1),
      subtract(PaquetDebut, Joueur1, R),
      creer_paquet(ParPaquet, R, Joueur2),
      subtract(R, Joueur2, F),
      length(F, NbCartesRestantes).

% Distribuer toutes les cartes entre deux joueurs
distribuer_toutes_cartes([],[],[]).
distribuer_toutes_cartes([P1,P2|Paquet],[P1|J1],[P2|J2]) :-
    dealer(Paquet,J1,J2).

% Distribuer toutes les cartes entre deux joueurs
partager_cartes(Joueur1,Joueur2) :-
    nouveau_paquet(Paquet),
    distribuer_toutes_cartes(Paquet, Joueur1, Joueur2).
