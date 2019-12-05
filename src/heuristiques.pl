:- ensure_loaded('choose_move.pl').
:- dynamic compteurActuel/1.  
:- dynamic meilleurMove/1.  
:- dynamic compteurMove/1.  


%H1 : Prend le premier mouvement possible
heuristique_premier_mouv(Board, Player, [H|T], H).



%H2 : Joue un coup possible aleatoirement
heuristique_mouv_aleatoire(Board, Player, MouvList, Mouvement):-random_member(Mouvement, MouvList).


%H3 : Joue le coup qui retourne le maximum de jetons, en cas d'egalite, retourne le premier. 
heuristique_max_jetons_retournes(Board, Player, MouvList, Mouvement) :- assert(meilleurMove(-404)), assert(compteurMove(-404)),
parcourirMouvementsPossibles(Board, Player, MouvList),
retract(compteurMove(G)), retract(meilleurMove(X)), Mouvement is X.

%%%% Parcour les différents mouvements possibles

parcourirMouvementsPossibles(Board, Player, []).
parcourirMouvementsPossibles(Board, Player, [Coup|CoupsPossibles]) :- assert(compteurActuel(0)), directions_Mouvement(Board, Player, Coup, [Emplacement|MouvementDirections]),
compterPieces(Board, Emplacement, MouvementDirections, Player), actualiserMeilleurCoup(Coup), 
parcourirMouvementsPossibles(Board, Player, CoupsPossibles).

%%%% Compare le nombre de jetons retournés en mémoire et celui actuel puis met à jour le meilleur coup.

actualiserMeilleurCoup(Move) :- retract(compteurActuel(X)), retract(compteurMove(Y)), assert(compteurActuel(X)), assert(compteurMove(Y)),  X =< Y,
retract(compteurActuel(X)), !.
actualiserMeilleurCoup(Move) :- retract(compteurActuel(X)), retract(compteurMove(Y)), X > Y, retract(meilleurMove(_)), assert(meilleurMove(Move)), assert(compteurMove(X)), 
!.

%%%% Ajoute 1 à la variable somme
somme :- retract(compteurActuel(X)), Variable is X+1, assert(compteurActuel(Variable)).


%%%% Compte les pieces retournee en parcourant les directions possibles

compterPieces(Board, Emplacement, [], Player).
compterPieces(Board, Emplacement, [Direction|Reste], Player) :- 
compterPiecesDirection(Board, Emplacement, Direction, Player),
compterPieces(Board, Emplacement, Reste, Player).


%%%% Compte les pieces retournee dans une directions precise

compterPiecesDirection(Board, Emplacement, Direction, Player):- Nemplacement is Emplacement+Direction, 
nth1(Nemplacement, Board, Valeur), Valeur == Player, !.
compterPiecesDirection(Board, Emplacement, Direction, Player):- Nemplacement is Emplacement+Direction,
nth1(Nemplacement, Board, Valeur), Valeur \== Player, somme,
compterPiecesDirection(Board, Nemplacement, Direction, Player).



%H4 : Joue le coup qui minimise les coups possibles pour l'adversaire et maximise les coups possibles pour le joueur
heuristique_mobilite(Board, Player, [H|T], H).