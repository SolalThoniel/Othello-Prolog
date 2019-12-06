:- ensure_loaded('othello.pl').
:- dynamic compteurActuel/1.  
:- dynamic meilleurMove/1.  
:- dynamic compteurMove/1. 


choix_Mouvement(Board, Player, PlayerType, MouvementDirections) :-
	trouver_Mouvements(Board, Player, MouvList),
	once(choix_Mouvement_Directions(Board, Player, PlayerType, MouvList, MouvementDirections)).

	
%Teste si on a trouve des mouvements possibles ou non
choix_Mouvement_Directions(Board, Player, PlayerType, [], []).

%Utilise une des heuristiques pour trouver le mouvement suivant PlayerType
choix_Mouvement_Directions(Board, Player, PlayerType, MouvList, MouvementDirections) :- PlayerType == 2, heuristique_premier_mouv(Board, Player, MouvList, Mouvement),
directions_Mouvement(Board, Player, Mouvement, MouvementDirections).

choix_Mouvement_Directions(Board, Player, PlayerType, MouvList, MouvementDirections) :- PlayerType == 3, heuristique_mouv_aleatoire(Board, Player, MouvList, Mouvement),
directions_Mouvement(Board, Player, Mouvement, MouvementDirections).

choix_Mouvement_Directions(Board, Player, PlayerType, MouvList, MouvementDirections) :- PlayerType == 4, heuristique_max_jetons_retournes(Board, Player, MouvList, Mouvement),
directions_Mouvement(Board, Player, Mouvement, MouvementDirections).

choix_Mouvement_Directions(Board, Player, PlayerType, MouvList, MouvementDirections) :- PlayerType == 5, heuristique_mobilite(Board, Player, MouvList, Mouvement),
directions_Mouvement(Board, Player, Mouvement, MouvementDirections).

choix_Mouvement_Directions(Board, Player, PlayerType, MouvList, MouvementDirections) :- PlayerType == 6, heuristique_max_jetons_retournes_coins_et_bords(Board, Player, MouvList, Mouvement),
directions_Mouvement(Board, Player, Mouvement, MouvementDirections).

%Le once(length) force la list à choisir une taille et elimine les elements inconnu qui pourraits etre solution.
trouver_Mouvements(Board, Player, MouvList) :- once(recup_Mouv_possible(Board,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64], Player, MouvList)), once(length(MouvList, Taille)).


%Parcours de toutes les cases du plateau et test de l'ajout de la case comme coup possible
recup_Mouv_possible(Board, [], Player, MouvList).
recup_Mouv_possible(Board, [H|T], Player, MouvList) :- ajout_Mouv_possible(Board, H, Player, MouvList), recup_Mouv_possible(Board, T, Player, MouvList).


%Si la case testee est vide alors cela peut etre un coup possible
ajout_Mouv_possible(Board, Mouv, Player, MouvList) :- nth1(Mouv, Board, MouvCase), var(MouvCase), test_Mouv_possible(Board, Mouv, Player, MouvList).

%Si la case testee n'est pas vide, alors elle n'est pas exploitable pour un coup
ajout_Mouv_possible(Board, Mouv, Player, MouvList).


%on teste les parcours dans les 8 directions
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, -9),
test_premier_voisin(Board, Mouv, Player, MouvList, -8), test_premier_voisin(Board, Mouv, Player, MouvList, -7),
test_premier_voisin(Board, Mouv, Player, MouvList, -1), test_premier_voisin(Board, Mouv, Player, MouvList, 1),
test_premier_voisin(Board, Mouv, Player, MouvList, 7), test_premier_voisin(Board, Mouv, Player, MouvList, 8),
test_premier_voisin(Board, Mouv, Player, MouvList, 9).


%On regarde le premier voisin de la case du mouvement avant de lancer la recursion.
test_premier_voisin(Board, Mouv, Player, MouvList, Dir) :- nonSortis(Mouv, Dir), NouvPos is Mouv + Dir,
nth1(NouvPos, Board, NouvCase), nonvar(NouvCase), NouvCase\=Player, test_voisin_suivant(Board, NouvPos, Mouv, Player, MouvList, Dir).

test_premier_voisin(Board, Mouv, Player, MouvList, Dir).


%Sortie de la recursion en ayant trouve un mouvement
test_voisin_suivant(Board, Mouv, MouvOrigine, Player, MouvList, Dir) :- nonSortis(Mouv, Dir), NouvPos is Mouv + Dir,
nth1(NouvPos, Board, NouvCase), nonvar(NouvCase), NouvCase==Player, member(MouvOrigine, MouvList).

%Sortie de la recursion sans avoir trouve de mouvement
test_voisin_suivant(Board, Mouv, MouvOrigine, Player, MouvList, Dir) :- (not(nonSortis(Mouv, Dir))); ( NouvPos is Mouv + Dir,
nth1(NouvPos, Board, NouvCase), var(NouvCase) ).

%On continue la recursion
test_voisin_suivant(Board, Mouv, MouvOrigine, Player, MouvList, Dir) :- nonSortis(Mouv, Dir), NouvPos is Mouv + Dir,
nth1(NouvPos, Board, NouvCase), nonvar(NouvCase), NouvCase\=Player, test_voisin_suivant(Board, NouvPos, MouvOrigine, Player, MouvList, Dir).


%Test si on sort du plateau depuis un case vers une direction choisie
nonSortis(Mouv, Dir) :- Dir = -9, Mouv\=1, Mouv\=2, Mouv\=3, Mouv\=4, Mouv\=5, Mouv\=6, Mouv\=7, Mouv\=8, Mouv\=9, Mouv\=17, Mouv\=25, Mouv\=33, Mouv\=41, Mouv\=49, Mouv\=57.
nonSortis(Mouv, Dir) :- Dir = -8, Mouv\=1, Mouv\=2, Mouv\=3, Mouv\=4, Mouv\=5, Mouv\=6, Mouv\=7, Mouv\=8.
nonSortis(Mouv, Dir) :- Dir = -7, Mouv\=1, Mouv\=2, Mouv\=3, Mouv\=4, Mouv\=5, Mouv\=6, Mouv\=7, Mouv\=8, Mouv\=16, Mouv\=24, Mouv\=32, Mouv\=40, Mouv\=48, Mouv\=56, Mouv\=64.
nonSortis(Mouv, Dir) :- Dir = -1, Mouv\=1, Mouv\=9, Mouv\=17, Mouv\=25, Mouv\=33, Mouv\=41, Mouv\=49, Mouv\=57.
nonSortis(Mouv, Dir) :- Dir = 1, Mouv\=8, Mouv\=16, Mouv\=24, Mouv\=32, Mouv\=40, Mouv\=48, Mouv\=56, Mouv\=64.
nonSortis(Mouv, Dir) :- Dir = 7, Mouv\=1, Mouv\=9, Mouv\=17, Mouv\=25, Mouv\=33, Mouv\=41, Mouv\=49, Mouv\=57, Mouv\=58, Mouv\=59, Mouv\=60, Mouv\=61, Mouv\=62, Mouv\=63, Mouv\=64.
nonSortis(Mouv, Dir) :- Dir = 8, Mouv\=57, Mouv\=58, Mouv\=59, Mouv\=60, Mouv\=61, Mouv\=62, Mouv\=63, Mouv\=64.
nonSortis(Mouv, Dir) :- Dir = 9, Mouv\=57, Mouv\=58, Mouv\=59, Mouv\=60, Mouv\=61, Mouv\=62, Mouv\=63, Mouv\=64, Mouv\=8, Mouv\=16, Mouv\=24, Mouv\=32, Mouv\=40, Mouv\=48, Mouv\=56.






%Prend le premier mouvement possible
heuristique_premier_mouv(Board, Player, [H|T], H).





heuristique_mouv_aleatoire(Board, Player, MouvList, Mouvement):-random_member(Mouvement, MouvList).








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
heuristique_mobilite(Board, Player, MouvList, Mouvement) :- choix_Coup_Nbr_Mouvements(Board,Player,MouvList,MouvTemp,-100,Mouvement).


heuristique_Nbr_Mouvements(Board,Player1,Heuristique):-trouver_Mouvements(Board,Player1,List1),
	oppose(Player1,Player2),trouver_Mouvements(Board,Player2,List2),
	length(List1,N1),length(List2,N2),Heuristique is N1-N2.

% condition d'arret de l'ia qui maximise le differentiel du nombre de
% coups entre les deux joueurs
choix_Coup_Nbr_Mouvements(_,_,[],Coup,_,Coup).

% regle principale, joue le coup, récupère l'heuristique et garde la
% plus grande heuristique et son coup équivalent.
choix_Coup_Nbr_Mouvements(Board,Player,[H|T],Coup,Heuristique,CoupAbsolu):-directions_Mouvement(Board, Player, H, MouvementDirections),
 playMove(Board,MouvementDirections,NewBoard,Player),
 heuristique_Nbr_Mouvements(NewBoard,Player,HeuristiqueTMP),
 choix_Coup_Nbr_Mouvements_6(HeuristiqueTMP,Heuristique,Board,Player,T,H,Coup,CoupAbsolu).

%if else permettant de garder le coup ayant la plus grande heuristique.
choix_Coup_Nbr_Mouvements_6(HeuristiqueTMP,Heuristique,Board,Player,List,CoupTMP,Coup,CoupAbsolu):-HeuristiqueTMP>=Heuristique,
 choix_Coup_Nbr_Mouvements(Board,Player,List,CoupTMP,HeuristiqueTMP,CoupAbsolu).
%if else permettant de garder le coup ayant la plus grande heuristique.
choix_Coup_Nbr_Mouvements_6(HeuristiqueTMP,Heuristique,Board,Player,List,CoupTMP,Coup,CoupAbsolu):-HeuristiqueTMP<Heuristique,
 choix_Coup_Nbr_Mouvements(Board,Player,List,Coup,Heuristique,CoupAbsolu).

oppose(Player, Oppose) :- Player=='x',!, Oppose='o'.
oppose(Player, Oppose) :- Player=='o',!, Oppose='x'.




%H5 amélioration de H3 en priorisant les coups permettants de prendre un coin ou un bord, sans prendre un bord si l'adversaire peut prendre un coin ensuite.
heuristique_max_jetons_retournes_coins_et_bords(Board, Player, CoupsPossibles, Move) :- assert(meilleurMove(-404)), assert(compteurMove(-404)),
parcourirMouvementsPossibles2(Board, Player, CoupsPossibles),
retract(compteurMove(G)), retract(meilleurMove(X)), Move is X.


parcourirMouvementsPossibles2(Board, Player, []).

parcourirMouvementsPossibles2(Board, Player, [Coup|CoupsPossibles]) :- assert(compteurActuel(0)), directions_Mouvement(Board, Player, Coup, [Emplacement|MouvementDirections]),
compterPieces(Board, Emplacement, MouvementDirections, Player), testBord(Board, Emplacement, MouvementDirections, Player), actualiserMeilleurCoup(Coup), 
parcourirMouvementsPossibles2(Board, Player, CoupsPossibles).


testBord(Board, Emplacement, [], Player).
    testBord2(Board, Emplacement, [], Player).
    testBord(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [3,4,5,6,17,25,33,41,24,32,40,48,59,60,61,62], Emplacement), retract(compteurActuel(Y)), assert(compteurActuel(48)), testBord(Board, Emplacement, [], Player), !.
    testBord(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [1,8,57,64], Emplacement), retract(compteurActuel(Y)), assert(compteurActuel(50)), testBord(Board, Emplacement, [], Player), !.
    testBord(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), retract(compteurActuel(Y)), assert(compteurActuel(1)), 
            testBord2(Board, Emplacement, [Direction|Reste], Player).
    testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 2, 
            nth1(1, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player), !.
    testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 7, 
            nth1(8, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player), !.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 10, 
            nth1(1, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player), !.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- Emplacement == 9,
            nth1(1, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)),
            testBord2(Board, Emplacement, [], Player), !.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 15, 
            nth1(8, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player), !.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 50, 
            nth1(57, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player),!.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 16, 
            nth1(8, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player),!.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 49, 
            nth1(57, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player),!.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 58, 
            nth1(57, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player),!.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 56, 
            nth1(64, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player),!.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 55, 
            nth1(64, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player),!.
testBord2(Board, Emplacement, [Direction|Reste], Player) :- nth1(X, [2,7, 10,9,15, 50,16,49,58,56,55,63], Emplacement), X == 63, 
            nth1(64, Board, M), M == Player, retract(compteurActuel(Y)), assert(compteurActuel(49)), 
            testBord2(Board, Emplacement, [], Player),!.

    testBord(Board, Emplacement, [Direction|Reste], Player).
     testBord2(Board, Emplacement, [Direction|Reste], Player).





%Recupere les directions ou des pions vont etre retournes pour le mouvement
directions_Mouvement(Board, Player, Mouvement, MouvementDirections) :- once(tests_directions(Board, Player, Mouvement, Directions)), once(length(Directions, Taille)), ajouter_Tete(Mouvement, Directions, MouvementDirections).

ajouter_Tete(Tete, Liste, [Tete|Liste]).

tests_directions(Board, Player, Mouvement, Directions) :- test_premier_voisin_dir(Board, Player, Mouvement, Directions, -9),
test_premier_voisin_dir(Board, Player, Mouvement, Directions, -8), test_premier_voisin_dir(Board, Player, Mouvement, Directions, -7),
test_premier_voisin_dir(Board, Player, Mouvement, Directions, -1), test_premier_voisin_dir(Board, Player, Mouvement, Directions, 1),
test_premier_voisin_dir(Board, Player, Mouvement, Directions, 7), test_premier_voisin_dir(Board, Player, Mouvement, Directions, 8),
test_premier_voisin_dir(Board, Player, Mouvement, Directions, 9).


%On regarde le premier voisin de la case du mouvement avant de lancer la recursion.
test_premier_voisin_dir(Board, Player, Mouvement, Directions, Dir) :- nonSortis(Mouvement, Dir), NouvPos is Mouvement + Dir,
nth1(NouvPos, Board, NouvCase), nonvar(NouvCase), NouvCase\=Player, test_voisin_suivant_dir(Board, Player, NouvPos, Directions, Dir).

test_premier_voisin_dir(Board, Player, Mouvement, Directions, Dir).


%Sortie de la recursion en ayant trouve un mouvement
test_voisin_suivant_dir(Board, Player, Mouvement, Directions, Dir) :- nonSortis(Mouvement, Dir), NouvPos is Mouvement + Dir,
nth1(NouvPos, Board, NouvCase), nonvar(NouvCase), NouvCase==Player, member(Dir, Directions).

%Sortie de la recursion sans avoir trouve de mouvement
test_voisin_suivant_dir(Board, Player, Mouvement, Directions, Dir) :- (not(nonSortis(Mouvement, Dir))); ( NouvPos is Mouvement + Dir,
nth1(NouvPos, Board, NouvCase), var(NouvCase) ).

%On continue la recursion
test_voisin_suivant_dir(Board, Player, Mouvement, Directions, Dir) :- nonSortis(Mouvement, Dir), NouvPos is Mouvement + Dir,
nth1(NouvPos, Board, NouvCase), nonvar(NouvCase), NouvCase\=Player, test_voisin_suivant_dir(Board, Player, NouvPos, Directions, Dir).



