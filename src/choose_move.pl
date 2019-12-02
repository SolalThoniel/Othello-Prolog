

choix_Mouvement(Board, Player, MouvementDirections) :-
	trouver_Mouvements(Board, Player, MouvList),
	evaluer_Mouvements(Board, Player, MouvList, Mouvement),
	directions_Mouvement(Board, Player, Mouvement, MouvementDirections).
	

%Le once(length) force la list à choisir une taille et elimine les elements inconnu qui pourraits etre solution.
trouver_Mouvements(Board, Player, MouvList) :- once(recup_Mouv_possible(Board,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64], Player, MouvList)), once(length(MouvList, Taille)).

%Parcours de toutes les cases du plateau et test de l'ajout de la case comme coup possible
recup_Mouv_possible(Board, [], Player, MouvList).
recup_Mouv_possible(Board, [H|T], Player, MouvList) :- ajout_Mouv_possible(Board, H, Player, MouvList), recup_Mouv_possible(Board, T, Player, MouvList).


%Si la case testee est vide alors cela peut etre un coup possible
ajout_Mouv_possible(Board, Mouv, Player, MouvList) :- nth1(Mouv, Board, MouvCase), MouvCase=='v', test_Mouv_possible(Board, Mouv, Player, MouvList).

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
nth1(NouvPos, Board, NouvCase), NouvCase\='v', NouvCase\=Player, test_voisin_suivant(Board, NouvPos, Mouv, Player, MouvList, Dir).

test_premier_voisin(Board, Mouv, Player, MouvList, Dir).


%Sortie de la recursion en ayant trouve un mouvement
test_voisin_suivant(Board, Mouv, MouvOrigine, Player, MouvList, Dir) :- nonSortis(Mouv, Dir), NouvPos is Mouv + Dir,
nth1(NouvPos, Board, NouvCase), NouvCase\='v', NouvCase==Player, member(MouvOrigine, MouvList).

%Sortie de la recursion sans avoir trouve de mouvement
test_voisin_suivant(Board, Mouv, MouvOrigine, Player, MouvList, Dir) :- (not(nonSortis(Mouv, Dir))); ( NouvPos is Mouv + Dir,
nth1(NouvPos, Board, NouvCase), NouvCase=='v' ).

%On continue la recursion
test_voisin_suivant(Board, Mouv, MouvOrigine, Player, MouvList, Dir) :- nonSortis(Mouv, Dir), NouvPos is Mouv + Dir,
nth1(NouvPos, Board, NouvCase), NouvCase\='v', NouvCase\=Player, test_voisin_suivant(Board, NouvPos, MouvOrigine, Player, MouvList, Dir).


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
evaluer_Mouvements(Board, Player, [H|T], H).




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
nth1(NouvPos, Board, NouvCase), NouvCase\='v', NouvCase\=Player, test_voisin_suivant_dir(Board, Player, NouvPos, Directions, Dir).

test_premier_voisin_dir(Board, Player, Mouvement, Directions, Dir).


%Sortie de la recursion en ayant trouve un mouvement
test_voisin_suivant_dir(Board, Player, Mouvement, Directions, Dir) :- nonSortis(Mouvement, Dir), NouvPos is Mouvement + Dir,
nth1(NouvPos, Board, NouvCase), NouvCase\='v', NouvCase==Player, member(Dir, Directions).

%Sortie de la recursion sans avoir trouve de mouvement
test_voisin_suivant_dir(Board, Player, Mouvement, Directions, Dir) :- (not(nonSortis(Mouvement, Dir))); ( NouvPos is Mouvement + Dir,
nth1(NouvPos, Board, NouvCase), NouvCase=='v' ).

%On continue la recursion
test_voisin_suivant_dir(Board, Player, Mouvement, Directions, Dir) :- nonSortis(Mouvement, Dir), NouvPos is Mouvement + Dir,
nth1(NouvPos, Board, NouvCase), NouvCase\='v', NouvCase\=Player, test_voisin_suivant_dir(Board, Player, NouvPos, Directions, Dir).



