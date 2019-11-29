

choix_Mouvement(Board, Player, Mouvement) :-
	trouver_Mouvements(Board, Player, MouvList),
	evaluer_Mouvements(Board, Player, MouvList, Mouvement).


trouver_Mouvements(Board, Player, MouvList).
	
%Parcours de toutes les cases du plateau et test de l'ajout de la case comme coup possible
recup_Mouv_possible(Board, [], Player, MouvList).
recup_Mouv_possible(Board, [H|T], Player, MouvList) :- ajout_Mouv_possible(Board, H, Player, MouvList), recup_Mouv_possible(Board, T, Player, MouvList).


%Si la case testee est vide alors cela peut etre un coup possible
ajout_Mouv_possible(Board, Mouv, Player, MouvList) :- nth1(Mouv, Board, MouvCase), nonvar(MouvCase), test_Mouv_possible(Board, Mouv, Player, MouvList).

%Si la case testee n'est pas vide, alors elle n'est pas exploitable pour un coup
ajout_Mouv_possible(Board, Mouv, Player, MouvList).


%on teste les parcours dans les 8 directions
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, -9).
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, -8).
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, -7).
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, -1).
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, 1).
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, 7).
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, 8).
test_Mouv_possible(Board, Mouv, Player, MouvList) :- test_premier_voisin(Board, Mouv, Player, MouvList, 9).


%On regarde le premier voisin de la case du mouvement avant de lancer la recursion.
test_premier_voisin(Board, Mouv, Player, MouvList, Dir) :- nonSortis(Mouv, Dir), nouvPos is Mouv + Dir, 
nth1(nouvPos, Board, nouvCase), var(nouvCase), nouvCase\=Player, test_voisin_suivant(Board, nouvPos, Player, MouvList, Dir).


%Sortie de la recursion positive
test_voisin_suivant(Board, Mouv, Player, MouvList, Dir) :- nonSortis(Mouv, Dir), nouvPos is Mouv + Dir, 
nth1(nouvPos, Board, nouvCase), var(nouvCase), nouvCase==Player.

%Sortie de la recursion negative
test_voisin_suivant(Board, Mouv, Player, MouvList, Dir) :- (\nonSortis(Mouv, Dir)); ( nouvPos is Mouv + Dir, 
nth1(nouvPos, Board, nouvCase), nonvar(nouvCase) ).

%On continue la recursion
test_voisin_suivant(Board, Mouv, Player, MouvList, Dir) :- nonSortis(Mouv, Dir), nouvPos is Mouv + Dir, 
nth1(nouvPos, Board, nouvCase), var(nouvCase), nouvCase\=Player, test_voisin_suivant(Board, nouvPos, Player, MouvList, Dir).


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

