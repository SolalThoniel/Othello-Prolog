:- ensure_loaded('choose_move.pl').
:- ensure_loaded('othello.pl').
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
