

choix_mouvement(Board, Player, Mouvement) :-
	trouver_mouvements(Board, Player, MouvList),
	evaluer_mouvements(Board, Player, MouvList, Mouvement).


trouver_mouvements(Board, Player, MouvList) :- .
	
	
recup_mouv_possible(Board, [], Player, MouvList).
recup_mouv_possible(Board, [H|T], Player, MouvList) :- ajout_mouv_possible(Board, [H|T], Player, MouvList), recup_mouv_possible(Board, T, Player, MouvList).


ajout_mouv_possible([H|T], Player, MouvList) :- H==vide, test_mouv_possible([H|T], Player, MouvList).
ajout_mouv_possible([H|T], Player, MouvList) :- H\==vide.

test_mouv_possible([H|T], Player, MouvList) :- .
test_mouv_possible([H|T], Player, MouvList) :- .



evaluer_mouvements(Board, Player, [H|T], H).

