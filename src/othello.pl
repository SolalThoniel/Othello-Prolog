% The game state will be represented by a list of 64 elements
% board(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) at the beginning
% eg board(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,'x','o',_,_,_,_,_,_,_,_,_,_,_,'o','x',_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) after the first round 
% ...
% until someone wins or the board is fully instanciated

:- ensure_loaded('choose_move.pl').
:- dynamic board/1.


%%%% Test is the game is finished %%%
%gameover(Winner) :- board(Board), winner(Board,Winner), !. % There exists a winning configuration: We cut!
gameover('Draw') :- board(Board), isBoardFull(Board). % the Board is fully instanciated (no free variable): Draw.
%%%% Test if a Board is a winning configuration for the player P.
%winner(Board, P) :- false % No moves are possible for both players

%%%% Recursive predicate that checks if all the elements of the List (a board) 

isBoardFull([]).
isBoardFull([H|T]):- nonvar(H), isBoardFull(T).


% The game is over, we use a cut to stop the proof search, and display the winner-board.
play(_,_):- gameover(Winner), !, write('Game is Over. Winner: '), writeln(Winner), displayBoard.


%The game is not over, we play the next turn for a human
play(Player, TabPlayerType) :- getPlayerType(Player, TabPlayerType, PlayerType),
PlayerType == 1,
write('New turn for:'), writeln(Player),
board(Board), % instanciate the board from the knowledge base
displayBoard, % print it

%Demande un mouvement au joueur humain jusqu'a ce qu il soit possible
demandeMouv(Board, Player, Move),

directions_Mouvement(Board, Player, Move, MouvementDirections),
write(MouvementDirections),
playMove(Board,MouvementDirections,NewBoard,Player), % Play the move and get the result in a new Board
applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
changePlayer(Player, NextPlayer), % Change the player before next turn
play(NextPlayer, TabPlayerType). % next turn!


%The game is not over, we play the next turn for an IA
play(Player, TabPlayerType) :- getPlayerType(Player, TabPlayerType, PlayerType),
PlayerType == 2,
write('New turn for:'), writeln(Player),
board(Board), % instanciate the board from the knowledge base
displayBoard, % print it
choix_Mouvement(Board, Player, MouvementDirections),
write(MouvementDirections),
faire_mouvement(Board,MouvementDirections,Player),
changePlayer(Player,NextPlayer), % Change the player before next turn
play(NextPlayer,TabPlayerType). % next turn!

%Demande un mouvement au joueur humain
demandeMouv(Board, Player, Move) :- menuJouerLigne,
read(Ligne),
menuJouerColonne,
read(Colonne),
convertTab(Colonne, Ligne, Move),
trouver_Mouvements(Board, Player, MouvList),
member(Move, MouvList).

demandeMouv(Board, Player, Move) :- writeln("Desole votre mouvement n'est pas possible, veuillez en choisir un autre"),
demandeMouv(Board, Player, Move).

%Applique le mouvement si il existe
faire_mouvement(Board,[],Player) :- writeln("Desole vous ne pouvez pas jouer, vous passez votre tour.").

faire_mouvement(Board,MouvementDirections,Player) :- playMove(Board,MouvementDirections,NewBoard,Player), % Play the move and get the result in a new Board
applyIt(Board, NewBoard). % Remove the old board from the KB and store the new one.


%Play a Move, the new Board will be the same, but one value will be instanciated with the Move
playMove(Board, [Emplacement|Directions],NewBoard,Player) :- placePiece(Board,  Emplacement, BoardTemp, Player), 
		retournerPieces(BoardTemp, Emplacement, Directions, NewBoard, Player).
placePiece(Board, Emplacement, NewBoard, Player) :- Board = NewBoard, nth1(Emplacement,NewBoard,Player), !.
placePiece(Board, Emplacement, BoardFinal, Player) :- Board = NewBoard, not(nth1(Emplacement,NewBoard,Player)), 
		replace(NewBoard, Emplacement, Player, BoardFinal) .

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

retournerPieces(Board, Emplacement, [], NewBoard,Player) :- Board=NewBoard, !.
retournerPieces(Board, Emplacement, [Direction|Reste], NewBoard, Player) :- 
	   retournerPiecesDirection(Board, Emplacement, Direction, BoardTemp, Player),
	   retournerPieces(BoardTemp, Emplacement, Reste, NewBoard, Player).

retournerPiecesDirection(Board, Emplacement, Direction, Board, Player):- Nemplacement is Emplacement+Direction, 
	   nth1(Nemplacement, Board, Valeur), Valeur == Player, !.
retournerPiecesDirection(Board, Emplacement, Direction, NewBoard, Player):- Nemplacement is Emplacement+Direction,
	   nth1(Nemplacement, Board, Valeur), Valeur \== Player, placePiece(Board, Nemplacement, BoardTemp, Player),  
	   retournerPiecesDirection(BoardTemp, Nemplacement, Direction, NewBoard, Player).


oppose(Player, Oppose) :- Player=='x', Oppose=='o'.
oppose(Player, Oppose) :- Player=='o', Oppose=='x'.

%Remove old board-save new on in the knowledge base
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).

%renvoie le type de joueur, 1 = humain, 2 = IA
getPlayerType(Player, TabPlayerType, PlayerType) :- Player == 'x', nth1(2, TabPlayerType, PlayerType).
getPlayerType(Player, TabPlayerType, PlayerType) :- Player == 'o', nth1(4, TabPlayerType, PlayerType).

%%%% Predicate to get the next player
changePlayer('x','o').
changePlayer('o','x').

convertTab(Colonne, Ligne, Result) :- Result is ((Ligne-1)*8 + Colonne).

%%%% Print the value of the board at index N:
% if its a variable, print ? and x or o otherwise.
printVal(N) :- board(B), nth1(N,B,Val), var(Val), write(' -'), !.
printVal(N) :- board(B), nth1(N,B,Val), write(' '),write(Val).

%%%% Display the board
displayBoard:-
 writeln('  A B C D E F G H '),
 write('1'), printVal(1), printVal(2), printVal(3), printVal(4), printVal(5), printVal(6), printVal(7), printVal(8), writeln(' 1'),
 write('2'), printVal(9), printVal(10), printVal(11), printVal(12), printVal(13), printVal(14), printVal(15), printVal(16),writeln(' 2'),
 write('3'), printVal(17), printVal(18), printVal(19), printVal(20), printVal(21), printVal(22), printVal(23), printVal(24), writeln(' 3'),
 write('4'), printVal(25), printVal(26), printVal(27), printVal(28), printVal(29), printVal(30), printVal(31), printVal(32), writeln(' 4'),
 write('5'), printVal(33), printVal(34), printVal(35), printVal(36), printVal(37), printVal(38), printVal(39), printVal(40), writeln(' 5'),
 write('6'), printVal(41), printVal(42), printVal(43), printVal(44), printVal(45), printVal(46), printVal(47), printVal(48), writeln(' 6'),
 write('7'), printVal(49), printVal(50), printVal(51), printVal(52), printVal(53), printVal(54), printVal(55), printVal(56), writeln(' 7'),
 write('8'), printVal(57), printVal(58), printVal(59), printVal(60), printVal(61), printVal(62), printVal(63), printVal(64), writeln(' 8'),
 writeln('  A B C D E F G H ').

%%%%% Start the game!

initBoard(C, D) :- length(Board,64) , assert(board(Board)), placePiece(Board,28,NewBoard,'o'), placePiece(Board,37,NewBoard,'o'), placePiece(Board,29,NewBoard,'x'), placePiece(Board,36, NewBoard,'x'), applyIt(Board,NewBoard), play('x', ['x', C, 'o', D]).


%%%%% Menu
menuPlayer :- writeln("1- Humain"), writeln("2- IA").
menuJouerColonne :- writeln("Selectionner la colonne que vous souhaitez jouer").
menuJouerLigne :- writeln("Selectionner la ligne que vous souhaitez jouer").

%%%%% Start the menu before playing
start :- writeln("Bienvenue sur le jeu du Ohtello."), writeln("Selectionner le premier joueur : ") , menuPlayer, read(C), writeln("Selectionner le deuxieme joueur : "), menuPlayer, read(D), initBoard(C, D).
