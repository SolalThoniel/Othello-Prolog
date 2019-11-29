% The game state will be represented by a list of 9 elements
% board(_,_,_,_,_,_,_,_,_) at the beginning
% eg board(_,_,'x',_,_,_,_,_,_) after the first round
% eg board(_,_,'x',_,_,_,'o',_,_) after the second round
% ...
% until someone wins or the board is fully instanciated
:- dynamic board/1.
%%%% Test is the game is finished %%%
%gameover(Winner) :- board(Board), winner(Board,Winner), !. % There exists a winning configuration: We cut!
gameover('Draw') :- board(Board), isBoardFull(Board). % the Board is fully instanciated (no free variable): Draw.
%%%% Test if a Board is a winning configuration for the player P.
%winner(Board, P) :- false % No moves are possible for both players

%%%% Recursive predicate that checks if all the elements of the List (a board)
%%%% are instanciated: true e.g. for [x,x,o,o,x,o,x,x,o] false for [x,x,o,o,_G125,o,x,x,o]
isBoardFull([]).
isBoardFull([H|T]):- nonvar(H), isBoardFull(T).
%%%% Artificial intelligence: choose in a Board the index to play for Player (_)
%%%% This AI plays randomly and does not care who is playing: it chooses a free position
%%%% in the Board (an element which is an free variable).
ia(Board, Move, Player, Index) :- 
%%%% Recursive predicate for playing the game.

% The game is over, we use a cut to stop the proof search, and display the winner/board.
play(_):- gameover(Winner), !, write('Game is Over. Winner: '), writeln(Winner), displayBoard.

% The game is not over, we play the next turn
play(Player):- write('New turn for:'), writeln(Player),
 board(Board), % instanciate the board from the knowledge base
 displayBoard, % print it
 menuJouerLigne,
 read(Ligne), 
 menuJouerColonne, 
 read(Colonne), 
 convertTab(Colonne, Ligne, Move),
 ia(Board, Move, Player, Index), % ask the AI for a move, that is, an index for the Player
 playMove(Board,Move,NewBoard,Player), % Play the move and get the result in a new Board
 applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
 changePlayer(Player,NextPlayer), % Change the player before next turn
 play(NextPlayer). % next turn!

%%%% Play a Move, the new Board will be the same, but one value will be instanciated with the Move
playMove(Board,Move,NewBoard,Player) :- Board = NewBoard, nth0(Move,NewBoard,Player).

%%%% Remove old board/save new on in the knowledge base
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).

%%%% Predicate to get the next player
changePlayer('x','o').
changePlayer('o','x').

%%%% Conversion 
convertLetter(Letter, Num) :- Letter == 'A', Num = 1.
convertLetter(Letter, Num) :- Letter == 'B', Num = 2.
convertLetter(Letter, Num) :- Letter == 'C', Num = 3.
convertLetter(Letter, Num) :- Letter == 'D', Num = 4.
convertLetter(Letter, Num) :- Letter == 'E', Num = 5.
convertLetter(Letter, Num) :- Letter == 'F', Num = 6.
convertLetter(Letter, Num) :- Letter == 'G', Num = 7.
convertLetter(Letter, Num) :- Letter == 'H', Num = 8.
convertTab(Colonne, Ligne, Result) :- Result is ((Ligne-1)*8+Colonne) -1.

%%%% Print the value of the board at index N:
% if its a variable, print ? and x or o otherwise.
printVal(N) :- board(B), nth0(N,B,Val), var(Val), write(' -'), !.
printVal(N) :- board(B), nth0(N,B,Val), write(' '),write(Val).

%%%% Display the board
displayBoard:-
 writeln('  A B C D E F G H '), 
 write('1'), printVal(0), printVal(1), printVal(2), printVal(3), printVal(4), printVal(5), printVal(6), printVal(7), writeln(' 1'),
 write('2'), printVal(8), printVal(9), printVal(10), printVal(11), printVal(12), printVal(13), printVal(14), printVal(15), writeln(' 2'),
 write('3'), printVal(16), printVal(17), printVal(18), printVal(19), printVal(20), printVal(21), printVal(22), printVal(23), writeln(' 3'),
 write('4'), printVal(24), printVal(25), printVal(26), printVal(27), printVal(28), printVal(29), printVal(30), printVal(31), writeln(' 4'),
 write('5'), printVal(32), printVal(33), printVal(34), printVal(35), printVal(36), printVal(37), printVal(38), printVal(39), writeln(' 5'),
 write('6'), printVal(40), printVal(41), printVal(42), printVal(43), printVal(44), printVal(45), printVal(46), printVal(47), writeln(' 6'),
 write('7'), printVal(48), printVal(49), printVal(50), printVal(51), printVal(52), printVal(53), printVal(54), printVal(55), writeln(' 7'),
 write('8'), printVal(56), printVal(57), printVal(58), printVal(59), printVal(60), printVal(61), printVal(62), printVal(63), writeln(' 8'),
 writeln('  A B C D E F G H ').

%%%%% Start the game!
init(C, D) :- C == 1, length(Board,64) , assert(board(Board)), playMove(Board,27,NewBoard,'o'), playMove(Board,36,NewBoard,'o'), playMove(Board,28,NewBoard,'x'), playMove(Board,35, NewBoard,'x'),applyIt(Board,NewBoard), play('x').
init(C, D) :- C == 2, length(Board,64) , assert(board(Board)), playMove(Board,27,NewBoard,'o'), playMove(Board,36,NewBoard,'o'), playMove(Board,28,NewBoard,'x'), playMove(Board,35, NewBoard,'x'),applyIt(Board,NewBoard), play('x').

%%%%% Menu
menuPlayer :- writeln("1- Humain"), writeln("2- IA").
menuJouerColonne :- writeln("Selectionner la colonne que vous souhaitez jouer").
menuJouerLigne :- writeln("Selectionner la ligne que vous souhaitez jouer").

%%%%% Start the menu before playing
start :- writeln("Bienvenue sur le jeu du Ohtello."), writeln("Selectionner le premier joueur : ") , menuPlayer, read(C), writeln("Selectionner le deuxième joueur : "), menuPlayer, read(D), init(C, D).
