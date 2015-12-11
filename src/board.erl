%% @author Admin
%% @doc @todo Add description to board.


-module(board).

%% ====================================================================
%% API functions
%% ====================================================================
-export([connect/0,board/1,printBoard/1,printBoards/1,printConnect/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================



printConnectItem([H|T])->
	case H of
		z -> io:format("O");
		x -> io:format("X");
		_ -> io:format("-")
	end,
	T.

printConnect([])->io:format("~n");
printConnect(B)->printConnect(B,[]).
printConnect([[]|_],[])->io:format("~n");
printConnect([],B)->io:format("~n"),
					printConnect(lists:reverse(B));
printConnect([H|T],Acc)->%%io:format("~s~w ~w~n", ["trace here ",H, Acc]),
						 printConnect(T,[printConnectItem(H)|Acc]).


printBoard([]) -> ok;
printBoard([H|T]) -> 
	case length(H) of
		3 -> io:format("~s~w~n", ["    ",H]);
		_ -> io:format("~w~n", [H])
	end,
	printBoard(T).

printBoards([])->ok;
printBoards([H|T])->
	io:format("~n"),
	printBoard(H),
	printBoards(T).

connect()->[[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,o,o,o]].
board(connect)->connect();
board(full)->[[x,x,z,x,z,x],[x,x,z,x,z,x],[x,x,z,x,z,x],[x,x,z,x,z,x],[x,x,z,x,z,x],[x,x,z,x,z,x],[x,x,z,x,z,x]];
board(simple)->[[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,z,z,x],[o,o,o,o,o,x],[o,o,o,o,o,o]];
board(horizontal)->[[o,o,o,o,o,z],[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,x,z,z],[o,o,o,x,z,x],[o,o,o,o,x,z],[o,o,o,o,o,x]];
board(winner)->[[o,o,o,z,z,x],[o,o,o,o,o,x],[o,o,o,o,o,x],[o,o,o,o,o,x],[o,o,o,o,o,o],[o,o,o,o,o,o],[o,o,o,o,o,o]];
board(complete)->[[x,x,x],
	 [x,x,x],
 [x,x,x,x,x,x,x],
 [x,x,x,o,x,x,x],
 [x,x,x,x,x,x,x],
	 [x,x,x],
	 [x,x,x]];
board(goal)->[[o,o,o], [o,o,o], [o,o,o,o,o,o,o], [o,o,o,x,o,o,o], [o,o,o,o,o,o,o],[o,o,o],[o,o,o]];
board(test)->[[o,o,o], [o,o,o], [o,o,x,x,o,o,o], [o,x,x,x,x,o,o], [o,o,x,o,o,o,o],[o,x,o],[o,o,o]];
board(reduced)->[[o,o,o], [o,x,o], [x,o,x,x,x,o,o], [x,x,x,x,x,o,o], [o,x,o,o,x,x,o],[o,x,x],[x,x,x]];
board(1)->[[x,x,o], [x,x,x], [x,x,x,x,x,x,o], [x,x,x,x,x,x,x], [x,x,x,x,x,x,x],[x,x,x],[o,x,x]];
board(3)->[[o,o,o], [o,o,x], [o,o,o,o,x,o,o], [o,o,o,o,o,x,o], [o,o,o,o,o,o,o],[o,o,o],[o,o,o]];
board(nosolution)->[[o,x,x], [o,o,o], [o,o,o,o,o,o,o], [o,x,x,o,o,o,o], [o,o,o,o,o,o,o],[o,o,o],[o,o,o]];
board(basic)->[[o,o,o], [o,o,o], [o,o,o,o,o,o,o], [o,x,x,o,o,o,o], [o,o,o,o,o,o,o],[o,o,o],[o,o,o]].
