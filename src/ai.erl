%% @author Admin
%% @doc @todo Add description to ai.


-module(ai).

%% ====================================================================
%% API functions
%% ====================================================================
-export([bfs/0, bfs/1, dfs/0, dfs/1, star/0, star/1, nodeSort/2]).
-export([applyMove/2,minimax/1,connectFour/0,connectFour/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================



trace(N,D)->traceSolution(N,D,[N]).
traceSolution(Node,D,L)->
	%%io:format("~s~w~n", ["trace here ",Node]),
	[Parent] = dict:fetch(Node, D),
	case Parent of
		root ->board:printBoards(L);
		_ -> traceSolution(Parent,D,[Parent|L])
	end.

%% ====================================================================
%% Heuristic Search functions
%% ====================================================================
%%
%% The flow of execution is as follows:
%% 
%% 1) Pass in the initial and goal states
%% 2) Initialize Open & Closed (list and hashmap)
%% 3) Starting from initial state
%%    -> Test for goal condition
%%    -> Expand the fringe
%% 4) Terminate when fringe is exhausted
%%
%% For the expansion of nodes:
%%
%% 1) Generate a list of all possible moves for the given state
%% 2) Check for history on each new generated state 
%% 3) Add unvisited states to Open
%%
%%


%% 1) Pass in the initial and goal states
star()-> star1({board:board(goal), board:board(complete)}).
star(X)->star1({board:board(goal),board:board(X)}).

%% 2) Initialize Open & Closed
star1({GOAL, S})->
	Node = {0, 0, S},
	Open = [Node],
	L = dict:append(S, root , dict:new()),
	star(GOAL, {Open, L, 0}).

%% 3) Starting from initial state
%%    -> Test for goal condition
%%    -> Expand the fringe
%% 4) Terminate when fringe is exhausted
star(_, {[],_,_})-> unsolvable;
star(GOAL, {[Node|T], Closed,Acc}) -> 
	case Node of
		{_,_,GOAL} -> trace(GOAL,Closed),
					  {ok,{exported,Acc}};
		_ -> star(GOAL, expStar(Node, T, Closed,Acc))
	end.

%% For the expansion of nodes:
%%
%% 1) Generate a list of all possible moves for the given state
expStar({_,_,State}=Node, Open, Closed, Acc) ->
	NewStates = permute:heuristic2(State),
	expStar(Node,Open,Closed,NewStates,Acc+1).

%% 2) Check for history on each new generated state 
%% 3) For A* merge the unvisited states to sorted list Open
expStar(_,Open,Closed,[],Acc)->{Open,Closed,Acc};
expStar({Depth,_,Parent}=Node, Open, Closed,[{Her,H}|T],Acc) ->
	%%io:format("Heuristic Value ~w~n", [Her]),
	case dict:is_key(H, Closed) of
		true-> Open2 = Open,
			   Closed2 = Closed;
		false->
			Open2 = lists:merge(fun nodeSort/2,[{Depth+1,Her,H}],Open),
			Closed2 = dict:append(H,Parent,Closed) 
	end,

	expStar(Node,Open2,Closed2,T,Acc).

%% Sort the Open Nodes based on heuristic
nodeSort({X,Y,_},{X2,Y2,_})->X+Y=<X2+Y2.

%% ====================================================================
%% Depth First Search functions
%% ====================================================================

dfs()-> dfs1({board:board(goal), board:board(complete)}).
dfs(X)->dfs1({board:board(goal),board:board(X)}).

dfs1({GOAL, S})->
	Node = {root, 0, S},
	Q = queue:in(Node,queue:new()),
	L = dict:append(S, root , dict:new()),
	dfs(GOAL, {Q, L, 0}).

dfs(_, {{[],[]},_,_})-> unsolvable;
dfs(GOAL, {Q, Dict,Acc}) -> 
	{{value, Node}, Q2} = queue:out(Q),
	case Node of
		{_,_,GOAL} -> trace(GOAL,Dict),
					  {ok,{exported,Acc}};
		{visited,_,_} -> dfs(GOAL,{Q2,Dict,Acc});
		_ -> dfs(GOAL, succesor(Node, Q2, Dict,Acc))
	end.

succesor({_,_,State}=Node, Q, D,Acc) ->
	NewStates = permute:permute(State),
	succesor(Node,Q,D,NewStates,Acc+1).

succesor(_,Q,D,[],Acc)->{Q,D,Acc};
succesor({_,Depth,Parent}=Node, Q, Dict,[H|T],Acc) ->
	case dict:is_key(H, Dict) of
		true->Status = visited,
			Dict2 = Dict;
		false->Status = new,
			Dict2 = dict:append(H,Parent,Dict)   
	end,
	%% Last in First out!
	Q2 = queue:in_r({Status,Depth+1,H},Q),
	succesor(Node,Q2,Dict2,T,Acc).

%% ====================================================================
%% Breadth First Search functions
%% ====================================================================


bfs()-> bfs1({board:board(goal), board:board(complete)}).
bfs(X)->bfs1({board:board(goal),board:board(X)}).

bfs1({GOAL, S})->
	Node = {root, 0, S},
	Q = queue:in(Node,queue:new()),
	L = dict:append(S, root , dict:new()),
	bfs(GOAL, {Q, L,0}).

bfs(_, {{[],[]},_,_})-> unsolvable;
bfs(GOAL, {Q, Dict,Acc}) -> 
	{{value, Node}, Q2} = queue:out(Q),
	case Node of
		{_,_,GOAL} -> trace(GOAL,Dict),
					  {ok,{exported,Acc}};
		{visited,_,_} -> bfs(GOAL,{Q2,Dict,Acc});
		_ -> bfs(GOAL, expand(Node, Q2, Dict,Acc))
	end.

expand({_,_,State}=Node, Q, D,Acc) ->
	NewStates = permute:permute(State),
	expand(Node,Q,D,NewStates,Acc+1).

expand(_,Q,D,[],Acc)->{Q,D,Acc};
expand({_,Depth,Parent}=Node, Q, Dict,[H|T],Acc) ->
	case dict:is_key(H, Dict) of
		true->Status = visited,
			Dict2 = Dict;
		false->Status = new,
			Dict2 = dict:append(H,Parent,Dict)   
	end,
	%% First in First out!
	Q2 = queue:in({Status,Depth+1,H},Q),
	expand(Node,Q2,Dict2,T,Acc).

%% ====================================================================
%% Connect Four
%% ====================================================================


connectFour()->connectFour(board:board(connect),2).
connectFour(Board,Acc)->{_,B} = minimax(Board),
			   board:printConnect(B),
			   case permute:endState(B) of
					true -> "AI Win";
				    false->
			   {R,_} = string:to_integer(io:get_line("Your Turn: ")),
			   NewBoard = applyMove(R,B),
			   board:printConnect(NewBoard),
			   case permute:endState(NewBoard) of
					true -> io:format("AI loss~n");
					false-> io:format("My #~w Turn~n",[Acc]),
			  				connectFour(NewBoard,Acc+1)
			   end
			end.

connectFour(random)->connectFourRandom(board:board(connect),2).
connectFourRandom(Board,Acc)->{_,B} = minimax(Board),
			   board:printConnect(B),
			   case permute:endState(B) of
					true -> "AI Win";
				    false->
			   
			   NewBoard = applyMove(validRandom(B),B),
			   board:printConnect(NewBoard),
			   case permute:endState(NewBoard) of
					true -> io:format("AI loss~n");
					false-> io:format("My #~w Turn~n",[Acc]),
			  				connectFourRandom(NewBoard,Acc+1)
			   end
			end.
	
validRandom(B)->
	validRandom(B,random:uniform(7)).
validRandom(B,I)->
	[H|_] = lists:nth(I, B),
	case H of
		o -> I;
		_-> validRandom(B)
	end.

apply2Move([],_)->invalid_move;
apply2Move([o|T],L)->lists:merge(T,[z|L]);
apply2Move([H|T],L)->apply2Move(T,[H|L]).

applyMove(M,State)->
	{L1,[N|T]}=lists:split(M-1, State),
    %%L1 = lists:sublist(M-1, State),
	%%N = lists:nth(M, State),
	%%T = lists:nthtail(M, State),
	Row = apply2Move(lists:reverse(N),[]),
	lists:append(L1, [Row|T]).

minimax(State)->
	%%Node = {0,State},
	%%Q = queue:in(Node,queue:new()),
	%%L = dict:append(State, root , dict:new()),
	%%minimax(Q,L,0).
	Operators = permute:permute3(State),
	L = lists:map(fun(A) -> {min(0,4,A,-1000,1000),A} end,Operators),
	%%lists:map(fun({N,B})->io:format("Value~w~n",[N]),printBoard(B) end,L),
	%%io:format("~n",[]),
	L2 = lists:keysort(1, L),
	lists:last(L2).

cutoff(L,L,_)->true;
cutoff(_,_,State) -> permute:endState(State).

max(Depth,Dlimit,State,Alpha,Beta)->
	%%printBoard(State),
	case cutoff(Depth,Dlimit,State) of
		 true -> V = permute:connectEval(State),
				 %%io:format("Max trace evaled ~w at depth ~w~n", [V+Depth,Depth]),
				 %%printBoard(State),
				 V+Depth;
		 false-> pruneAlpha(Alpha,Beta,Depth,Dlimit,permute:permute3(State))
	 end.

min(Depth,Dlimit,State,Alpha,Beta)->
	%%printBoard(State),
	case cutoff(Depth,Dlimit,State) of
		 true -> V = permute:connectEval(State),
				 %%io:format("Min trace evaled ~w at depth ~w~n", [V-Depth,Depth]),
				 %%printBoard(State),
				 V-Depth;
		 false-> pruneBeta(Beta, Alpha, Depth, Dlimit, permute:permute3(State))	
	 end.


pruneAlpha(Alpha, Beta, Depth, Dlimit, [S|T]) -> pruneAlpha2(Alpha, -1000,Beta, Depth, Dlimit, [S|T]). 
pruneAlpha2(_,Alpha,_,_,_,[])-> Alpha;
pruneAlpha2(Alpha, Alpha2, Beta, Depth, Dlimit, [S|T]) ->
	Alpha3 = lists:max([Alpha2,min(Depth+1,Dlimit,S,Alpha,Beta)]),
	case Alpha >= Beta of
		true -> Alpha3;
		false-> pruneAlpha2(Alpha, Alpha3, Beta, Depth, Dlimit, T)
	end.

pruneBeta(Beta, Alpha, Depth, Dlimit, [S|T]) -> pruneBeta2(Beta, 1000, Alpha, Depth, Dlimit, [S|T]).
pruneBeta2(_,Beta,_,_,_,[])-> Beta;
pruneBeta2(Beta, Beta2, Alpha, Depth, Dlimit, [S|T]) ->
	Beta3 = lists:min([Beta2,max(Depth+1,Dlimit,S,Alpha,Beta)]),
	case Beta3 =< Alpha of
		true -> Beta3;
		false-> pruneBeta2(Beta, Beta3, Alpha, Depth, Dlimit, T)
	end.