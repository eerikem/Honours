%% @author Admin
%% @doc @todo Add description to connectFour.


-module(connectFour).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,play/0,stop/1,drop_token/2]).

play()->
	{ok,Pid} = start_link(),
	{ok,B} = ai_move(Pid),
	%%{ok, B} = getBoard(Pid),
	board:printConnect(B),
	play_turn(Pid).

play_turn(Pid)->
	{R,_} = string:to_integer(io:get_line("Your Turn: ")),
	{ok, Board} = drop_token(Pid,R),
	board:printConnect(Board),
	case permute:endState(Board) of
		true -> io:format("AI loss~n"),
				stop(Pid);
		false-> ai_turn(Pid)
		end.


ai_turn(Pid)->
	io:format("AI Turn~n"),
	{ok, Board} = ai_move(Pid),
	board:printConnect(Board),
	case permute:endState(Board) of
		true-> io:format("AI Win~n"),
				stop(Pid);
		false->play_turn(Pid)
		end.

getBoard(Pid)->
	gen_server:call(Pid, board).

drop_token(Pid, Pos)->
	gen_server:call(Pid, {move,Pos}).

ai_move(Pid)->
	gen_server:call(Pid, ai).

stop(Pid)->
	gen_server:cast(Pid,stop).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
%%-record(state, {}).




%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, board:connect()}.

start_link() -> gen_server:start_link(?MODULE, [], []).

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({move,Pos}, _, State) ->
	B = ai:applyMove(Pos, State),
    Reply = ok,
    {reply, {Reply,B}, B};

handle_call(board,_,State)->
	{reply, {ok, State},State};

handle_call(ai,_,Board)->
	{_,B} = ai:minimax(Board),
	{reply, {ok, B},B};

handle_call(R,_,B)->
	{reply,{badarg,R},B}.

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_cast(stop, State)->
	{stop, normal, State};

handle_cast(Msg, State) ->
    {noreply, State}.

%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


