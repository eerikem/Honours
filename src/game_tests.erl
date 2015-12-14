%% @author Admin
%% @doc @todo Add description to game_tests.


-module(game_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F),{setup,fun start/0,fun stop/1,F}).
%% ====================================================================
%% API functions
%% ====================================================================

start_stop_test_()->
	{"Start a game then stop the game.",
	 ?setup(fun game_exists/1)}.

player_move_test_()->
	{"A player can place a token on the board",
	 ?setup(fun place_token/1)}.

place_token(Pid)->
	L = connectFour:drop_token(Pid,2),
	[?_assertMatch({ok,[[o,o,o,o,o,z]|_]},L)].
  

%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	{ok,Pid} = connectFour:start_link(),
	Pid.

stop(Pid) -> connectFour:stop(Pid).

game_exists(Pid) ->
	[?_assert(erlang:is_process_alive(Pid))].
	
