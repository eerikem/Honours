%% @author Admin
%% @doc @todo Add description to permute.


-module(permute).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, permute/1,heuristic/1,heuristic2/1]).
-export([endState/1,connectEval/1,permute3/1]).


-on_load(init/0).

init()-> erlang:load_nif("./permute",0).

permute(_)-> "Nif Library Not Loaded.".
heuristic(_)-> "Nif Library Not Loaded.".
heuristic2(_)-> "Nif Library Not Loaded.".

endState(_)->"Nif Library Not Loaded.".
connectEval(_)->"Nif Library Not Loaded.".
permute3(_)->"Nif Library Not Loaded.".

%% ====================================================================
%% Internal functions
%% ====================================================================


