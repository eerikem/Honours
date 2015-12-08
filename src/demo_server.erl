-module(demo_server).
-export([start/0]).

start() ->
	Port = open_port({spawn, demo_server}, [{packet, 2}]),
	Port ! {self(), {command, [1,2,3,4,5]}},
	Port ! {self(), {command, [10,1,2,3,4,5]}},
	Port ! {self(), {command, "echo"}},
	Port ! {self(), {command, "abc"}},
	read_replies(Port).

read_replies(Port) ->
	receive
	{Port, Any} ->
		io:format('erlang received from port:~w~n', [Any]),
		read_replies(Port)
	after 2000 ->
		Port ! {self(), close},
		receive
			{Port, closed} ->
			true
		end
	end.