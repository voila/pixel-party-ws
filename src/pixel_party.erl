%% Feel free to use, reuse and abuse the code in this file.

-module(pixel_party).

%% API.
-export([start/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(pixel_party).
