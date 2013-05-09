%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(pixel_party_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [{pixel_party_serv,
            {pixel_party_serv, start_link, []}, % M,F,A 
            permanent, % always restarted
            1000,      % 1000 millisec to shutdown orderly
            worker, [pixel_party_serv]}],
	{ok, {{one_for_one, 10, 10}, Procs}}.
