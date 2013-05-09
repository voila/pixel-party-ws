-module(world).
-compile(export_all).

%-export([init/3]).
%-export([handle/2]).
%-export([terminate/3]).

init(_Transport, Req, []) ->
 World = create_world(),
 {ok, Req, World}.

handle(Req, State) ->
  WorldBin = world_to_json(State),
  % error_logger:info_msg("~p~n", [WorldBin]),
  {ok, Req2} = cowboy_req:reply(200, [], WorldBin, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
ok.


%% Helpers

world_to_json(World) ->
    WordList = world_to_list(World, 0, [], []),
    jsx:encode([{<<"pixels">>, WordList},
                {<<"version">>, 0}, %% should inc for each modified pixel
                {<<"control">>, [{}]}]).


create_world() ->
   World = ets:new(canvas,[]),
   create_world(World, 0).
 

create_world(World, 3072) ->
    %% <<"21BFAA">> 48 x 64 matrix
    World;
create_world(World, Idx) ->
    true = ets:insert(World, {Idx, <<"FFFF00">>}),
    create_world(World, Idx+1).



world_to_list(_, 3072, Row, Mat) ->
    lists:reverse([lists:reverse(Row)|Mat]);
world_to_list(World, Idx, Row, Mat) ->
    [{Idx, Color}] = ets:lookup(World, Idx),
    Idx2 = Idx+1,
    case {Idx, Idx rem 64} of
        {0, _} -> world_to_list(World, Idx2, [Color|Row], Mat);
        {_, 0} -> world_to_list(World, Idx2, [Color], [lists:reverse(Row)|Mat]);
        {_,_} -> world_to_list(World, Idx2, [Color|Row], Mat)
    end.


update_world(World, CoordJson) ->
    %% [{"x":"18","y":"1","c":"ECE644"}]
    Coords = jsx:decode(CoordJson),
    lists:foreach(fun([{<<"x">>, X},{<<"y">>, Y},{<<"c">>, C}]) ->
                          Idx = 64*X + Y,
                          true = ets:insert(World, {Idx, C})
                  end,
                  Coords).

