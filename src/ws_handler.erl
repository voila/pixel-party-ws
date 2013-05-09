-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  pixel_party_serv:add_client(self()),
  CanvasList = pixel_party_serv:canvas_cells(),
  self() ! {world, self(), CanvasList},
	{ok, Req, undefined_state}.


websocket_handle({text, Msg}, Req, State) ->
  Cells = jsx:decode(Msg),
  pixel_party_serv:update_canvas(Cells),
  pixel_party_serv:broadcast_changes(Cells),
  Msg2 = jsx:encode([{<<"control">>, [{}]}, {<<"changes">>,[Cells]}]),
	{reply, {text, Msg2}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.


websocket_info({world, _Ref, CanvasList}, Req, State) ->
  Msg = jsx:encode([{<<"pixels">>, CanvasList},
                    {<<"version">>, 0}, %% should inc for each modified pixel
                    {<<"control">>, [{}]}]),
  {reply, {text, << Msg/binary >>}, Req, State};
websocket_info({changes, _Ref, Cells}, Req, State) ->
  Msg = jsx:encode([{<<"control">>, [{}]}, {<<"changes">>,Cells}]),  
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.


websocket_terminate(_Reason, _Req, _State) ->
  pixel_party_serv:remove_client(self()),
	ok.
