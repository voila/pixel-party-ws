%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(pixel_party_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_handler, []},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, [
				{directory, {priv_dir, pixel_party, [<<"static">>]}},
				{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			]}
		]}
	]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    pixel_party_sup:start_link().

stop(_State) ->
	ok.
