-module(pixel_party_serv).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_client/1,
         remove_client/1,
         canvas_cells/0,
         update_canvas/1,
         broadcast_changes/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(DEFAULT_COLOUR, <<"FFFFFF">>).

-record(state, {clients, canvas}).
-type canvas() :: [[binary()]].
-type cell() :: {integer(), integer(), binary()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Return the entire canvas as a nested list of cell colours 
%%--------------------------------------------------------------------
-spec canvas_cells() -> canvas() | {error, atom()}.
canvas_cells() ->
    gen_server:call(?MODULE, canvas_cells).

%%--------------------------------------------------------------------
%% @doc update the cnvas with the new cells
%%--------------------------------------------------------------------
-spec update_canvas([cell()]) -> ok.
update_canvas(Cells) ->
    gen_server:cast(?MODULE, {update_canvas, Cells}).

%%--------------------------------------------------------------------
%% @doc Send the changes to all other clients
%%--------------------------------------------------------------------
-spec broadcast_changes([cell()]) -> ok.
broadcast_changes(Cells) ->
    gen_server:cast(?MODULE, {broadcast_changes, Cells}).

%%--------------------------------------------------------------------
%% @doc add a new client to the list
%%--------------------------------------------------------------------
-spec add_client(pid()) -> ok.
add_client(Pid) ->
    gen_server:cast(?MODULE, {add_client, Pid}).

%%--------------------------------------------------------------------
%% @doc remove a client to the list
%%--------------------------------------------------------------------
-spec remove_client(pid()) -> ok.
remove_client(Pid) ->
    gen_server:cast(?MODULE, {remove_client, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Canvas = create_canvas(),
    {ok, #state{clients=[], canvas=Canvas}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(canvas_cells, _From, State) ->
    CanvasList = canvas_to_list(State#state.canvas, 0, [], []),
    {reply, CanvasList, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({update_canvas, Cells}, State) ->
    update_canvas(State#state.canvas, Cells),
    {noreply, State};

handle_cast({broadcast_changes, Cells}, State) ->
    Clients = State#state.clients,
    %% error_logger:info_msg("Broadcasting ~w to ~w~n",[Cells, Clients]),
    lists:foreach(fun(Pid) -> Pid ! {changes, self(), Cells} end, Clients),
    {noreply, State};

handle_cast({add_client, Pid}, #state{clients=Clients} = State) ->
    {noreply, State#state{clients=[Pid|Clients]}};

handle_cast({remove_client, Pid}, #state{clients=Clients} = State) ->
    NewClients = lists:delete(Pid, Clients),
    {noreply, State#state{clients=NewClients}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_canvas() ->
   Canvas = ets:new(canvas,[]),
   create_canvas(Canvas, 0).
 

create_canvas(Canvas, 3072) ->
    %% <<"21BFAA">> 48 x 64 matrix
    Canvas;
create_canvas(Canvas, Idx) ->
    true = ets:insert(Canvas, {Idx, ?DEFAULT_COLOUR}),
    create_canvas(Canvas, Idx+1).



canvas_to_list(_, 3072, Row, Mat) ->
    lists:reverse([lists:reverse(Row)|Mat]);
canvas_to_list(Canvas, Idx, Row, Mat) ->
    [{Idx, Color}] = ets:lookup(Canvas, Idx),
    Idx2 = Idx+1,
    case Idx rem 64 of
        0 when Idx > 0 -> canvas_to_list(Canvas, Idx2, [Color], [lists:reverse(Row)|Mat]);
        _ -> canvas_to_list(Canvas, Idx2, [Color|Row], Mat)
    end.


update_canvas(Canvas, Coords) ->
    %% [{"x":"18","y":"1","c":"ECE644"}]
    lists:foreach(
      fun([{<<"x">>, X},{<<"y">>, Y},{<<"c">>, C}]) ->
              Idx = X + 64*Y,
              true = ets:insert(Canvas, {Idx, C})
      end,
      Coords).




