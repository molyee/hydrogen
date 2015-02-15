%%%-------------------------------------------------------------------
%%% Copyright (c) 2015 Alex Sarapulov
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-------------------------------------------------------------------
-module(hydro_arena).
-author("Alex Sarapulov").

-behaviour(gen_server).

-include("arena.hrl").

%% API
-export([start_link/2]).
-export([status/1]).
-export([join/2, leave/2]).
-export([start_battle/1, finish_battle/1]).
-export([move/3]).
-export([shoot/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    started         :: boolean(),
    data            :: arena(),
    bounds          :: size(),
    resp            :: resp_list(),
    players = #{}   :: players()
}).

-record(hit, {
    shooter :: player(),
    wounded :: player(),
    damage :: integer(),
    crit :: boolean()
}).

-record(kill, {
    shooter :: player(),
    wounded :: player()
}).

-record(size, {
    width                   :: integer(),
    height                  :: integer()
}).

-type size() :: #size{}.
-type resp_list() :: [] | [location()] | [location()|resp_list()].
-type players() :: map().
-type player() :: pid().
-type state() :: #state{}.
-type hit() :: #hit{} | #kill{}.
-type direction() :: up | down | left | right.


%%%===================================================================
%%% API
%%%===================================================================

-spec status(pid()) -> state().
status(Arena) ->
    gen_server:call(Arena, status).

-spec join(pid(), player()) -> ok | {error, term()}.
join(Arena, Player) ->
    gen_server:call(Arena, {join, Player}).

-spec leave(pid(), player()) -> ok | {error, term()}.
leave(Arena, Player) ->
    gen_server:call(Arena, {leave, Player}).

-spec start_battle(pid()) -> ok | {error, term()}.
start_battle(Arena) ->
    gen_server:call(Arena, start).

-spec finish_battle(pid()) -> ok | {error, term()}.
finish_battle(Arena) ->
    gen_server:call(Arena, finish).

-spec shoot(pid(), player(), direction()) -> miss | hit().
shoot(Arena, Player, Direction) ->
    gen_server:call(Arena, {shoot, Player, Direction}).

-spec move(pid(), player(), direction()) -> {ok, location()}.
move(Arena, Player, Direction) ->
    gen_server:call(Arena, {move, Player, Direction}).

-spec start_link(binary(), arena()) -> {ok, pid()}.
start_link(Id, Data) ->
    gen_server:start_link({local, Id}, ?MODULE, [Data], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([arena()]) -> {ok, state()}.
init([Data]) ->
    Bounds = get_arena_bounds(Data#arena.size),
    {ok, #state{
        resp = gen_resp_locations(Bounds, Data#arena.max_players),
        bounds = Bounds,
        data = Data
    }}.

-spec handle_call(term(), pid(), state()) -> {reply, {error, term()} | term() | state(), state()}.
% get status
handle_call(status, _From, State) ->
    reply(State, State);

% start battle
handle_call(start, _From, State) ->
    reply(apply_starting_battle(State));

% finish battle
handle_call(finish, _From, State) ->
    reply(apply_finishing_battle(State));

% join player
handle_call({join, Player}, _From, State) ->
    reply(add_player(Player, State));

% leave player
handle_call({leave, Player}, _From, State) ->
    reply(remove_player(Player, State));

% moving
handle_call({move, Player, Direction}, _From, State) ->
    reply(move_player(Player, Direction, State));

% shooting
handle_call({shoot, Player, Direction}, _From, State) ->
    reply(shoot_player(Player, Direction, State));

% others
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec reply(R::any(), S::state()) -> {reply, R::any(), S::state()}.
reply(Res, State) ->
    {reply, Res, State}.

-spec reply({R::any(), S::state()}) -> {reply, R::any(), S::state()}.
reply({Res, State}) ->
    reply(Res, State).

-spec is_member(player(), players()) -> boolean().
is_member(Player, Players) ->
    maps:is_key(Player, Players).

-spec add_player(player(), state()) -> {ok | {error, term()}, state()}.
add_player(Player, State = #state{data = Arena, players = Players}) ->
    IsMember = is_member(Player, Players),
    if
        IsMember ->
            {{error, <<"player_already_exists">>}, State};
        length(Players) >= Arena#arena.max_players ->
            {{error, <<"max_players_limit">>}, State};
        true -> {ok, State#state{players = [Player|Players]}}
    end.

-spec remove_player(player(), state()) -> state().
remove_player(Player, State = #state{players = Players}) ->
    {ok, State#state{players = maps:remove(Player, Players)}}.

-spec apply_starting_battle(state()) -> {ok, state()} | {error, term()}.
apply_starting_battle(State = #state{started = Started, players = Players}) ->
    if
        Started -> {{error, <<"battle_already_started">>}, State};
        length(Players) < 2 -> {{error, <<"rivals_not_found">>}, State};
        true -> {ok, State#state{started = true}}
    end.

-spec apply_finishing_battle(state()) -> {list(), state()} | {error, term()}.
apply_finishing_battle(State = #state{started = Started, players = Players}) ->
    if
        not Started -> {{error, <<"battle_not_started">>}, State};
        true -> {Players, State#state{started = false}}
    end.

-spec move_player(player(), direction(), state()) -> {ok, location()} | {error, term()}.
move_player(Player, Direction, State = #state{bounds = Bounds, players = Players}) ->
    case get_location(Player, Players) of
        {error, Reason} -> {{error, Reason}, State};
        Location -> {{ok, next_location(Location, Bounds, Direction)},
            State#state{players = set_location(Player, Players)}}
    end.

-spec shoot_player(player(), direction(), state()) -> {ok, location()} | {error, term()}.
shoot_player(Player, Direction, State = #state{bounds = Bounds, players = Players}) ->
    case get_location(Player, Players) of
        {error, Reason} -> {{error, Reason}, State};
        Location -> {{ok, next_location(Location, Bounds, Direction)}, State}
    end.

-spec next_location(location(), size(), direction()) -> location().
next_location(Location, Bounds, Direction) ->
    case Direction of
        up -> Location#location{y = min(Bounds#size.height, Location#location.y + 1)};
        down -> Location#location{y = max(0, Location#location.y - 1)};
        left -> Location#location{x = max(0, Location#location.x - 1)};
        right -> Location#location{x = min(Bounds#size.width, Location#location.x + 1)}
    end.

%% common

get_location(Player, Players) ->
    case lists:member(Player, Players) of
        true -> #location{};
        false -> {error, <<"player_not_joined">>}
    end.

set_location(Player, Players) ->
    case lists:member(Player, Players) of
        true -> Players;
        false -> {error, <<"player_not_joined">>}
    end.

-spec get_arena_bounds(arena_size()) -> size().
get_arena_bounds(ArenaSize) ->
    case ArenaSize of
        small -> #location{x = 10, y = 10};
        medium -> #location{x = 20, y = 20};
        big -> #location{x = 50, y = 50}
    end.

-spec get_random_resp(size()) -> location().
get_random_resp(Bounds) ->
    X = random:uniform(Bounds#size.width),
    Y = random:uniform(Bounds#size.height),
    #location{x = X, y = Y}.

-spec gen_resp_locations(arena_size(), integer()) -> resp_list() | {error, term()}.
gen_resp_locations(Bounds, Num) ->
    if
        Bounds#size.width * Bounds#size.height < Num div 2 ->
            {error, <<"too_many_resps">>};
        true ->
            gen_resps(Bounds, Num, [])
    end.

-spec gen_resps(size(), integer(), resp_list()) -> resp_list().
gen_resps(Bounds, Num, RespL) when Num > 0 ->
    Resp = get_random_resp(Bounds),
    case lists:member(Resp, RespL) of
        true -> gen_resps(Bounds, Num, RespL);
        false -> gen_resps(Bounds, Num - 1, [Resp|RespL])
    end;

gen_resps(_Bounds, 0, RespL) -> RespL.
