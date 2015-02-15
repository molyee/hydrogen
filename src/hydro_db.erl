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
-module(hydro_db).
-author("Alex Sarapulov").

-include("arena.hrl").
-include("player.hrl").

%% API
-export([init/0]).
-export([create_player/1]).
-export([create_arena/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok | {error, term()}.
init() ->
    ok.

-spec create_player(binary()) -> {player_id(), player()} | {error, term()}.
create_player(Name) ->
    Player = gen_player(Name),
    {Player#player.id, Player}.

-spec create_arena(term()) -> {arena_id(), arena()} | {error, term()}.
create_arena({Name, MaxPlayers}) ->
    Arena = gen_arena(Name),
    {Arena#arena.id, Arena#arena{max_players = MaxPlayers}};

create_arena(Name) ->
    Arena = gen_arena(Name),
    {Arena#arena.id, Arena}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% generate player
-spec gen_player(binary()) -> player().
gen_player(Name) -> #player{
        id = gen_unique_player_id(Name),
        name = Name,
        location = gen_start_location()
    }.

-spec gen_unique_player_id(binary()) -> player_id().
gen_unique_player_id(Name) ->
    Name.

-spec gen_start_location() -> location().
gen_start_location() ->
    #location{x = 0, y = 0}.

% generate arena
-spec gen_arena(binary()) -> arena().
gen_arena(Name) -> #arena{
        id = gen_unique_arena_id(Name),
        name = Name
    }.

-spec gen_unique_arena_id(binary()) -> arena_id().
gen_unique_arena_id(Name) ->
    Name.
