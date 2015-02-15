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
-module(hydro).
-author("Alex Sarapulov").

-include("arena.hrl").
-include("player.hrl").

%% API
-export([run/0]).
-export([create_player/1]).
-export([create_arena/1]).

%%-type identifier() :: string().

-spec run() -> {ok, [atom()]}.
run() ->
    application:load(hydro),
    application:start(hydro_app).

-spec create_player(binary()) -> {ok, pid()} | {error, Reason::term()}.
create_player(Name) ->
    Player = #player{
        name = Name
    },
    hydro_player_sup:start_player(Name, Player).

-spec create_arena(binary()) -> {ok, pid()} | {error, Reason::term()}.
create_arena(Name) ->
    Arena = #arena{
        name = Name
    },
    hydro_arena_sup:start_arena(Name, Arena).
