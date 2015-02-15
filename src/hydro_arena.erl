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

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    data            :: arena(),
    players = []    :: list()
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec status(pid()) -> state().
status(Arena) ->
    gen_server:call(Arena, status).

-spec join(pid(), pid()) -> ok | {error, term()}.
join(Player, Arena) ->
    gen_server:call(Arena, {join, Player}).

-spec leave(pid(), pid()) -> ok | {error, term()}.
leave(Player, Arena) ->
    gen_server:call(Arena, {leave, Player}).

-spec start_link(binary(), arena()) -> {ok, pid()}.
start_link(Id, Data) ->
    gen_server:start_link({local, Id}, ?MODULE, [Data], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([arena()]) -> {ok, state()}.
init([Data]) ->
    {ok, #state{data = Data}}.

-spec handle_call(term(), pid(), state()) -> {reply, {error, term()} | term() | state(), state()}.
% status
handle_call(status, _From, State) ->
    {reply, State, State};

% join
handle_call({join, Player}, _From, State) ->
    {Res, NewState} = add_player(Player, State),
    {reply, Res, NewState};

% leave
handle_call({leave, Player}, _From, State) ->
    {Res, NewState} = remove_player(Player, State),
    {reply, Res, NewState};

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

-spec add_player(pid(), state()) -> {ok | {error, term()}, state()}.
add_player(Player, State = #state{data = Arena, players = Players}) ->
    IsMember = lists:member(Player, Players),
    if
        IsMember ->
            {{error, <<"player_already_exists">>}, State};
        length(Players) >= Arena#arena.max_players ->
            {{error, <<"max_players_limit">>}, State};
        true -> {ok, State#state{players = [Player|Players]}}
    end.

-spec remove_player(pid(), state()) -> state().
remove_player(Player, State = #state{players = Players}) ->
    {ok, State#state{players = lists:delete(Player, Players)}}.