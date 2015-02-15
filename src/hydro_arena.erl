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
    data            :: #arena{},
    players = []    :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec status(pid()) -> #state{}.
status(Arena) ->
    gen_server:call(Arena, status).

-spec join(pid(), pid()) -> ok | {error, Reason::term()}.
join(Player, Arena) ->
    gen_server:call(Arena, {join, Player}).

-spec leave(pid(), pid()) -> ok | {error, Reason::term()}.
leave(Player, Arena) ->
    gen_server:call(Arena, {leave, Player}).

-spec start_link(binary(), #arena{}) -> {ok, pid()}.
start_link(Id, Data) ->
    gen_server:start_link({local, Id}, ?MODULE, [Data], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([#arena{}]) -> {ok, #state{}}.
init([Data]) ->
    {ok, #state{data = Data}}.

% status
handle_call(status, _From, State) ->
    {reply, State, State};

% join
handle_call({join, Player}, _From, State) ->
    case is_member(Player, State) of
        true -> {reply, {error, badarg}, State};
        false -> {reply, ok, add_player(Player, State)}
    end;

% leave
handle_call({leave, Player}, _From, State) ->
    case is_member(Player, State) of
        true -> {reply, ok, remove_player(Player, State)};
        false -> {reply, {error, badarg}, State}
    end;

% others
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec is_member(pid(), #state{}) -> boolean().
is_member(Player, #state{players = Players}) ->
    lists:member(Player, Players).

-spec add_player(pid(), #state{}) -> #state{}.
add_player(Player, State = #state{players = Players}) ->
    State#state{players = [Player|Players]}.

-spec remove_player(pid(), #state{}) -> #state{}.
remove_player(Player, State = #state{players = Players}) ->
    State#state{players = lists:delete(Player, Players)}.