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
-module(hydro_player).
-author("Alex Sarapulov").

-behaviour(gen_server).

-include("player.hrl").

%% API
-export([start_link/2]).
-export([status/1]).
-export([join_arena/2, leave_arena/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    data    :: player(),
    arena   :: pid() | undefined
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec status(pid()) -> state() | {error, term()}.
status(Player) ->
    gen_server:call(Player, status).

-spec join_arena(pid(), pid()) -> ok | {error, term()}.
join_arena(Player, Arena) ->
    gen_server:call(Player, {join_arena, Arena}).

-spec leave_arena(pid(), pid()) -> ok | {error, term()}.
leave_arena(Player, Arena) ->
    gen_server:call(Player, {leave_arena, Arena}).

-spec start_link(binary(), player()) -> {ok, pid()} | {error, term()}.
start_link(Id, Data) ->
    gen_server:start_link({local, Id}, ?MODULE, [Data], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([player()]) -> {ok, state()}.
init([Data]) ->
    {ok, #state{data = Data}}.

-spec handle_call(any(), pid(), state()) -> {reply, {error, term()} | term() | state(), state()}.
% status
handle_call(status, _From, State) ->
    {reply, State, State};

% arena
handle_call({join_arena, Arena}, _From, State) ->
    {Res, NewState} = case State#state.arena of
        undefined -> apply_joining_arena(Arena, State);
        {error, Reason} -> {{error, Reason}, State};
        _Pid -> {{error, <<"movement_between_arenas">>}, State}
    end,
    {reply, Res, NewState};

handle_call({leave_arena, Arena}, _From, State) ->
    {Res, NewState} = apply_leaving_arena(Arena, State),
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

-spec apply_joining_arena(pid(), state()) -> {ok | {error, term()}, state()}.
apply_joining_arena(Arena, State) ->
    case hydro_arena:join(self(), Arena) of
        ok -> {ok, State#state{arena = Arena}};
        {error, Reason} -> {{error, Reason}, State}
    end.

-spec apply_leaving_arena(pid(), state()) -> {ok | {error, term()}, state()}.
apply_leaving_arena(Arena, State) ->
    case hydro_arena:leave(self(), Arena) of
        ok -> {ok, State#state{arena = undefined}};
        {error, Reason} -> {{error, Reason}, State}
    end.

