%% From https://github.com/madnight/benchmarksgame/blob/9f5a20ba09e668a20bb0f9765b33071fa9bb3f1d/LICENSE
%%
%% Copyright © 2004-2008 Brent Fulgham, 2005-2017 Isaac Gouy
%%
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%    Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%
%%    Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution.
%%
%%    Neither the name of "The Computer Language Benchmarks Game" nor the name
%%    of "The Computer Language Shootout Benchmarks" nor the names of its
%%    contributors may be used to endorse or promote products derived from this
%%    software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%%% contributed by Christian von Roques
%%% modified by Jiri Isa

%% Each chameneos is its own process.
%% A chameneos sends {self(), Color} to the broker to request a
%% meeting with another chameneos.
%% The broker replies with {Pid, Color} of the partner met or 'stop'
%% whereupon the chameneos prints the Meetings and Selfmeetings it had
%% and replies with the number of Meetings for the broker to sum.

-module(chameneosredux).
-export([main/2]).

-import(lists, [foreach/2]).

spell(0) -> " zero";
spell(N) -> spell(N, []).

spell(0, L) -> L;
spell(N, L) -> spell(N div 10, [element(N rem 10 + 1, {" zero", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine"}) | L]).


complement(C, C) -> C;
complement(blue, red) -> yellow;
complement(blue, yellow) -> red;
complement(red, blue) -> yellow;
complement(red, yellow) -> blue;
complement(yellow, blue) -> red;
complement(yellow, red) -> blue.


show_complements(Fd) ->
    [ io:fwrite(Fd, "~p + ~p -> ~p~n", [A, B, complement(A, B)]) ||
        A <- [blue, red, yellow],
        B <- [blue, red, yellow]].


print_header(Fd, L) ->
    io:fwrite(Fd, "~n", []),
    foreach(fun(C) -> io:fwrite(Fd, " ~p", [C]) end, L),
    io:fwrite(Fd, "~n", []).


run(Fd, L, N) ->
    print_header(Fd, L),
    Broker = self(),
    foreach(fun(Color) -> spawn(fun() -> chameneos(Fd, Broker, Color, 0, 0) end) end, L),
    broker(N),
    cleanup(Fd, length(L), 0).


chameneos(Fd, Broker, Color, Meetings, MetSelf) ->
    Broker ! { self(), Color },
    receive
        {OPid, OColor} ->
            chameneos(Fd, Broker, complement(Color, OColor), Meetings+1,
                      if OPid == self() -> MetSelf+1; true -> MetSelf end);
        stop ->
            io:fwrite(Fd, "~w~s\n", [Meetings, spell(MetSelf)]),
            Broker ! Meetings
    end.


broker(0) -> nil;
broker(N) ->
    receive
        C1 = {Pid1, _} -> nil
    end,
    receive
        C2 = {Pid2, _} ->
            Pid1 ! C2,
            Pid2 ! C1,
            broker(N-1)
    end.

cleanup(Fd, 0, M) -> io:fwrite(Fd, "~s~n", [spell(M)]);
cleanup(Fd, N, M) ->
    receive
        {Pid, _Color} ->
            Pid ! stop,
            cleanup(Fd, N, M);
        Meetings ->
            cleanup(Fd, N-1, M+Meetings)
    end.


main(Fd, N) ->
    show_complements(Fd),
    run(Fd, [blue, red, yellow], N),
    run(Fd, [blue, red, yellow, red, yellow, blue, red, yellow, red, blue], N),
    io:fwrite(Fd, "~n", []).

