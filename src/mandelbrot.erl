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
%% Contributed by Fredrik Svahn based on Per Gustafsson's mandelbrot program

-module(mandelbrot).
-export([main/2]).
-define(LIM_SQR, 4.0).
-define(ITER, 50).
-define(SR, -1.5).
-define(SI, -1).

main(Io, N) ->
    NStr = integer_to_list(N),
    file:write(Io, ["P4\n", NStr, " ", NStr, "\n"]),

    %% Spawn one process per row
    Row = fun(Y)-> spawn(fun()-> row(Io, 0, ?SI+Y*2/N, N, 0, [], 7) end) end,
    Pids = lists:map(Row, lists:seq(0,N-1)),

    %Pass token around to make sure printouts are in the right order
    hd(Pids) ! tl(Pids) ++ [ self() ],
    receive _Token -> ok end.

%Iterate over a row, collect bits, bytes and finally print the row
row(Io, X, _, N, Bits, Bytes, BitC) when X =:= N-1 ->
    receive Pids ->
	    put_chars(Io, Bits, Bytes, BitC),
	    hd(Pids) ! tl(Pids)
    end;

row(Io, X, Y2, N, Bits, Bytes, 0) ->
    row(Io, X+1, Y2, N, 0, [Bits bsl 1 + m(?ITER, ?SR+X*2/N, Y2) | Bytes], 7);

row(Io, X, Y2, N, Bits, Bytes, BitC) ->
    row(Io, X+1, Y2, N, Bits bsl 1 + m(?ITER, ?SR+X*2/N, Y2), Bytes, BitC-1).

%Mandelbrot algorithm
m(Iter, CR,CI) -> m(Iter - 1, CR, CI, CR, CI).

m(Iter, R, I, CR, CI) ->
    case R*R+I*I > ?LIM_SQR of
	false when Iter > 0 -> m(Iter-1, R*R-I*I+CR, 2*R*I+CI, CR, CI);
	false -> 1;
	true -> 0
    end.

put_chars(Io, _, Bytes, 7)-> file:write(Io, lists:reverse(Bytes));
put_chars(Io, Bits, Bytes, C) -> file:write(Io, lists:reverse([Bits bsl (C+1) | Bytes])).