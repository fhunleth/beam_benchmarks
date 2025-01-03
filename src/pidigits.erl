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
%
%   contributed by Mark Scandariato
%
%   erl -noshell -noinput -run pidigits main 7


-module(pidigits).
-export([main/2]).

% conversion
is_safe(Z, N) -> N == extr(Z, 4).
next(Z)       -> extr(Z, 3).
prod(Z, N)    -> comp({10, -10*N, 0, 1}, Z).
cons(Z, Zp)   -> comp(Z, Zp).

% LFT
-define(unit, {1,0,0,1}).
comp({Q,R,S,T}, {Qp, Rp, Sp, Tp}) ->
    {Q*Qp + R*Sp, Q*Rp + R*Tp, S*Qp + T*Sp, S*Rp + T*Tp}.
extr({Q,R,S,T}, X) -> (Q * X + R) div (S * X + T).

lft(K) -> {K, 4*K+2, 0, 2*K+1}.

stream(Fd, N) -> stream(Fd, N, 0, 1, ?unit, []).
stream(Fd, N, N, _, _, P) -> print(Fd,N,P);
stream(Fd, N, C, K, Z, P) ->
    Y = next(Z),
    case is_safe(Z, Y) of
        true  ->
            stream(Fd, N, C+1, K, prod(Z,Y), update(Fd,C,Y,P));
        false ->
            stream(Fd, N, C, K+1, cons(Z, lft(K)), P)
    end.


update(Fd, C, D, P) when C rem 10 == 0, C > 0 ->
    print(Fd, C, P),
    [D];

update(_Fd, _, D, P) -> [D|P].


print(Fd, C, P) -> do_print(Fd, C, lists:reverse(P)).


do_print(Fd, C, []) when C rem 10 == 0 -> io:fwrite(Fd, "\t:~p~n", [C]);
do_print(Fd, C, []) -> io:fwrite(Fd, "~*.1c:~p~n", [10 - C rem 10, $\t, C]);
do_print(Fd, C, [H|T]) -> io:fwrite(Fd, "~p", [H]), do_print(Fd, C, T).

main(Fd, N) when N > 1 -> stream(Fd, N).
