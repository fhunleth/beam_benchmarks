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
%   contributed by Fredrik Svahn

-module(spectralnorm2).
-export([main/1]).
-compile( [ inline, { inline_size, 1000 } ] ).

main(N) ->
    register(server, self()),
    {U, V} = power_method(N, 10, erlang:make_tuple(N, 1), []),
    eigen(N, U, V, 0, 0).

% eigenvalue of V
eigen(0, _, _, VBV, VV) when VV /= 0 -> math:sqrt(VBV / VV);

eigen(I, U, V, VBV, VV) when I /= 0 ->
    VI = element(I, V),
    eigen(I-1, U, V, VBV + element(I, U)*VI, VV + VI*VI).

% 2I steps of the power method
power_method(_, 0, A, B) -> {A, B};
power_method(N, I, A, _B) ->
    V = atav(N, A),
    U = atav(N, V),
    power_method(N, I-1, U, V).


% return element i,j of infinite matrix A
a(II,JJ) -> 1/((II+JJ-2)*(II-1+JJ)/2+II).


% multiply vector v by matrix A
av(N, V) -> pmap(N, fun(Begin, End) -> av(N, Begin, End, V) end).

av(N, Begin, End, V) -> server ! { self(), [ avloop(N, I, V, 0.0) || I <- lists:seq(Begin, End) ]}.

avloop(0, _, _, X) ->  X;
avloop(J, I, V, X) ->  avloop(J-1, I, V, X + a(I, J)*element(J, V) ).


% multiply vector v by matrix A transposed
atv(N, V) -> pmap(N, fun(Begin, End)-> atv(N, Begin, End, V) end).

atv(N, Begin, End, V) -> server ! { self(), [ atvloop(N, I, V, 0.0) || I <- lists:seq(Begin, End) ]}.

atvloop(0, _, _, X) -> X;
atvloop(J, I, V, X) -> atvloop(J-1, I, V, X + a(J, I)*element(J, V) ).


% multiply vector v by matrix A and then by matrix A transposed
atav(N, V) -> atv(N, av(N, V)).


%Helper function for multicore
pmap(N, F) ->
    Chunks = chunks(0, erlang:system_info(logical_processors), N, []),
    Pids = [spawn(fun()-> F(Begin, End) end) || {Begin, End} <- Chunks],
    Res = [ receive {Pid, X} -> X end || Pid <- Pids],
    list_to_tuple(lists:flatten(Res)).

chunks(I, P, N, A) when I == P-1 -> lists:reverse([{I*(N div P)+1, N} | A ]);
chunks(I, P, N, A) -> chunks(I+1, P, N, [{ I*(N div P)+1, (I+1)*(N div P)} | A ]).

