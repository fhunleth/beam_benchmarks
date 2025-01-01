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
%   contributed by Isaac Gouy (Erlang novice)

-module(spectralnorm).
-export([main/1]).

main(N) ->
    {U,V} = powerMethod(N,10, array(1.0,N,[]), array(0.0,N,[]) ),
    loop(N,U,V,0.0,0.0).

% eigenvalue of V
loop(0,_,_,VBV,VV) -> math:sqrt(VBV/VV);
loop(I,U,V,VBV,VV) ->
   VI = element(I,V),
   loop(I-1,U,V, VBV + element(I,U)*VI, VV + VI*VI).

% 2I steps of the power method
powerMethod(_,0,A,B) -> {A,B};
powerMethod(N,I,A,B) ->
   V = atav(N,A,B),
   U = atav(N,V,A),
   powerMethod(N,I-1,U,V).

% return element i,j of infinite matrix A
a(II,JJ) ->
   I = II-1.0, J = JJ-1.0,
   1.0/((I+J)*(I+J+1.0)/2.0 +I+1.0).

% multiply vector v by matrix A
av(_,0,_,AV) -> AV;
av(N,I,V,AV) ->
   av(N,I-1,V, setelement(I,AV, avloop(N,I,V,0.0) )).

avloop(0,_,_,X) -> X;
avloop(J,I,V,X) ->
   avloop(J-1,I,V, X + a(I,J)*element(J,V) ).

% multiply vector v by matrix A transposed
atv(_,0,_,ATV) -> ATV;
atv(N,I,V,ATV) ->
   atv(N,I-1,V, setelement(I,ATV, atvloop(N,I,V,0.0) )).

atvloop(0,_,_,X) -> X;
atvloop(J,I,V,X) -> atvloop(J-1,I,V, X + a(J,I)*element(J,V) ).

% multiply vector v by matrix A and then by matrix A transposed
atav(N,V,ATAV) ->
   atv(N,N, av(N,N,V,array(0.0,N,[])) ,ATAV).

% initialize a list and convert it to a tuple
array(_,0,L) -> list_to_tuple(L);
array(X,N,L) -> array(X,N-1,[X|L]).
