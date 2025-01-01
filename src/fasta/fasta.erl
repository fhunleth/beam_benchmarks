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
%   contributed by Alex Peake
%
%   erl -noshell -noinput -run fasta main N

-module(fasta).

-export([main/1]).

-define(IM, 139968).
-define(IA, 3877).
-define(IC, 29573).

-define(ALU,"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA").
-define(HS, [{$a, 0.3029549426680}, {$c, 0.1979883004921}, {$g, 0.1975473066391}, {$t, 0.3015094502008}]).
-define(IUB, [{$a, 0.27}, {$c, 0.12}, {$g, 0.12}, {$t, 0.27}, {$B, 0.02}, {$D, 0.02}, {$H, 0.02}, {$K, 0.02}, {$M, 0.02}, {$N, 0.02}, {$R, 0.02}, {$S, 0.02}, {$V, 0.02}, {$W, 0.02}, {$Y, 0.02}]).

-define(LINE_LENGTH, 60).

main(N) ->
   Seed = 42,
   write_fasta_cycle("ONE","Homo sapiens alu", ?ALU, N*2),
   NewSeed = write_fasta_rand("TWO","IUB ambiguity codes", ?IUB, N*3, Seed),
   write_fasta_rand("THREE","Homo sapiens frequency", ?HS, N*5, NewSeed),
   halt(0).

%% Write a sequence in LINE_LENGTH lines
write_fasta_cycle(Id, Description, Seq, Total) ->
	io:put_chars(">" ++ Id ++ " " ++ Description ++ "\n"),
	write_fasta_cycle(Seq, Total).
write_fasta_cycle(Seq, Total) when Total =< ?LINE_LENGTH ->
	{Seq1, _Remainder} = seq_len(Seq, Total),
	io:put_chars(Seq1 ++ "\n");
write_fasta_cycle(Seq, Total) ->
	{Seq1, Remainder} = seq_len(Seq, ?LINE_LENGTH),
	io:put_chars(Seq1 ++ "\n"),
	write_fasta_cycle(Remainder, Total - ?LINE_LENGTH).

%% Return a Len of a cycle of ALU
seq_len(Seq, Len) when length(Seq) >= Len ->
	lists:split(Len, Seq);
seq_len(Seq, Len) when length(?ALU) < Len - length(Seq) ->
	seq_len(Seq ++ ?ALU, Len);
seq_len(Seq, Len) ->
	{Seq1, Seq2} = lists:split(Len - length(Seq), ?ALU),
	{Seq ++ Seq1, Seq2}.

%% Write a random sequence in LINE_LENGTH lines
write_fasta_rand(Id, Description, Freq, Total, Seed) ->
	io:put_chars(">" ++ Id ++ " " ++ Description ++ "\n"),
	NewSeed = write_fasta_rand(Freq, Total, Seed),
	NewSeed.
write_fasta_rand(Freq, Total, Seed) when Total =< ?LINE_LENGTH ->
	{RandList, NewSeed} = rand_list(Freq, Total, [], Seed),
	io:put_chars(RandList),
	NewSeed;
write_fasta_rand(Freq, Total, Seed) ->
	{RandList, NewSeed} = rand_list(Freq, ?LINE_LENGTH, [], Seed),
	io:put_chars(RandList),
	write_fasta_rand(Freq, Total - ?LINE_LENGTH, NewSeed).

%% Return a Len of a random list of Freq
rand_list(_Freq, 0, List, Seed) ->
	{lists:reverse(["\n" | List]), Seed};
rand_list(Freq, Len, List, Seed) ->
	{Rand, NewSeed} = rand(Seed),
	rand_list(Freq, Len - 1, [choose_base(Freq, Rand) | List], NewSeed).

%% Functional random number generator
rand(Seed) ->
   NewSeed = (Seed * ?IA + ?IC) rem ?IM,
   {NewSeed / ?IM, NewSeed}.

%% Select the Base corresponding to the calculated cumulative Probability
choose_base([{Base,_}], _)
	-> Base;
choose_base([{Base,Freq} | _], Prob) when Prob < Freq ->
	Base;
choose_base([{_Base,Freq} | Bases], Prob) ->
	choose_base(Bases, Prob - Freq).
