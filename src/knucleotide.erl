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
%% contributed by Fredrik Svahn based on an earlier submission
%%             by Kenneth Johansson, Vlad Dumitrescu and Ulf Wiger

-module(knucleotide).
-export([main/1]).

to_upper(<<C, Cs/binary>>, Acc) when C >= $a, C =< $z ->
    to_upper(Cs, [C-($a-$A)| Acc]);
to_upper(<<$\n, Cs/binary>>, Acc) -> to_upper(Cs, Acc);
to_upper(<<C, Cs/binary>>, Acc) -> to_upper(Cs, [C | Acc]);
to_upper(<<>>, Acc) -> lists:reverse(Acc).

%% Read and discard until start of third segment
seek_three() ->
    case io:get_line('') of
	<<">TH", _/binary>> -> done;
	eof        -> erlang:error(eof);
	_          -> seek_three()
    end.

%% Read third segment
get_seq_three(Seq) ->
    case io:get_line('') of
	eof -> iolist_to_binary(lists:reverse(Seq));
	Str -> get_seq_three([to_upper(Str, [])|Seq])
    end.

%% Generate frequency hash table
gen_freq_table(FreqT, Seq, Len) ->
    gen_freq_table(Seq, Len, FreqT, size(Seq)-Len).

gen_freq_table(_, _, _, -1) -> done;
gen_freq_table(Seq, Len, FreqT, Dec) ->
    <<_:Dec/binary, Key:Len/binary, _/binary>> = Seq,
    update_counter(Key, FreqT),
    gen_freq_table(Seq, Len, FreqT, Dec-1).

%% Update hash table counter for already existing pattern or insert new
update_counter(Key, FreqT) ->
    try ets:update_counter(FreqT, Key, 1) of _ -> ok
    catch error:badarg -> ets:insert(FreqT, {Key, 1})
    end.

%% Print the frequency table in the right order
print_freq_table(FreqT) ->
    FreqList = lists:reverse(lists:keysort(2, ets:tab2list(FreqT))),
    Tot = lists:foldr(fun({_, Cnt}, Acc)-> Acc + Cnt end, 0, FreqList),
    lists:foreach(fun({Nucleoid, Cnt})->
			  io:fwrite("~s ~.3f\n",[Nucleoid, Cnt*100/Tot])
		  end, FreqList),
    io:fwrite("\n").

%% Print number of occurrences for a specific pattern
print_count(FreqT, Pattern) ->
    case ets:lookup(FreqT, Pattern) of
	[{_, Value}] -> io:fwrite("~w\t~s\n",[Value, Pattern]);
	[] -> io:fwrite("~w\t~s\n",[0, Pattern])
    end.

%% Spawn a worker process with its own hash table
do({PrintFun, Pattern}, Seq) ->
    spawn( fun()->
		   FreqT = ets:new(hash, [set]),
		   gen_freq_table(FreqT, Seq, size(Pattern)),
		   %Work is done, wait for token and print
		   receive Pids ->
			   PrintFun(FreqT, Pattern),
			   hd(Pids) ! tl(Pids)
		   end,
		   ets:delete(FreqT)
	   end ).

main(_Arg) ->
    io:setopts(standard_io, [binary]),
    seek_three(),
    Seq = get_seq_three([]),
    PrintFreq = fun(Res, _Pattern)-> print_freq_table(Res) end,
    PrintCount = fun(Res, Pattern)-> print_count(Res, Pattern) end,
    Actions = [{PrintFreq,  <<"?">>},
	       {PrintFreq,  <<"??">>},
	       {PrintCount, <<"GGT">>},
	       {PrintCount, <<"GGTA">>},
	       {PrintCount, <<"GGTATT">>},
	       {PrintCount, <<"GGTATTTTAATT">>},
	       {PrintCount, <<"GGTATTTTAATTTATAGT">>}],

    Pids = [ do(Action, Seq) || Action <- Actions ],
    %Pass token to print in right order
    hd(Pids) ! tl(Pids) ++ [self()],
    receive _Pid -> ok end.
