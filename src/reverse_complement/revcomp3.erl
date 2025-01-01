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
%%
%% contributed by Vlad Balin
%% optimizations by Fredrik Svahn

-module(revcomp3).
-compile( [ inline, { inline_size, 100 } ] ).

-export([main/0]).

main() ->
    register(print_server, self()),
    print_server ! flush_queue_empty,
    io:setopts( [ binary ] ),
    loop([]).

loop( Buffer ) ->
    case io:get_line('') of
        eof ->
	    % block until previous output process is done
	    receive flush_queue_empty -> ok end,
	    flush( Buffer, << >> );
        << ">", _/bytes >> = Head ->
	    % block until previous output process is done
	    receive flush_queue_empty -> ok end,
	    % spawn output process and continue to read in main thread
            spawn(fun()-> flush( Buffer, Head ) end),
	    loop( [] );
        Line -> loop( [ rev_comp_line( Line, <<>> ) | Buffer] )
    end.

%% flush( Buffer, Suffix ) -> atom().
%% Buffer = Suffix = iolist().
%% Format and write Buffer with sequence followed by Suffix text
flush( Buffer, Suffix ) ->
    io:put_chars( format( iolist_to_binary(Buffer), Suffix ) ),
    print_server ! flush_queue_empty.

%% format( Buffer, Suffix ) -> iolist().
%% Buffer = bytes(), Suffix = iolist().
%% Split Buffer into 60-char lines, append Suffix to the end of buffer.
format( << Line:60/bytes, Rest/bytes >>, Suffix ) -> [ Line, 10 | format( Rest, Suffix ) ];
format( << >>, Suffix ) -> Suffix;
format( Line, Suffix ) -> [ Line, 10, Suffix ].

%% rev_comp_line( Line, Buffer ) -> Buffer.
%% Line = binary().
%% Buffer = binary().
rev_comp_line( << _:8 >>, Buffer ) -> Buffer;
rev_comp_line( << H, Rest/bytes >>, Buffer ) ->
    C = rev_comp( H ),
    rev_comp_line( Rest, << C:8, Buffer/binary >> ).

rev_comp( $A ) -> $T;
rev_comp( $C ) -> $G;
rev_comp( $G ) -> $C;
rev_comp( $T ) -> $A;
rev_comp( $U ) -> $A;
rev_comp( $M ) -> $K;
rev_comp( $R ) -> $Y;
rev_comp( $Y ) -> $R;
rev_comp( $K ) -> $M;
rev_comp( $V ) -> $B;
rev_comp( $H ) -> $D;
rev_comp( $D ) -> $H;
rev_comp( $B ) -> $V;
rev_comp( $a ) -> $T;
rev_comp( $c ) -> $G;
rev_comp( $g ) -> $C;
rev_comp( $t ) -> $A;
rev_comp( $u ) -> $A;
rev_comp( $m ) -> $K;
rev_comp( $r ) -> $Y;
rev_comp( $y ) -> $R;
rev_comp( $k ) -> $M;
rev_comp( $v ) -> $B;
rev_comp( $h ) -> $D;
rev_comp( $d ) -> $H;
rev_comp( $b ) -> $V;
rev_comp( $N ) -> $N;
rev_comp( $S ) -> $S;
rev_comp( $W ) -> $W;
rev_comp( $n ) -> $N;
rev_comp( $s ) -> $S;
rev_comp( $w ) -> $W;
rev_comp( _ ) -> $?.
