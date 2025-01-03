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
% Contributed by John Shahbazian

-module(threadring3).
-export([main/1,process_spawner/2,ring_node/2]).

-define(TOTAL_PROCESSES, 503).

main(N) ->
  Pid = process_spawner(?TOTAL_PROCESSES,[]),
  Pid ! N,
  receive Result -> Result end.

process_spawner(Num_Processes,[]) ->
  Pid = spawn(threadring3,ring_node,[self(), self()]),
  register(list_to_atom("number" ++ integer_to_list(1)),Pid),
  Next_Pid = spawn(threadring3,ring_node,[process_spawner(Num_Processes-2,Pid), self()]),
  register(list_to_atom("number" ++ integer_to_list(2)),Next_Pid),
  Pid ! {update,Next_Pid},
  Pid;
process_spawner(1,Starter_Pid) ->
  Pid = spawn(threadring3,ring_node,[Starter_Pid, self()]),
  register(list_to_atom("number" ++ integer_to_list(?TOTAL_PROCESSES)),Pid),
  Pid;
process_spawner(Num_Processes,Starter_Pid) ->
  Pid = spawn(threadring3,ring_node,[process_spawner(Num_Processes-1,Starter_Pid), self()]),
  register(list_to_atom("number" ++ integer_to_list(?TOTAL_PROCESSES - (Num_Processes-1) )),Pid),
  Pid.

ring_node(Send_To_Pid, Caller_Pid) ->
  receive
    {update, New_Pid} ->
      ring_node(New_Pid, Caller_Pid);
    0 ->
      {_,Process_Name} = process_info(self(),registered_name),
      Process_Name2 = list_to_integer(string:sub_string(atom_to_list(Process_Name),7)),
      Caller_Pid ! Process_Name2;
    N ->
      Send_To_Pid ! (N-1),
      ring_node(Send_To_Pid, Caller_Pid)
  end.
