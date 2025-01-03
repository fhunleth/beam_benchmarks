# BeamBenchmarks

[![Hex version](https://img.shields.io/hexpm/v/beam_benchmarks.svg "Hex version")](https://hex.pm/packages/beam_benchmarks)
[![API docs](https://img.shields.io/hexpm/v/beam_benchmarks.svg?label=hexdocs "API docs")](https://hexdocs.pm/beam_benchmarks/doc/BeamBenchmarks.html)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/fhunleth/beam_benchmarks/tree/main.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/fhunleth/beam_benchmarks/tree/main)

This is a collection of Erlang benchmarks copied from other projects for ease of
use. This project makes these benchmarks a hex dependency and helper function
away. Some documentation is copied here for convenience, but please see
the source projects for more complete information.

Benchmarks include:

Name   | Source   | Description
------ | -------- | -----------
`bang` | [bencherl][bencherl] |	A benchmark for many-to-one message passing that spawns one receiver and multiple senders that flood the receiver with messages.
`big`  | [bencherl][bencherl] | A benchmark that implements a many-to-many message passing scenario.
`binary_trees` | [benchmarksgame][benchmarksgame] | Allocate and deallocate many many binary trees
`chameneos_redux` | [benchmarksgame][benchmarksgame] | Symmetrical thread rendezvous requests
`ehb`  | [bencherl][bencherl] |	This is an implementation of *hackbench* in Erlang, a benchmark and stress test for Linux schedulers.
`estone` | [estone_SUITE.erl][estone_SUITE.erl] | This is a suite of benchmarks that measure performance of various Erlang primitives.
`ets_test` | [bencherl][bencherl] | This benchmark creates an ETS table and spawns several readers and writers that perform a certain number of reads (lookups) and writes (inserts), respectively, to that table.
`fannkuch_redux` | [benchmarksgame][benchmarksgame] | Flipping pancakes
`fasta` | [benchmarksgame][benchmarksgame] | Generate and write random DNA sequences
`genstress` | [bencherl][bencherl] | This is a generic server benchmark that spawns an echo server and a number of clients.
`k_nucleotide` | [benchmarksgame][benchmarksgame] | Hashtable update and k-nucleotide strings
`mandelbrot` | [benchmarksgame][benchmarksgame] | Generate Mandelbrot set portable bitmap file
`mbrot` | [bencherl][bencherl] | This benchmark extrapolates the coordinates of a 2-D complex plane that correspond to the pixels of a 2-D image of a specific resolution.
`nbody` | [benchmarksgame][benchmarksgame] | Model the orbits of Jovian planets using a simple symplectic-integrator
`orbit_int` | [bencherl][bencherl] | This benchmark operates on a distributed hash table, and follows a master/worker architecture.
`parallel`  | [bencherl][bencherl] | A benchmark for parallel execution that spawns a number of processes, each of which creates a list of $N$ timestamps and, after it checks that each element of the list is strictly greater than its previous one (as promised by the implementation of erlang:now/0), it sends the result to its parent.
`pcmark` | [bencherl][bencherl] | This benchmark is also about ETS operations. It creates five ETS tables, fills them with values, and then spawns a certain number of processes that read the contents of those tables and update them. As soon as one process finishes, a new process is spawned, until a certain total number of processes has been reached. The benchmark is parameterized by the number of initial processes and the total number of processes.
`pidigits` | [benchmarksgame][benchmarksgame] | Streaming arbitrary-precision arithmetic
`ran` | [bencherl][bencherl] | Another benchmark for parallel execution that spawns a certain number of processes, each of which generates a list of ten thousand random integers, sorts it and sends its first half to the parent process. The benchmark receives the number of processes as a parameter.
`regex_redux` | [benchmarksgame][benchmarksgame] | Match DNA 8-mers and substitute magic patterns
`reverse_complement` | [benchmarksgame][benchmarksgame] | Read DNA sequences - write their reverse-complement
`serialmsg` | [bencherl][bencherl] | A benchmark about message proxying through a dispatcher. The benchmark spawns a certain number of receivers, one dispatcher, and a certain number of generators. The dispatcher forwards the messages that it receives from generators to the appropriate receiver. Each generator sends a number of messages to a specific receiver.
`spectral_norm` | [benchmarksgame][benchmarksgame] | The "Hundred-Dollar, Hundred-Digit Challenge Problem"
`thread_ring` | [benchmarksgame][benchmarksgame] | Switch from thread to thread passing one token
`timer_wheel` | [bencherl][bencherl] | A timer management benchmark that spawns a certain number of processes that exchange *ping* and *pong* messages.

[bencherl]: https://github.com/softlab-ntua/bencherl
[benchmarksgame]: https://madnight.github.io/benchmarksgame/
[estone_SUITE.erl]: https://github.com/erlang/otp/blob/maint-27/erts/emulator/test/estone_SUITE.erl

## Installation

Adding `beam_benchmarks` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
      {:beam_benchmarks, "~> 0.1"}
  ]
end
```

## Sample run

Here's an example run of the EStone benchmark from the Erlang/OTP source code.

```elixir
iex> BeamBenchmarks.estone
EStone test completed
**** CPU speed UNKNOWN MHz ****
**** Total time 0.95421 seconds ****
**** ESTONES = 696638 ****

    Title                            Millis        Estone       %    Loops

{'ESTONES',696638}
list manipulation                    55              27511        7     6400
small messages                       215             14442        10    1515
medium messages                      220             27669        14    1527
huge messages                        25              19659        4     52
pattern matching                     8               102649       5     1046
traverse                             18              26960        4     2834
Port i/o                             153             29175        12    4800
Work with large dataset              13              21564        3     1193
Work with large local dataset        13              22174        3     1174
Alloc and dealloc                    4               35418        2     3710
Bif dispatch                         12              162170       8     5623
Binary handling                      34              14654        4     581
ets datadictionary                   34              32825        6     342
Generic server (with timeout)        114             22092        9     7977
Small Integer arithmetics            9               32213        3     4157
Float arithmetics                    2               14790        1     5526
Function calls                       12              62344        5     882
Timers                               13              9553         2     2312
Links                                2               18776        1     30
{:comment, 'UNKNOWN MHz, 696638 ESTONES'}
```

All other benchmarks have a similar API that runs the one test and returns the
result, timing, and other info in a `t:BeamBenchmarks.Results.t/0` struct. These
run pretty quickly with default settings, but become much more interesting load
generates when passed bigger arguments.

Here's Benchmarks Game's `pidigits`:

```elixir
iex> BeamBenchmarks.BenchmarksGame.pidigits
%BeamBenchmarks.Results{
  name: :pidigits,
  results: "3141592653\t:10\n5897932384\t:20\n6264338  \t:27\n",
  options: [n: 27],
  duration_us: 4462
}
iex> BeamBenchmarks.BenchmarksGame.pidigits(n: 10000)
%BeamBenchmarks.Results{
  name: :pidigits,
  results: "3141592653" <> ...,
  options: [n: 10000],
  duration_us: 2463606
}
```

## Licensing

Please see the individual source files for their licenses.
