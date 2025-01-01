# SPDX-FileCopyrightText: 2024 Frank Hunleth
#
# SPDX-License-Identifier: Apache-2.0

defmodule BeamBenchmarks do
  @moduledoc """
  BEAM Benchmarks

  See functions for supported benchmarks.
  """
  alias BeamBenchmarks.Results

  @typedoc """
  Options for running bencherl tests

  * `version` - run a `:short`, `:intermediate`, `:long` version. Default is `:short`.
  * `number_of_cores` - the number of cores to use. Default is the number of schedulers.
  """
  @type bencherl_options() :: [version: bencherl_version(), number_of_cores: non_neg_integer()]
  @type bencherl_version() :: :short | :intermediate | :long

  @type estone_options() :: [data_dir: charlist()]

  @bencherl_tests [
    :bang,
    :ehb,
    # :ets_random_ops,
    :orbit_int,
    :ran,
    :ets_test,
    :mbrot,
    :parallel,
    :serialmsg,
    :big,
    # :ets_bench,
    :genstress,
    :moves,
    :pcmark,
    :timer_wheel
  ]

  @doc """
  Return information about the device
  """
  @spec device_info() :: BeamBenchmarks.Info.t()
  def device_info() do
    BeamBenchmarks.Info.all_info()
  end

  @doc """
  Run estone_SUITE and pretty print the results

  This is a suite of benchmarks that measure performance of various Erlang
  primitives.

  See `estone_bench/1` for machine readable results.
  """
  @spec estone(estone_options()) :: {:comment, charlist()}
  def estone(opts \\ []) do
    opts
    |> Keyword.put_new(:data_dir, :code.priv_dir(:beam_benchmarks))
    |> :estone_SUITE.estone()
  end

  @doc """
  Run estone_SUITE benchmarks

  This is a suite of benchmarks that measure performance of various Erlang
  primitives.
  """
  @spec estone_bench(estone_options()) :: list()
  def estone_bench(opts \\ []) do
    opts
    |> Keyword.put_new(:data_dir, :code.priv_dir(:beam_benchmarks))
    |> :estone_SUITE.estone_bench()
  end

  @doc """
  Run bencherl's bang test

  A benchmark for many-to-one message passing that spawns one receiver and
  multiple senders that flood the receiver with messages.
  """
  @spec bang(bencherl_options()) :: []
  def bang(opts \\ []) do
    run_bencherl_test(:bang, opts)
  end

  @doc """
  Run bencherl's big test

  A benchmark that implements a many-to-many message passing scenario.
  """
  @spec big(bencherl_options()) :: []
  def big(opts \\ []) do
    run_bencherl_test(:big, opts)
  end

  @doc """
  Run bencherl's ehb test

  This is an implementation of *hackbench* in Erlang, a benchmark and stress
  test for Linux schedulers.
  """
  @spec ehb(bencherl_options()) :: []
  def ehb(opts \\ []) do
    run_bencherl_test(:ehb, opts)
  end

  @doc """
  Run bencherl's ets_test test

  This benchmark creates an ETS table and spawns several readers and writers
  that perform a certain number of reads (lookups) and writes (inserts),
  respectively, to that table.
  """
  @spec ets_test(bencherl_options()) :: []
  def ets_test(opts \\ []) do
    run_bencherl_test(:ets_test, opts)
  end

  @doc """
  Run bencherl's ets_random_ops test

  This benchmark creates an ETS table and spawns several readers and writers
  that perform a certain number of reads (lookups) and writes (inserts),
  respectively, to that table.
  """
  @spec ets_random_ops(bencherl_options()) :: []
  def ets_random_ops(opts \\ []) do
    run_bencherl_test(:ets_random_ops, opts)
  end

  @doc """
  Run bencherl's genstress test

  This is a generic server benchmark that spawns an echo server and a number of
  clients.
  """
  @spec genstress(bencherl_options()) :: []
  def genstress(opts \\ []) do
    run_bencherl_test(:genstress, opts)
  end

  @doc """
  Run bencherl's mbrot test

  This benchmark extrapolates the coordinates of a 2-D complex plane that
  correspond to the pixels of a 2-D image of a specific resolution.
  """
  @spec mbrot(bencherl_options()) :: []
  def mbrot(opts \\ []) do
    run_bencherl_test(:mbrot, opts)
  end

  @doc """
  Run bencherl's orbit_int test

  This benchmark operates on a distributed hash table, and follows a
  master/worker architecture.
  """
  @spec orbit_int(bencherl_options()) :: []
  def orbit_int(opts \\ []) do
    run_bencherl_test(:orbit_int, opts)
  end

  @doc """
  Run bencherl's parallel test

  A benchmark for parallel execution that spawns a number of processes, each of
  which creates a list of $N$ timestamps and, after it checks that each element
  of the list is strictly greater than its previous one (as promised by the
  implementation of erlang:now/0), it sends the result to its parent.
  """
  @spec parallel(bencherl_options()) :: []
  def parallel(opts \\ []) do
    run_bencherl_test(:parallel, opts)
  end

  @doc """
  Run bencherl's pcmark test

  This benchmark is also about ETS operations. It creates five ETS tables,
  fills them with values, and then spawns a certain number of processes that
  read the contents of those tables and update them. As soon as one process
  finishes, a new process is spawned, until a certain total number of processes
  has been reached. The benchmark is parameterized by the number of initial
  processes and the total number of processes.
  """
  @spec pcmark(bencherl_options()) :: []
  def pcmark(opts \\ []) do
    run_bencherl_test(:pcmark, opts)
  end

  @doc """
  Run bencherl's ran test

  Another benchmark for parallel execution that spawns a certain number of
  processes, each of which generates a list of ten thousand random integers,
  sorts it and sends its first half to the parent process. The benchmark
  receives the number of processes as a parameter.
  """
  @spec ran(bencherl_options()) :: []
  def ran(opts \\ []) do
    run_bencherl_test(:ran, opts)
  end

  @doc """
  Run bencherl's moves test

  Parallel benchmark program that solves the moves problem. A description of
  the moves problem can be found in the ETS implementation report that can be
  found here:

  https://github.com/kjellwinblad/ets_impl_project
  """
  @spec moves(bencherl_options()) :: []
  def moves(opts \\ []) do
    run_bencherl_test(:moves, opts)
  end

  @doc """
  Run Benchmark Game's n-body test

  Model the orbits of Jovian planets, using the same simple symplectic-integrator.

  https://madnight.github.io/benchmarksgame/nbody-description.html#nbody

  Options:
  * `:n` - the number of iterations to run. Default is 1000.
  """
  defdelegate nbody(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run Benchmark Game's Fannkuch benchmark

  The fannkuch benchmark is defined by programs in [Performing Lisp Analysis of the FANNKUCH Benchmark](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.35.5124), Kenneth R. Anderson and Duane Rettig. FANNKUCH is an abbreviation for the German word Pfannkuchen, or pancakes, in analogy to flipping pancakes. The conjecture is that the maximum count is approximated by n*log(n) when n goes to infinity.

  https://madnight.github.io/benchmarksgame/fannkuchredux-description.html#fannkuchredux

  Options:
  * `:n` - the number of iterations to run. Default is 1000.
  """
  defdelegate fannkuch_redux(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run Benchmark Game's Spectral benchmark

  MathWorld: ["Hundred-Dollar, Hundred-Digit Challenge Problems"](http://mathworld.wolfram.com/Hundred-DollarHundred-DigitChallengeProblems.html), [Challenge #3](http://mathworld.wolfram.com/SpectralNorm.html).

  https://madnight.github.io/benchmarksgame/spectralnorm-description.html#spectralnorm

  Options:
  * `:n` - the number of iterations to run. Default is 1000.
  """
  defdelegate spectral_norm(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run Benchmark Game's Binary Trees benchmark

  Allocate and deallocate many many binary trees

  https://madnight.github.io/benchmarksgame/binarytrees-description.html#binarytrees

  Options:
  * `:n` - the number of iterations to run. Default is 10.
  """
  defdelegate binary_trees(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run Benchmark Game's Chameneos Redux benchmark

  Symmetrical thread rendezvous requests

  https://madnight.github.io/benchmarksgame/chameneosredux-description.html#chameneosredux

  Options:
  * `:n` - the number of iterations to run. Default is 600.
  """
  defdelegate chameneos_redux(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run Benchmark Game's Fasta benchmark

  Generate and write random DNA sequences

  https://madnight.github.io/benchmarksgame/fasta-description.html#fasta

  Options:
  * `:io` - the IO device to write the output to. Required.
  * `:n` - the number of iterations to run. Default is 1000.
  """
  defdelegate fasta(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  # @doc """
  # Run Benchmark Game's K-Nucleotide benchmark

  # Hashtable update and k-nucleotide strings

  # https://madnight.github.io/benchmarksgame/knucleotide-description.html#knucleotide

  # Options:
  # * `:n` - the number of iterations to run. Default is 10.
  # """
  # defdelegate k_nucleotide(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run Benchmark Game's Mandelbrot benchmark

  Generate Mandelbrot set portable bitmap file

  https://madnight.github.io/benchmarksgame/mandelbrot-description.html#mandelbrot

  Options:
  * `:io` - the IO device to write the output to. Required.
  * `:n` - the width of the square Mandelbrot picture. Default is 200.
  """
  defdelegate mandelbrot(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run Benchmark Game's Pi Digits benchmark

  Streaming arbitrary-precision arithmetic

  https://madnight.github.io/benchmarksgame/pidigits-description.html#pidigits

  Options:
  * `:n` - the number of digits to calculate. Default is 27.
  """
  defdelegate pidigits(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  # @doc """
  # Run Benchmark Game's Regex Redux benchmark

  # Match DNA 8-mers and substitute magic patterns

  # https://madnight.github.io/benchmarksgame/regexredux-description.html#regexredux

  # Options:
  # * `:n` - the number of iterations to run. Default is 10.
  # """
  # defdelegate regex_redux(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  # @doc """
  # Run Benchmark Game's Reverse Complement benchmark

  # Read DNA sequences - write their reverse-complement

  # https://madnight.github.io/benchmarksgame/revcomp-description.html#revcomp

  # Options:
  # * `:n` - the number of iterations to run. Default is 10.
  # """
  # defdelegate reverse_complement(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run Benchmark Game's Thread Ring benchmark

  Switch from thread to thread passing one token

  https://madnight.github.io/benchmarksgame/threadring-description.html#threadring

  Options:
  * `:n` - the number of messages to pass around the ring. Default is 1000.
  """
  defdelegate thread_ring(opts \\ []), to: BeamBenchmarks.BenchmarksGame

  @doc """
  Run bencherl's serialmsg test

  A benchmark about message proxying through a dispatcher. The benchmark spawns
  a certain number of receivers, one dispatcher, and a certain number of
  generators. The dispatcher forwards the messages that it receives from
  generators to the appropriate receiver. Each generator sends a number of
  messages to a specific receiver.
  """
  @spec serialmsg(bencherl_options()) :: []
  def serialmsg(opts \\ []) do
    run_bencherl_test(:serialmsg, opts)
  end

  @doc """
  Run bencherl's timer_wheel test

  A timer management benchmark that spawns a certain number of processes that
  exchange *ping* and *pong* messages.
  """
  @spec timer_wheel(bencherl_options()) :: []
  def timer_wheel(opts \\ []) do
    run_bencherl_test(:timer_wheel, opts)
  end

  @doc """
  Run all bencherl tests
  """
  @spec run_bencherl_tests(bencherl_options()) :: [non_neg_integer()]
  def run_bencherl_tests(opts \\ []) do
    @bencherl_tests
    |> Enum.flat_map(&run_bencherl_test(&1, opts))
  end

  @spec run_bencherl_test(module(), bencherl_options()) :: [map()]
  def run_bencherl_test(test, opts \\ []) do
    version = Keyword.get(opts, :version, :short)

    bencherl_opts =
      opts
      |> Keyword.delete(:version)
      |> Keyword.put_new(:number_of_cores, :erlang.system_info(:schedulers))

    args = test.bench_args(version, bencherl_opts)

    for arg <- args do
      {time, :ok} = :timer.tc(fn -> test.run(arg, [], nil) end)
      %{test: test, time: time, args: arg}
    end
  end

  @doc false
  defmacro run(options, do: block) do
    {name, _arity} = __CALLER__.function

    quote do
      {duration_ms, result} = :timer.tc(fn -> unquote(block) end)

      %Results{
        name: unquote(name),
        options: unquote(options),
        results: result,
        duration_ms: duration_ms
      }
    end
  end
end
