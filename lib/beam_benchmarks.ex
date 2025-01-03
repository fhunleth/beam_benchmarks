# SPDX-FileCopyrightText: 2024 Frank Hunleth
#
# SPDX-License-Identifier: Apache-2.0

defmodule BeamBenchmarks do
  @moduledoc """
  BEAM Benchmarks

  See functions for supported benchmarks.
  """

  @type estone_options() :: [data_dir: charlist()]

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
end
