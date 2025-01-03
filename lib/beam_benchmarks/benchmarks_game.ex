defmodule BeamBenchmarks.BenchmarksGame do
  @moduledoc """
  Benchmarks Game benchmarks

  See https://madnight.github.io/benchmarksgame/
  """

  alias BeamBenchmarks.Results

  defmacrop run(options, do: block) do
    {name, _arity} = __CALLER__.function

    quote do
      {duration_us, result} = :timer.tc(fn -> unquote(block) end)

      %Results{
        name: unquote(name),
        options: unquote(options),
        results: result,
        duration_us: duration_us
      }
    end
  end

  @doc """
  Run Benchmark Game's n-body test

  Model the orbits of Jovian planets, using the same simple symplectic-integrator.

  https://madnight.github.io/benchmarksgame/nbody-description.html#nbody

  Options:
  * `:n` - the number of iterations to run. Default is 1000.
  """
  @spec nbody(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def nbody(opts \\ []) do
    opts = Keyword.merge([n: 1000], opts)
    run(opts, do: :nbody.main(opts[:n]))
  end

  @doc """
  Run Benchmark Game's Fannkuch benchmark

  The fannkuch benchmark is defined by programs in [Performing Lisp Analysis of the FANNKUCH Benchmark](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.35.5124), Kenneth R. Anderson and Duane Rettig. FANNKUCH is an abbreviation for the German word Pfannkuchen, or pancakes, in analogy to flipping pancakes. The conjecture is that the maximum count is approximated by n*log(n) when n goes to infinity.

  https://madnight.github.io/benchmarksgame/fannkuchredux-description.html#fannkuchredux

  Options:
  * `:n` - the number of iterations to run. Default is 1000.
  """
  @spec fannkuch_redux(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def fannkuch_redux(opts \\ []) do
    opts = Keyword.merge([n: 7], opts)
    run(opts, do: :fannkuchredux.main(opts[:n]))
  end

  @doc """
  Run Benchmark Game's Spectral benchmark

  MathWorld: ["Hundred-Dollar, Hundred-Digit Challenge Problems"](http://mathworld.wolfram.com/Hundred-DollarHundred-DigitChallengeProblems.html), [Challenge #3](http://mathworld.wolfram.com/SpectralNorm.html).

  https://madnight.github.io/benchmarksgame/spectralnorm-description.html#spectralnorm

  Options:
  * `:n` - the number of iterations to run. Default is 1000.
  """
  @spec spectral_norm(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def spectral_norm(opts \\ []) do
    opts = Keyword.merge([n: 100], opts)

    # Use the second implementation since it's faster
    run(opts, do: :spectralnorm2.main(opts[:n]))
  end

  @doc """
  Run Benchmark Game's Binary Trees benchmark

  Allocate and deallocate many many binary trees

  https://madnight.github.io/benchmarksgame/binarytrees-description.html#binarytrees

  Options:
  * `:n` - the number of iterations to run. Default is 10.
  """
  @spec binary_trees(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def binary_trees(opts \\ []) do
    opts = Keyword.merge([n: 10], opts)

    run(opts, do: :binarytrees2.main(opts[:n]))
  end

  @doc """
  Run Benchmark Game's Chameneos Redux benchmark

  Symmetrical thread rendezvous requests

  https://madnight.github.io/benchmarksgame/chameneosredux-description.html#chameneosredux

  Options:
  * `:n` - the number of iterations to run. Default is 600.
  """
  @spec chameneos_redux(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def chameneos_redux(opts \\ []) do
    opts = Keyword.merge([n: 10], opts)

    {:ok, fd} = StringIO.open("")

    run opts do
      :chameneosredux.main(fd, opts[:n])
      {:ok, {_in, out}} = StringIO.close(fd)
      out
    end
  end

  @doc """
  Run Benchmark Game's Fasta benchmark

  Generate and write random DNA sequences

  https://madnight.github.io/benchmarksgame/fasta-description.html#fasta

  Options:
  * `:io` - the IO device to write the output to. Required.
  * `:n` - the number of iterations to run. Default is 1000.
  """
  @spec fasta(n: non_neg_integer(), io: IO.device()) :: BeamBenchmarks.Results.t()
  def fasta(opts \\ []) do
    opts = Keyword.merge([n: 1000], opts)

    if opts[:io] == nil do
      raise ArgumentError, "You must provide an IO device for the `io` option"
    end

    run(opts, do: :fasta2.main(opts[:io], opts[:n]))
  end

  @doc """
  Run Benchmark Game's K-Nucleotide benchmark

  Hashtable update and k-nucleotide strings

  https://madnight.github.io/benchmarksgame/knucleotide-description.html#knucleotide

  Options:
  * `:n` - the number of iterations to run. Default is 10.
  """
  @spec k_nucleotide(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def k_nucleotide(opts \\ []) do
    opts = Keyword.merge([n: 10], opts)

    run(opts, do: :knucleotide.main(opts[:n]))
  end

  @doc """
  Run Benchmark Game's Mandelbrot benchmark

  Generate Mandelbrot set portable bitmap file

  https://madnight.github.io/benchmarksgame/mandelbrot-description.html#mandelbrot

  Options:
  * `:io` - the IO device to write the output to. Required.
  * `:n` - the width of the square Mandelbrot picture. Default is 200.
  """
  @spec mandelbrot(n: non_neg_integer(), io: IO.device()) :: BeamBenchmarks.Results.t()
  def mandelbrot(opts \\ []) do
    opts = Keyword.merge([n: 200], opts)

    if opts[:io] == nil do
      raise ArgumentError, "You must provide an IO device for the `io` option"
    end

    run(opts, do: :mandelbrot.main(opts[:io], opts[:n]))
  end

  @doc """
  Run Benchmark Game's Pi Digits benchmark

  Streaming arbitrary-precision arithmetic

  https://madnight.github.io/benchmarksgame/pidigits-description.html#pidigits

  Options:
  * `:n` - the number of digits to calculate. Default is 27.
  """
  @spec pidigits(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def pidigits(opts \\ []) do
    opts = Keyword.merge([n: 27], opts)
    {:ok, fd} = StringIO.open("")

    run opts do
      :pidigits.main(fd, opts[:n])
      {:ok, {_in, out}} = StringIO.close(fd)
      out
    end
  end

  @doc """
  Run Benchmark Game's Regex Redux benchmark

  Match DNA 8-mers and substitute magic patterns

  https://madnight.github.io/benchmarksgame/regexredux-description.html#regexredux

  Options:
  * `:n` - the number of iterations to run. Default is 10.
  """
  @spec regex_redux(keyword()) :: BeamBenchmarks.Results.t()
  def regex_redux(opts \\ []) do
    run(opts, do: :regexredux6.main())
  end

  @doc """
  Run Benchmark Game's Reverse Complement benchmark

  Read DNA sequences - write their reverse-complement

  https://madnight.github.io/benchmarksgame/revcomp-description.html#revcomp

  Options:
  * `:n` - the number of iterations to run. Default is 10.
  """
  @spec reverse_complement(keyword()) :: BeamBenchmarks.Results.t()
  def reverse_complement(opts \\ []) do
    run(opts, do: :revcomp4.main())
  end

  @doc """
  Run Benchmark Game's Thread Ring benchmark

  Switch from thread to thread passing one token

  https://madnight.github.io/benchmarksgame/threadring-description.html#threadring

  Options:
  * `:n` - the number of messages to pass around the ring. Default is 1000.
  """
  @spec thread_ring(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def thread_ring(opts \\ []) do
    opts = Keyword.merge([n: 1000], opts)
    run(opts, do: :threadring3.main(opts[:n]))
  end
end
