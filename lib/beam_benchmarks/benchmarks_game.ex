defmodule BeamBenchmarks.BenchmarksGame do
  @moduledoc false

  # From https://github.com/madnight/benchmarksgame/tree/master
  require BeamBenchmarks

  @spec nbody(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def nbody(opts \\ []) do
    opts = Keyword.merge([n: 1000], opts)
    BeamBenchmarks.run(opts, do: :nbody.main(opts[:n]))
  end

  @spec fannkuch_redux(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def fannkuch_redux(opts \\ []) do
    opts = Keyword.merge([n: 7], opts)
    BeamBenchmarks.run(opts, do: :fannkuchredux.main(opts[:n]))
  end

  @spec spectral_norm(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def spectral_norm(opts \\ []) do
    opts = Keyword.merge([n: 100], opts)

    # Use the second implementation since it's faster
    BeamBenchmarks.run(opts, do: :spectralnorm2.main(opts[:n]))
  end

  @spec binary_trees(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def binary_trees(opts \\ []) do
    opts = Keyword.merge([n: 10], opts)

    BeamBenchmarks.run(opts, do: :binarytrees2.main(opts[:n]))
  end

  @spec chameneos_redux(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def chameneos_redux(opts \\ []) do
    opts = Keyword.merge([n: 10], opts)

    {:ok, fd} = StringIO.open("")

    BeamBenchmarks.run opts do
      :chameneosredux.main(fd, opts[:n])
      {:ok, {_in, out}} = StringIO.close(fd)
      out
    end
  end

  @spec fasta(n: non_neg_integer(), io: IO.device()) :: BeamBenchmarks.Results.t()
  def fasta(opts \\ []) do
    opts = Keyword.merge([n: 1000], opts)

    if opts[:io] == nil do
      raise ArgumentError, "You must provide an IO device for the `io` option"
    end

    BeamBenchmarks.run(opts, do: :fasta2.main(opts[:io], opts[:n]))
  end

  @spec k_nucleotide(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def k_nucleotide(opts \\ []) do
    opts = Keyword.merge([n: 10], opts)

    BeamBenchmarks.run(opts, do: :knucleotide.main(opts[:n]))
  end

  @spec mandelbrot(n: non_neg_integer(), io: IO.device()) :: BeamBenchmarks.Results.t()
  def mandelbrot(opts \\ []) do
    opts = Keyword.merge([n: 200], opts)

    if opts[:io] == nil do
      raise ArgumentError, "You must provide an IO device for the `io` option"
    end

    BeamBenchmarks.run(opts, do: :mandelbrot.main(opts[:io], opts[:n]))
  end

  @spec pidigits(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def pidigits(opts \\ []) do
    opts = Keyword.merge([n: 27], opts)
    {:ok, fd} = StringIO.open("")

    BeamBenchmarks.run opts do
      :pidigits.main(fd, opts[:n])
      {:ok, {_in, out}} = StringIO.close(fd)
      out
    end
  end

  @spec regex_redux(keyword()) :: BeamBenchmarks.Results.t()
  def regex_redux(opts \\ []) do
    BeamBenchmarks.run(opts, do: :regexredux6.main())
  end

  @spec reverse_complement(keyword()) :: BeamBenchmarks.Results.t()
  def reverse_complement(opts \\ []) do
    BeamBenchmarks.run(opts, do: :revcomp4.main())
  end

  @spec thread_ring(n: non_neg_integer()) :: BeamBenchmarks.Results.t()
  def thread_ring(opts \\ []) do
    opts = Keyword.merge([n: 1000], opts)
    BeamBenchmarks.run(opts, do: :threadring3.main(opts[:n]))
  end
end
