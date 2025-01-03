defmodule BeamBenchmarks.BenchmarksGameTest do
  use ExUnit.Case
  alias BeamBenchmarks.BenchmarksGame
  doctest BenchmarksGame

  test "nbody" do
    result = BenchmarksGame.nbody(n: 1000)

    assert result.name == :nbody
    assert result.options == [n: 1000]

    {starting_energy, ending_energy} = result.results
    assert_in_delta starting_energy, -0.169075164, 0.000000001
    assert_in_delta ending_energy, -0.169087605, 0.000000001
  end

  test "fannkuch_redux" do
    result = BenchmarksGame.fannkuch_redux(n: 7)

    assert result.name == :fannkuch_redux
    assert result.options == [n: 7]

    {max_flips, checksum} = result.results
    assert max_flips == 16
    assert checksum == 228
  end

  test "spectral_norm" do
    result = BenchmarksGame.spectral_norm(n: 100)

    assert result.name == :spectral_norm
    assert result.options == [n: 100]

    assert_in_delta result.results, 1.274219991, 0.000000001
  end

  test "binary_trees" do
    result = BenchmarksGame.binary_trees(n: 10)

    assert result.name == :binary_trees
    assert result.options == [n: 10]
    assert result.results == {4095, 2047}
  end

  defp ndiff_fields_2_10(a, b) do
    lines_a = String.split(a, "\n")
    lines_b = String.split(b, "\n")
    ndiff_helper(lines_a, lines_b)
  end

  defp ndiff_helper([], []), do: []
  defp ndiff_helper([a | rest_a], []), do: [{a, nil} | ndiff_helper(rest_a, [])]
  defp ndiff_helper([], [b | rest_b]), do: [{nil, b} | ndiff_helper([], rest_b)]

  defp ndiff_helper([a | rest_a], [b | rest_b]) do
    a_elements = String.split(a, " ") |> Enum.slice(1, 8)
    b_elements = String.split(b, " ") |> Enum.slice(1, 8)

    if a_elements == b_elements do
      ndiff_helper(rest_a, rest_b)
    else
      # Just return the first difference since that's easier to debug
      # [{a, b} | ndiff_helper(rest_a, rest_b)]
      [{a, b}]
    end
  end

  test "chameneos_redux" do
    result = BenchmarksGame.chameneos_redux(n: 600)

    expected = """
    blue + blue -> blue
    blue + red -> yellow
    blue + yellow -> red
    red + blue -> yellow
    red + red -> red
    red + yellow -> blue
    yellow + blue -> red
    yellow + red -> blue
    yellow + yellow -> yellow

     blue red yellow
    400 zero
    400 zero
    400 zero
     one two zero zero

     blue red yellow red yellow blue red yellow red blue
    120 zero
    120 zero
    120 zero
    120 zero
    120 zero
    120 zero
    120 zero
    120 zero
    120 zero
    120 zero
     one two zero zero

    """

    assert result.name == :chameneos_redux
    assert result.options == [n: 600]
    assert ndiff_fields_2_10(result.results, expected) == []
  end

  test "fasta" do
    {:ok, io} = StringIO.open("")
    result = BenchmarksGame.fasta(n: 1000, io: io)
    {:ok, {_, actual}} = StringIO.close(io)
    expected = File.read!("test/fixtures/fasta-output.txt")

    assert result.name == :fasta
    assert {:n, 1000} in result.options
    assert actual == expected
  end

  # test "k_nucleotide" do
  #   result = BenchmarksGame.k_nucleotide(n: 10)

  #   assert result.name == :k_nucleotide
  #   assert result.options == [n: 10]
  #   assert result.results == :nope
  # end

  @tag :tmp_dir
  test "mandelbrot", %{tmp_dir: tmp_dir} do
    actual_path = Path.join(tmp_dir, "actual.pbm")
    {:ok, io} = File.open(actual_path, [:write])
    result = BenchmarksGame.mandelbrot(n: 200, io: io)
    File.close(io)

    actual = File.read!(actual_path)
    expected = File.read!("test/fixtures/mandelbrot-output.pbm")

    assert result.name == :mandelbrot
    assert {:n, 200} in result.options
    assert actual == expected
  end

  test "pidigits" do
    result = BenchmarksGame.pidigits(n: 27)

    assert result.name == :pidigits
    assert result.options == [n: 27]

    assert result.results == "3141592653\t:10\n5897932384\t:20\n6264338  \t:27\n"
  end

  # test "regex_redux" do
  #   result = BenchmarksGame.regex_redux(n: 10)

  #   assert result.name == :regex_redux
  #   assert result.options == [n: 10]
  #   assert result.results == :nope
  # end

  # Needs update to handle I/O
  # test "reverse_complement" do
  #   result = BenchmarksGame.reverse_complement(n: 10)

  #   assert result.name == :reverse_complement
  #   assert result.options == [n: 10]
  #   assert result.results == :nope
  # end

  test "thread_ring" do
    result = BenchmarksGame.thread_ring(n: 1000)

    assert result.name == :thread_ring
    assert result.options == [n: 1000]
    assert result.results == 498
  end
end
