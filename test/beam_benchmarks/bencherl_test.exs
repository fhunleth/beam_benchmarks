defmodule BeamBenchmarks.BencherlTest do
  use ExUnit.Case
  alias BeamBenchmarks.Bencherl

  doctest Bencherl

  test "mbrot" do
    result = Bencherl.mbrot()

    assert result.name == :mbrot
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "bang" do
    result = Bencherl.bang()

    assert result.name == :bang
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "ehb" do
    result = Bencherl.ehb()

    assert result.name == :ehb
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  # Doesn't complete in a reasonable amount of time
  # test "ets_random_ops" do
  #   result = Bencherl.ets_random_ops()
  #   assert result.name == :ets_random_ops
  #   assert result.options == [version: :short]
  #   assert [{_arg, :ok}] = result.results
  # end

  test "orbit_int" do
    result = Bencherl.orbit_int()

    assert result.name == :orbit_int
    assert result.options == [version: :short]
    assert [{_arg, {:size, _value}}, {_arg2, {:size, _value2}}] = result.results
  end

  test "ran" do
    result = Bencherl.ran()

    assert result.name == :ran
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "ets_test" do
    result = Bencherl.ets_test()

    assert result.name == :ets_test
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "parallel" do
    result = Bencherl.parallel()

    assert result.name == :parallel
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "serialmsg" do
    result = Bencherl.serialmsg()

    assert result.name == :serialmsg
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "big" do
    result = Bencherl.big()

    assert result.name == :big
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "genstress" do
    result = Bencherl.genstress()

    assert result.name == :genstress
    assert result.options == [version: :short]
    assert [{_arg, :ok}, {_arg2, :ok}] = result.results
  end

  test "moves" do
    result = Bencherl.moves()

    assert result.name == :moves
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "pcmark" do
    result = Bencherl.pcmark()

    assert result.name == :pcmark
    assert result.options == [version: :short]
    assert [{_arg, :ok}] = result.results
  end

  test "timer_wheel" do
    result = Bencherl.timer_wheel()

    assert result.name == :timer_wheel
    assert result.options == [version: :short]
    assert [{_arg, :ok}, {_arg2, :ok}] = result.results
  end
end
