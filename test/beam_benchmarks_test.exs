# SPDX-FileCopyrightText: 2024 Frank Hunleth
#
# SPDX-License-Identifier: Apache-2.0

defmodule BeamBenchmarksTest do
  use ExUnit.Case
  doctest BeamBenchmarks

  describe "estone_SUITE tests" do
    test "estone" do
      assert {:comment, _} = BeamBenchmarks.estone()
    end

    test "estone_bench" do
      results = BeamBenchmarks.estone_bench()
      assert length(results) == 19
    end
  end

  describe "bencherl tests" do
    test "mbrot" do
      results = BeamBenchmarks.mbrot()

      assert length(results) == 1
      result = hd(results)

      assert result[:test] == :mbrot
      assert Map.has_key?(result, :time)
      assert Map.has_key?(result, :args)
    end

    test "bang" do
      results = BeamBenchmarks.bang()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :bang
    end

    test "ehb" do
      results = BeamBenchmarks.ehb()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :ehb
    end

    # Doesn't complete in a reasonable amount of time
    # test "ets_random_ops" do
    #   results = BeamBenchmarks.ets_random_ops()
    #   assert length(results) == 1
    #   result = hd(results)
    #   assert result[:test] == :ets_random_ops
    # end

    test "orbit_int" do
      results = BeamBenchmarks.orbit_int()
      assert length(results) == 2
      result = hd(results)
      assert result[:test] == :orbit_int
    end

    test "ran" do
      results = BeamBenchmarks.ran()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :ran
    end

    test "ets_test" do
      results = BeamBenchmarks.ets_test()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :ets_test
    end

    test "parallel" do
      results = BeamBenchmarks.parallel()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :parallel
    end

    test "serialmsg" do
      results = BeamBenchmarks.serialmsg()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :serialmsg
    end

    test "big" do
      results = BeamBenchmarks.big()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :big
    end

    test "genstress" do
      results = BeamBenchmarks.genstress()
      assert length(results) == 2
      result = hd(results)
      assert result[:test] == :genstress
    end

    test "moves" do
      results = BeamBenchmarks.moves()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :moves
    end

    test "pcmark" do
      results = BeamBenchmarks.pcmark()
      assert length(results) == 1
      result = hd(results)
      assert result[:test] == :pcmark
    end

    test "timer_wheel" do
      results = BeamBenchmarks.timer_wheel()
      assert length(results) == 2
      result = hd(results)
      assert result[:test] == :timer_wheel
    end
  end
end
