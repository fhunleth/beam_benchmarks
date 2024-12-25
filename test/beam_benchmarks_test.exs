# SPDX-FileCopyrightText: 2024 Frank Hunleth
#
# SPDX-License-Identifier: Apache-2.0

defmodule BeamBenchmarksTest do
  use ExUnit.Case
  doctest BeamBenchmarks

  test "estone" do
    assert {:comment, _} = BeamBenchmarks.estone()
  end

  describe "bencherl tests" do
    test "mbrot" do
      results = BeamBenchmarks.run_bencherl_test(:mbrot, :short)

      assert length(results) == 1
      result = hd(results)

      assert result[:test] == :mbrot
      assert Map.has_key?(result, :time)
      assert Map.has_key?(result, :args)
    end
  end
end
