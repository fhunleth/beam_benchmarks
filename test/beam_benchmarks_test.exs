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
end
