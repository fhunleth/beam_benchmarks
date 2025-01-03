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
end
