defmodule BeamBenchmarks do
  @moduledoc """
  Documentation for `BeamBenchmarks`.
  """
  @bencherl_tests [
    :bang,
    :ehb,
    :ets_random_ops,
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

  @type(bencherl_version() :: :short | :intermediate, :long)

  @doc """
  Return information about the device
  """
  @spec device_info() :: map()
  def device_info() do
    BeamBenchmarks.Info.all_info()
  end

  @doc """
  """
  @spec estone() :: {:comment, charlist()}
  def estone() do
    :estone_SUITE.estone(data_dir: :code.priv_dir(:beam_benchmarks))
  end

  @spec run_bencherl_tests(bencherl_version()) :: [non_neg_integer()]
  def run_bencherl_tests(version \\ :short) do
    @bencherl_tests
    |> Enum.flat_map(&run_bencherl_test(&1, version))
  end

  @spec run_bencherl_test(module(), bencherl_version()) :: [map()]
  def run_bencherl_test(test, version \\ :short) do
    IO.puts("Running #{test}...")
    args = test.bench_args(version, number_of_cores: :erlang.system_info(:schedulers))

    for arg <- args do
      {time, :ok} = :timer.tc(fn -> test.run(hd(args), [], nil) end)
      %{test: test, time: time, args: arg}
    end
  end
end
