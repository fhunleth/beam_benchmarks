defmodule BeamBenchmarks.Results do
  @moduledoc """
  Common results struct for benchmarks
  """
  defstruct [:name, :results, :options, :duration_ms]

  @typedoc """
  Results of a benchmark run

  * `name` - the name of the benchmark
  * `options` - the options passed to the benchmark
  * `results` - the return value from the benchmark
  * `duration_ms` - the duration of the benchmark in milliseconds
  """
  @type t() :: %__MODULE__{
          name: atom(),
          results: any(),
          options: keyword(),
          duration_ms: non_neg_integer()
        }
end