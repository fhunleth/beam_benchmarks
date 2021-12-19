defmodule BeamBenchmarks.Cpuinfo do
  @moduledoc """
  Return CPU information in /proc/cpuinfo
  """

  @type info() :: %{num_cpu: non_neg_integer(), cpu: %{}}

  @doc """
  Read `/proc/cpuinfo`
  """
  @spec read!() :: info()
  def read!() do
    File.read!("/proc/cpuinfo")
    |> parse!()
  end

  @doc """
  Parse the contents of `/proc/cpuinfo`
  """
  @spec parse!(String.t()) :: info()
  def parse!(contents) do
    contents
    |> String.split("\n")
    |> Enum.reduce(initial_state(), &parse_line/2)
    |> finalize_result()
  end

  defp initial_state() do
    %{num_cpu: 0, cpu: :unknown, result: %{cpu: %{}}}
  end

  defp finalize_result(state) do
    state.result
    |> Map.put(:num_cpu, length(Map.keys(state.result.cpu)))
  end

  defp parse_line(line, state) do
    case Regex.run(~r/(.*?)\s*:\s+(.*)$/, line) do
      [_, key, value] -> handle(state, key, value)
      _ -> %{state | cpu: :unknown}
    end
  end

  defp handle(state, "processor", value) do
    cpu = String.to_integer(value)
    result = state.result

    %{state | cpu: cpu, result: put_in(result.cpu[cpu], %{})}
  end

  # Match lscpu naming
  defp handle(state, "ASEs implemented", value), do: attr(state, :flags, value)
  defp handle(state, "BogoMIPS", value), do: attr(state, :bogomips, value)
  defp handle(state, "CPU architecture", value), do: attr(state, :architecture, value)
  defp handle(state, "CPU implementer", value), do: attr(state, :vendor, value)
  defp handle(state, "CPU part", value), do: attr(state, :model, value)
  defp handle(state, "CPU revision", value), do: attr(state, :revision, value)
  defp handle(state, "CPU variant", value), do: attr(state, :stepping, value)
  defp handle(state, "Features", value), do: attr(state, :flags, value)
  defp handle(state, "address sizes", value), do: attr(state, :addrsz, value)
  defp handle(state, "bogomips per cpu", value), do: attr(state, :bogomips, value)
  defp handle(state, "cpu", value), do: attr(state, :model_name, value)
  defp handle(state, "cpu family", value), do: attr(state, :family, value)
  defp handle(state, "cpu model", value), do: attr(state, :model, value)
  defp handle(state, "family", value), do: attr(state, :family, value)
  defp handle(state, "features", value), do: attr(state, :flags, value)
  defp handle(state, "flags", value), do: attr(state, :flags, value)
  defp handle(state, "max thread id", value), do: attr(state, :mtid, value)
  defp handle(state, "model", value), do: attr(state, :model, value)
  defp handle(state, "model name", value), do: attr(state, :model_name, value)
  defp handle(state, "revision", value), do: attr(state, :revision, value)
  defp handle(state, "stepping", value), do: attr(state, :stepping, value)
  defp handle(state, "type", value), do: attr(state, :flags, value)
  defp handle(state, "vendor", value), do: attr(state, :vendor, value)
  defp handle(state, "vendor_id", value), do: attr(state, :vendor, value)
  defp handle(state, _key, _value), do: state

  defp attr(%{cpu: :unknown} = state, _key, _value) do
    # Skip attributes not associated with CPUs for now
    state
  end

  defp attr(state, key, value) do
    put_in(state.result.cpu[state.cpu][key], value)
  end
end
