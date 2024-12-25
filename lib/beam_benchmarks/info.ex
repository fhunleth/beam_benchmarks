# SPDX-FileCopyrightText: 2024 Frank Hunleth
#
# SPDX-License-Identifier: Apache-2.0

defmodule BeamBenchmarks.Info do
  @moduledoc """
  Utilities for returning information about the device
  """

  alias BeamBenchmarks.Cpuinfo

  @type t() :: %{
          :elixir_version => String.t(),
          :otp_version => String.t(),
          :erts_version => String.t(),
          :system_architecture => String.t(),
          :schedulers => non_neg_integer(),
          :schedulers_online => non_neg_integer(),
          optional(:device_model) => String.t(),
          optional(:kernel_version) => String.t(),
          optional(:cpu) => map()
        }

  @spec all_info() :: t()
  def all_info() do
    [erts_info(), linux_info(), cpu_info()]
    |> Enum.reduce(%{}, &deep_merge/2)
  end

  defp erts_info() do
    %{
      elixir_version: System.version(),
      otp_version: system_info(:otp_release),
      erts_version: system_info(:version),
      system_architecture: system_info(:system_architecture),
      schedulers: system_info(:schedulers),
      schedulers_online: system_info(:schedulers_online)
    }
  end

  defp linux_info() do
    %{
      kernel_version: safe_read("/proc/sys/kernel/osrelease"),
      device_model: safe_read("/proc/device-tree/model")
    }
  end

  defp cpu_info() do
    deep_merge(Cpuinfo.read!(), all_cpu_frequency())
  end

  defp safe_read(path) when is_list(path), do: safe_read(Path.join(path))

  defp safe_read(path) do
    case File.read(path) do
      {:ok, contents} -> contents |> String.trim_trailing("\0") |> String.trim()
      {:error, _} -> ""
    end
  end

  defp safe_read_integer(path) do
    case safe_read(path) do
      "" -> -1
      str -> String.to_integer(str)
    end
  end

  defp system_info(what) do
    :erlang.system_info(what) |> normalize_strings()
  end

  defp normalize_strings(s) when is_list(s) do
    to_string(s)
  end

  defp normalize_strings(other), do: other

  defp all_cpu_frequency() do
    %{cpu: all_cpu_frequency(%{}, 0)}
  end

  defp all_cpu_frequency(acc, cpu) do
    case cpu_frequency(cpu) do
      nil -> acc
      info -> all_cpu_frequency(Map.put(acc, cpu, info), cpu + 1)
    end
  end

  defp cpu_frequency(cpu) do
    path = "/sys/devices/system/cpu/cpu#{cpu}/cpufreq"

    if File.exists?(path) do
      %{
        frequency_current: safe_read_integer([path, "cpuinfo_cur_freq"]),
        frequency_min: safe_read_integer([path, "cpuinfo_min_freq"]),
        frequency_max: safe_read_integer([path, "cpuinfo_max_freq"]),
        scaling_governor: safe_read([path, "scaling_governor"])
      }
    else
      nil
    end
  end

  defp deep_merge(map1, map2) when is_map(map1) and is_map(map2) do
    Map.merge(map1, map2, fn _key, value1, value2 -> deep_merge(value1, value2) end)
  end

  defp deep_merge(_not_map1, not_map2), do: not_map2
end
