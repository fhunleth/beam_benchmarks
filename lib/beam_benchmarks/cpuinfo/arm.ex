defmodule BeamBenchmarks.CPUInfo.ARM do
  @moduledoc false

  # See https://elixir.bootlin.com/linux/latest/source/arch/arm64/include/asm/cputype.h
  # and https://elixir.bootlin.com/linux/latest/source/arch/arm/include/asm/cputype.h

  def add_fields(info) do
    cpu_infos = for {k, v} <- info.cpu, into: %{}, do: {k, add_arm_fields(v)}
    %{info | cpu: cpu_infos}
  end

  def add_arm_fields(%{vendor_id: vendor_id, model_id: model_id} = processor_info) do
    with {:ok, model} <- processor(model_id) do
      arch = if :erlang.system_info(:wordsize) == 8, do: :aarch64, else: :arm

      processor_info
      |> Map.put(:vendor, vendor(vendor_id))
      |> Map.put(:model, model)
      |> Map.put(:arch, arch)
    end
  end

  def vendor("0x41"), do: "ARM"
  def vendor("0x42"), do: "BRCM"
  def vendor("0x43"), do: "Cavium"
  def vendor("0x44"), do: "DEC"
  def vendor("0x46"), do: "Fujitsu"
  def vendor("0x48"), do: "HISI"
  def vendor("0x50"), do: "APM"
  def vendor("0x51"), do: "Qualcomm"
  def vendor("0x4e"), do: "NVIDIA"
  def vendor("0x61"), do: "Apple"
  def vendor("0x69"), do: "Intel"
  def vendor(other), do: other

  def processor("0xb36"), do: {:ok, "ARM1136"}
  def processor("0xb56"), do: {:ok, "ARM1156"}
  def processor("0xb76"), do: {:ok, "ARM1176"}
  def processor("0xb02"), do: {:ok, "ARM11MPCORE"}
  def processor("0xc08"), do: {:ok, "Cortex-A8"}
  def processor("0xc09"), do: {:ok, "Cortex-A9"}
  def processor("0xc05"), do: {:ok, "Cortex-A5"}
  def processor("0xc07"), do: {:ok, "Cortex-A7"}
  def processor("0xc0d"), do: {:ok, "Cortex-A12"}
  def processor("0xc0e"), do: {:ok, "Cortex-A17"}
  def processor("0xc0f"), do: {:ok, "Cortex-A15"}
  def processor("0xd03"), do: {:ok, "Cortex-A53"}
  def processor("0xd04"), do: {:ok, "Cortex-A35"}
  def processor("0xd05"), do: {:ok, "Cortex-A55"}
  def processor("0xd07"), do: {:ok, "Cortex-A57"}
  def processor("0xd0f"), do: {:ok, "AEM-V8"}
  def processor("0xd08"), do: {:ok, "Cortex-A72"}
  def processor("0xd09"), do: {:ok, "Cortex-A73"}
  def processor("0xd0a"), do: {:ok, "Cortex-A75"}
  def processor("0xd0b"), do: {:ok, "Cortex-A76"}
  def processor("0xd0c"), do: {:ok, "Neoverse-N1"}
  def processor("0xd0d"), do: {:ok, "Cortex-A77"}
  def processor(_other), do: :error
end
