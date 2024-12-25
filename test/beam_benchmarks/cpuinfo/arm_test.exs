defmodule BeamBenchmarks.CPUInfo.ARMTest do
  use ExUnit.Case

  alias BeamBenchmarks.CPUInfo.ARM

  doctest ARM

  test "beaglebone" do
    input = %{
      num_cpu: 1,
      cpu: %{
        0 => %{
          architecture: "7",
          bogomips: "298.59",
          flags: "half thumb fastmult vfp edsp thumbee neon vfpv3 tls vfpd32",
          model: "0xc08",
          model_name: "ARMv7 Processor rev 2 (v7l)",
          revision: "2",
          stepping: "0x3",
          vendor: "0x41"
        }
      }
    }

    expected = %{
      num_cpu: 1,
      cpu: %{
        0 => %{
          architecture: "7",
          bogomips: "298.59",
          flags: "half thumb fastmult vfp edsp thumbee neon vfpv3 tls vfpd32",
          model: "Cortex-A8",
          model_id: "0xc08",
          model_name: "ARMv7 Processor rev 2 (v7l)",
          revision: "2",
          stepping: "0x3",
          vendor_id: "0x41",
          vendor: "ARM"
        }
      }
    }

    assert true
  end
end
