defmodule BeamBenchmarks.CpuinfoTest do
  use ExUnit.Case

  alias BeamBenchmarks.Cpuinfo

  doctest Cpuinfo

  test "beaglebone" do
    contents = """
    processor	: 0
    model name	: ARMv7 Processor rev 2 (v7l)
    BogoMIPS	: 298.59
    Features	: half thumb fastmult vfp edsp thumbee neon vfpv3 tls vfpd32
    CPU implementer	: 0x41
    CPU architecture: 7
    CPU variant	: 0x3
    CPU part	: 0xc08
    CPU revision	: 2

    Hardware	: Generic AM33XX (Flattened Device Tree)
    Revision	: 0000
    Serial		: 11:22:33:44:55:66
    """

    expected = %{
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

    assert Cpuinfo.parse!(contents) == expected
  end

  test "stm32mp1" do
    contents = """
    processor       : 0
    model name      : ARMv7 Processor rev 5 (v7l)
    BogoMIPS        : 48.00
    Features        : half thumb fastmult vfp edsp thumbee neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm
    CPU implementer : 0x41
    CPU architecture: 7
    CPU variant     : 0x0
    CPU part        : 0xc07
    CPU revision    : 5

    processor       : 1
    model name      : ARMv7 Processor rev 5 (v7l)
    BogoMIPS        : 48.00
    Features        : half thumb fastmult vfp edsp thumbee neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm
    CPU implementer : 0x41
    CPU architecture: 7
    CPU variant     : 0x0
    CPU part        : 0xc07
    CPU revision    : 5

    Hardware        : STM32 (Device Tree Support)
    Revision        : 0000
    Serial          : 0032001C3338510934383330
    """

    expected = %{
      num_cpu: 2,
      cpu: %{
        0 => %{
          architecture: "7",
          bogomips: "48.00",
          flags:
            "half thumb fastmult vfp edsp thumbee neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm",
          model: "0xc07",
          model_name: "ARMv7 Processor rev 5 (v7l)",
          revision: "5",
          stepping: "0x0",
          vendor: "0x41"
        },
        1 => %{
          architecture: "7",
          bogomips: "48.00",
          flags:
            "half thumb fastmult vfp edsp thumbee neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm",
          model: "0xc07",
          model_name: "ARMv7 Processor rev 5 (v7l)",
          revision: "5",
          stepping: "0x0",
          vendor: "0x41"
        }
      }
    }

    assert Cpuinfo.parse!(contents) == expected
  end

  test "amd64" do
    # Abbreviated since this is really long...
    contents = """
    processor       : 0
    vendor_id       : AuthenticAMD
    cpu family      : 23
    model           : 8
    model name      : AMD Ryzen Threadripper 2950X 16-Core Processor
    stepping        : 2
    microcode       : 0x800820d
    cpu MHz         : 1883.424
    cache size      : 512 KB
    physical id     : 0
    siblings        : 32
    core id         : 3
    cpu cores       : 16
    apicid          : 6
    initial apicid  : 6
    fpu             : yes
    fpu_exception   : yes
    cpuid level     : 13
    wp              : yes
    flags           : fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ht syscall nx mmxext fxsr_opt pdpe1gb rdtscp lm constant_tsc rep_good nopl nonstop_tsc cpuid extd_apicid amd_dcm aperfmperf pni pclmulqdq monitor ssse3 fma cx16 sse4_1 sse4_2 movbe popcnt aes xsave avx f16c rdrand lahf_lm cmp_legacy svm extapic cr8_legacy abm sse4a misalignsse 3dnowprefetch osvw skinit wdt tce topoext perfctr_core perfctr_nb bpext perfctr_llc mwaitx cpb hw_pstate sme ssbd sev ibpb vmmcall fsgsbase bmi1 avx2 smep bmi2 rdseed adx smap clflushopt sha_ni xsaveopt xsavec xgetbv1 xsaves clzero irperf xsaveerptr arat npt lbrv svm_lock nrip_save tsc_scale vmcb_clean flushbyasid decodeassists pausefilter pfthreshold avic v_vmsave_vmload vgif overflow_recov succor smca
    bugs            : sysret_ss_attrs null_seg spectre_v1 spectre_v2 spec_store_bypass
    bogomips        : 6986.09
    TLB size        : 2560 4K pages
    clflush size    : 64
    cache_alignment : 64
    address sizes   : 43 bits physical, 48 bits virtual
    power management: ts ttp tm hwpstate cpb eff_freq_ro [13] [14]
    """

    expected = %{
      num_cpu: 1,
      cpu: %{
        0 => %{
          flags:
            "fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ht syscall nx mmxext fxsr_opt pdpe1gb rdtscp lm constant_tsc rep_good nopl nonstop_tsc cpuid extd_apicid amd_dcm aperfmperf pni pclmulqdq monitor ssse3 fma cx16 sse4_1 sse4_2 movbe popcnt aes xsave avx f16c rdrand lahf_lm cmp_legacy svm extapic cr8_legacy abm sse4a misalignsse 3dnowprefetch osvw skinit wdt tce topoext perfctr_core perfctr_nb bpext perfctr_llc mwaitx cpb hw_pstate sme ssbd sev ibpb vmmcall fsgsbase bmi1 avx2 smep bmi2 rdseed adx smap clflushopt sha_ni xsaveopt xsavec xgetbv1 xsaves clzero irperf xsaveerptr arat npt lbrv svm_lock nrip_save tsc_scale vmcb_clean flushbyasid decodeassists pausefilter pfthreshold avic v_vmsave_vmload vgif overflow_recov succor smca",
          model: "8",
          model_name: "AMD Ryzen Threadripper 2950X 16-Core Processor",
          stepping: "2",
          vendor: "AuthenticAMD",
          addrsz: "43 bits physical, 48 bits virtual",
          family: "23"
        }
      }
    }

    assert Cpuinfo.parse!(contents) == expected
  end
end
