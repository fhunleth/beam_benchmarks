defmodule NervesBench.MixProject do
  use Mix.Project

  @version "0.11.3"
  # @source_url "https://github.com/fhunleth/beam_benchmarks"

  def project do
    [
      app: :beam_benchmarks,
      version: @version,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      compilers: [:elixir_make | Mix.compilers()],
      make_targets: ["all"],
      make_clean: ["mix_clean"],
      make_error_message: "",
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:elixir_make, "~> 0.6", runtime: false}
    ]
  end
end
