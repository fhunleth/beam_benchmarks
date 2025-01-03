defmodule NervesBench.MixProject do
  use Mix.Project

  @version "0.2.0"
  @source_url "https://github.com/fhunleth/beam_benchmarks"

  def project do
    [
      app: :beam_benchmarks,
      version: @version,
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      compilers: [:elixir_make | Mix.compilers()],
      description: description(),
      package: package(),
      docs: docs(),
      make_targets: ["all"],
      make_clean: ["mix_clean"],
      make_error_message: "",
      deps: deps(),
      dialyzer: [
        flags: [:missing_return, :extra_return, :unmatched_returns, :error_handling, :underspecs]
      ],
      preferred_cli_env: %{
        docs: :docs,
        "hex.publish": :docs,
        "hex.build": :docs
      }
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

  defp deps do
    [
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:dialyxir, "~> 1.2", only: :dev, runtime: false},
      {:elixir_make, "~> 0.6", runtime: false},
      {:ex_doc, "~> 0.26", only: :docs, runtime: false}
    ]
  end

  defp description do
    "Various BEAM benchmarks in one place"
  end

  defp package do
    [
      # See source files for licenses
      licenses: ["ErlPL-1.1", "Apache-2.0", "BSD-3-Clause"],
      links: %{"Github" => @source_url}
    ]
  end

  defp docs do
    [
      extras: ["README.md", "CHANGELOG.md"],
      main: "readme",
      source_ref: "v#{@version}",
      source_url: @source_url,
      skip_undefined_reference_warnings_on: ["CHANGELOG.md"]
    ]
  end
end
