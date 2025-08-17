defmodule KaitaiToolkit.MixProject do
  use Mix.Project

  def project do
    [
      app: :kaitai_toolkit,
      version: "0.0.15",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: [
        description:
          "Kaitai Toolkit is a library for directly interpreting the Kaitai Struct language for common tasks.",
        licenses: ["MIT"],
        links: %{
          "github" => "https://github.com/polymorfiq/kaitai_toolkit_ex"
        },
        source_url: "https://github.com/polymorfiq/kaitai_toolkit_ex",
        homepage_url: "https://github.com/polymorfiq/kaitai_toolkit_ex/"
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:kaitai_struct, "~> 0.1.11"},
      {:yaml_elixir, "~> 2.11"},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
