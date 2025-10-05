defmodule Whoishiring.MixProject do
  use Mix.Project

  def project do
    [
      app: :whoishiring,
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:inets, :logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      {:jsx, git: "https://github.com/talentdeficit/jsx.git", tag: "master"}
    ]
  end
end

defmodule Mix.Tasks.Hello do
  use Mix.Task

  def run(_) do
    Mix.shell().info("Hello world")
  end
end
