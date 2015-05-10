defmodule Flyingfox.Mixfile do
  use Mix.Project

  def project do
    [app: :flyingfox,
     version: "0.0.1",
     elixir: "~> 1.0",
     escript: [main_module: Api],
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [{:message_pack, "~> 0.1.4"}]
  end
end
