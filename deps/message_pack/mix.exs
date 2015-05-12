defmodule MessagePack.Mixfile do
  use Mix.Project

  def project do
    [ app: :message_pack,
      version: "0.1.4",
      elixir: "~> 1.0.0 or ~> 0.15.1",
      deps: deps(Mix.env),
      build_per_environment: false,

      name: "MessagePack",
      source_url: "https://github.com/mururu/msgpack-elixir",
      description: "MessagePack Implementation for Elixir",
      package: package ]
  end

  def application do
    []
  end

  defp deps(:test) do
    [{ :properex, github: "reset/properex", branch: "elixir-15" },
     { :jsx, github: "talentdeficit/jsx" }]
  end

  defp deps(_) do
    []
  end

  defp package do
    [ contributors: ["Yuki Ito"],
      licenses: ["MIT"],
      links: %{ "GitHub" => "https://github.com/mururu/msgpack-elixir"} ]
  end
end
