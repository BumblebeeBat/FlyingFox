defmodule Flyingfox.Mixfile do
  use Mix.Project

  @repo "http://github.com/BumblebeeBat/FlyingFox"
  @issues @repo <> "/issues"

  def project do
    [app: :flyingfox,
     name: "Flying Fox",
     source_url: @repo,
     homepage_url: @repo,
     version: "0.0.1",
     elixir: "~> 1.0",
     #escript: escript_config,
     deps: deps,
     description: "Proof of stake blockchain",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     package: package]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:logger, :crypto, :cowboy],
     mod: {FlyingFox, []},
     registered: [:flyingfox]]
  end

  defp package do
    [files: ~W(lib mix.exs README.md test),
    author: "Zack Hess",
    contributors: ["Ricardo Lanziano"],
    licenses: ["Unlicence"],
    links: %{"GitHub" => @repo,
    	     "GitHub issues" => @issues}]
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
		#we need to add the sha256 hash of the push, that way we wont get tricked into downloading bad code. 
    [
				{:cowboy, "~> 1.0.0"},
				{:jiffy, github: "davisp/jiffy"},
				{:exleveldb, "~> 0.5"},
				{:eleveldb, github: "basho/eleveldb", tag: "2.1.0"}
		]
  end
end
