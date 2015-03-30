defmodule BlockMaker do
  def doit do
    BlockAbsorber.serve
    IO.puts("here")
  end
  def test do
    doit
  end
end
