defmodule Algae.Prim do
  def integer(int) when is_integer(int), do: int
  def integer(_), do: raise(ArgumentError, "not integer")

  def string(str) when is_string(str), do: str
  def string(_), do: raise(ArgumentError, "not string")
end
