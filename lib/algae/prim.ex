defmodule Algae.Prim do
  def integer(int) when is_integer(int), do: int
  def integer(_), do: raise(ArgumentError, "not integer")

  def string(str) when is_binary(str), do: str
  def string(_), do: raise(ArgumentError, "not string")

  def boolean(bool) when is_boolean(bool), do: bool
  def boolean(_), do: raise(ArgumentError, "not boolean")

  def float(float) when is_float(float), do: float
  def float(_), do: raise(ArgumentError, "not float")

  def binary(bin) when is_binary(bin), do: bin
  def binary(_), do: raise(ArgumentError, "not binary")
end

