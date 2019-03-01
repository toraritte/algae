defmodule Algae.Prim do
  def integer(int) when is_integer(int), do: int
  def integer(_), do: raise(ArgumentError, "not integer")

  def string(str) when is_binary(str), do: str
  def string(_), do: raise(ArgumentError, "not string")

  def boolean(bool) when is_boolean(bool), do: bool
  def boolean(_), do: raise(ArgumentError, "not boolean")

  def list(list) when is_list(list), do: list
  def list(_), do: raise(ArgumentError, "not list")
end
