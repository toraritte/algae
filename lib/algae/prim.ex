defmodule Algae.Prim do
  def integer(int) when is_integer(int), do: int
  def integer(_), do: raise(ArgumentError, "not integer")

  # 2019-03-06 NOTE `Prim.string/1` == `Prim.binary/1`
  # ==================================================
  # For now.  `Ecto.Changeset` has nice  validations for
  # strings,  probably should  re-use  those, either  by
  # pulling them in this project, or re-using parts.

  def string(str) when is_binary(str), do: str
  def string(_), do: raise(ArgumentError, "not string")

  def boolean(bool) when is_boolean(bool), do: bool
  def boolean(_), do: raise(ArgumentError, "not boolean")

  def float(float) when is_float(float), do: float
  def float(_), do: raise(ArgumentError, "not float")

  def binary(bin) when is_binary(bin), do: bin
  def binary(_), do: raise(ArgumentError, "not binary")
end

