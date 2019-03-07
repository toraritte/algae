defmodule Algae.Prim do
  def integer(int) when is_integer(int), do: :integer
  def integer(_), do: raise(ArgumentError, "not integer")

  # 2019-03-06 NOTE `Prim.string/1` == `Prim.binary/1`
  # ==================================================
  # For now.  `Ecto.Changeset` has nice  validations for
  # strings,  probably should  re-use  those, either  by
  # pulling them in this project, or re-using parts.
  #
  # 2019-03-07
  # Making a `get_type/1` function  would help, but then
  # the clauses  would have  to do  their best  to catch
  # types.  Maybe a  "string" clause  could come  first,
  # trying its best to identify a string, and categorize
  # it as a binary in a later clause if all else fails.

  def string(str) when is_binary(str), do: :string
  def string(_), do: raise(ArgumentError, "not string")

  def boolean(bool) when is_boolean(bool), do: :boolean
  def boolean(_), do: raise(ArgumentError, "not boolean")

  def float(float) when is_float(float), do: :float
  def float(_), do: raise(ArgumentError, "not float")

  def binary(bin) when is_binary(bin), do: :binary
  def binary(_), do: raise(ArgumentError, "not binary")

  def list(lst) when is_list(lst), do: :list
  def list(_), do: raise(ArgumentError, "not list")

  def get_type(arg) do

    base_types =
      [ :integer,
        :binary,
        :boolean,
        :float,
        :list
      ]

    do_get_type(base_types, arg)
  end

  defp do_get_type([], _arg), do: raise(ArgumentError, "something went wrong")
  defp do_get_type([type|tail], arg) do
    try do
      apply(__MODULE__, type, [arg])
    rescue
      _ -> do_get_type(tail, arg)
    end
  end
end

