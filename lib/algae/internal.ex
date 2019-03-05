defmodule Algae.Internal do
  @moduledoc false

  @type ast() :: {atom(), any(), any()}

  @doc """
  Construct a data type AST
  """
  @spec data_ast(module(), Macro.Env.t() | [module()], ast()) :: ast()
  def data_ast(lines, %{aliases: _} = caller) when is_list(lines) do
    {field_values, field_types, specs, args, defaults} = module_elements(lines, caller)

    quote do
      @type t :: %__MODULE__{unquote_splicing(field_types)}
      defstruct unquote(field_values)

      @doc "Positional constructor, with args in the same order as they were defined in"
      @spec new(unquote_splicing(specs)) :: t()
      def new(unquote_splicing(args)) do
        struct(__MODULE__, unquote(defaults))
      end

      defoverridable [new: unquote(Enum.count(args))]
    end
  end

  # x as in experimental. (No clue why I haven't done this in previous commits...)
  def data_astx(lines, %{aliases: _} = caller, opts) when is_list(lines) do
    {field_values, field_types, specs, args, defaults} = module_elements(lines, caller)

    IO.puts("data_astx's field_types:")
    IO.inspect(field_types)
    IO.puts("---")

    IO.puts("data_astx's defaults:")
    IO.inspect(defaults)
    IO.puts("---")

    new_override_list = Enum.map(0..Enum.count(args), &({:new, &1}))
    # newp/* should be private anyway
    newp_override_list = Enum.map(1..Enum.count(args), &({:newp, &1}))
      # More verbose, but clearer.
      # for arity <- 0..Enum.count(args) do
      #   {:new, arity}
      # end
    IO.puts("data_astx's newp_override_list:")
    IO.inspect(newp_override_list)
    IO.puts("---")

    args_without_defaults =
      Enum.map(args, fn({:\\, [], [stripped, _]}) -> stripped end)
    IO.puts("args_without_defaults:")
    IO.inspect(args_without_defaults)
    IO.puts("---")

    scanned_args_without_defaults =
      Enum.scan(args_without_defaults, [], &(&2 ++ [&1]))

    arity = length(args_without_defaults)

    {fields, types} =
      List.foldr(
        field_types,
        {[],[]},
        fn
          ({field, {type, _ctx, nil}}, {fs, ts}) when is_atom(type) ->
            {[field|fs],[{:prim, type}|ts]}
          ({field, {:__aliases__, _ctx, _mod_atom_list} = type}, {fs, ts}) ->
            {[field|ts],[{:algae, type}|ts]}
          # ({field_name, _} = field) ->
          #   IO.puts("types' field:")
          #   IO.inspect(field)
          #   IO.puts("---")
          #   field_name
        end
      )
    scanned_types =
      Enum.scan(types, [], &(&2 ++ [&1]))
    # import IEx; pry()

    quote do
      use Quark

      @type t :: %__MODULE__{unquote_splicing(field_types)}
      defstruct unquote(field_values)

      @doc "Positional constructor, with args in the same order as they were defined in"

      defpartialx newp(unquote_splicing(args_without_defaults)) do
        IO.puts("unquoted args_without_defaults:")
        IO.inspect(unquote(args_without_defaults))
        IO.puts("---")
        struct(__MODULE__, unquote(defaults))
      end

      # This trick allows overriding but won't allow partial type checking...
      # defpartialx new(unquote_splicing(args_without_defaults)) do
      #   newp(unquote_splicing(args_without_defaults))
      # end

      # track_partial new: &newp/unquote(arity)

      # def new(), do: fn(a)    -> apply(__MODULE__, :new, [a]) end
      # def new(a), do: fn(b)   -> apply(__MODULE__, :new, [a, b]) end
      # def new(a,b), do: fn(c) -> apply(__MODULE__, :new, [a, b, c]) end
      # def new(a, b, c), do: a - b - c
      #
      # defmodule A do
      #   def new(),    do: fn(a) -> Person.newp(a)   end
      #   def new(a),   do: fn(b) -> Person.newp(a,b) end
      #   def new(a,b), do: Person.newp(a,b)
      # end

      unquote do
        [[[]] ++ scanned_args_without_defaults, [[]] ++ scanned_types]
        |> Enum.zip()
        |> Enum.with_index(-arity)
        |> Enum.map(&track_newp/1)
        # Enum.map(
        #   [[]] ++ scanned_args_without_defaults,
        #   fn(a) ->
        #     case length(a) == arity do
        #       false ->
        #         quote do
        #           def new(unquote_splicing(a)) do
        #             fn(curried) ->
        #               apply(__MODULE__, :newp, unquote(a))
        #             end
        #           end
        #         end
        #       true ->
        #         quote do
        #           def new(unquote_splicing(a)) do
        #             apply(__MODULE__, :newp, unquote(a))
        #           end
        #         end
        #     end
        #   end)
      end

      def type(%module{} = data) do
        IO.puts("type's data:")
        IO.inspect(data)
        IO.puts("---")

        args =
          Enum.map(
            unquote(fields),
            &Map.get(data,&1)
          )

        IO.puts("type/1 args:")
        IO.inspect(args)
        IO.puts("---")

        apply(module, :newp, args)
      end


      # defoverridable unquote(newp_override_list)  ++ [type: 1]
      defoverridable unquote(newp_override_list) ++ unquote(new_override_list) ++ [type: 1]

      # override `new/*` data constructors with typechecked ones
      unquote do
        [scanned_args_without_defaults, scanned_types]
        # |> add_overrides_if_not_empty(opts)
        |> Enum.zip()
        |> Enum.map(&override_newp/1)
      end
      # override_data_constructors(scanned_args_without_defaults, scanned_types)
    end
  end

  # def new(), do: fn(a)    -> apply(__MODULE__, :new, [a]) end
  # def new(a), do: fn(b)   -> apply(__MODULE__, :new, [a, b]) end
  # def new(a,b), do: fn(c) -> apply(__MODULE__, :new, [a, b, c]) end
  # def new(a, b, c), do: a - b - c
  #
  # defmodule A do
  #   def new(),    do: fn(a) -> Person.newp(a)   end
  #   def new(a),   do: fn(b) -> Person.newp(a,b) end
  #   def new(a,b), do: Person.newp(a,b)
  # end
  #
  # # [new: {:/, [line: 91], [{:a_local, [line: 91], nil}, 2]}]
  #   defmacro track_partial([{tracking_name, {:/, _, [{tracked_fun, _, nil}, tracked_arity]}}]) do
  #   end

  defmacro track_partial(ast), do: IO.inspect(ast)
# [ new: {:&, [line: 91], [ {:/, [line: 91], [ {{:., [line: 91], [{:__aliases__, [line: 91], [:Quark, :Partial]}, :defpartial]}, [line: 91], []}, 2 ]} ]} ]
  # defmacro track_partial([{tracking_name, {:&, _, [{:/, _, [{{:., _, [{:__aliases__, _, tracked_mod}, tracked_fun]}, _, []}, tracked_arity]}]} = tracked}]) do

# [new: {:&, [line: 3], [{:/, [context: Algae.Internal, import: Kernel, line: 3], [{:newp, [line: 3], Algae.Internal}, 2]}]}]

# [new: {:&, [line: 3], [{:/, [context: Algae.Internal, import: Kernel, line: 3], [{{:., [line: 3], [{:__MODULE__, [line: 3], Algae.Internal}, :newp]}, [line: 3], []}, 2]}]} ]
# defmacro track_partial([{tracking_name, {:&, [line: 3], [{:/, [context: Algae.Internal, import: Kernel, line: 3], [{{:., [line: 3], [{:__MODULE__, [line: 3], Algae.Internal}, :newp]}, [line: 3], []}, 2]}]} }]) do
#     args = Macro.generate_arguments(tracked_arity, __MODULE__)
#     scanned_args = [[]] ++ Enum.scan(args, [], &(&2 ++ [&1]))

#     quote do
#       unquote do: make_curried_clauses(scanned_args, tracking_name, tracked)
#     end
#   end

#   defp make_curried_clauses([args], tracking_name, tracked) do
#     quote do
#       def unquote({tracking_name, [], args}) do
#         unquote(tracked).(unquote_splicing(args))
#       end
#     end
#   end

  # defp make_curried_clauses([args|rest], {fun_name, ctx, _} = fun_attrs) do
  defp make_curried_clauses([args|rest], tracking_name, tracked) do
    curried = Macro.generate_arguments(1, __MODULE__)
    quote do
      def unquote({tracking_name, [], args}) do
        # fn(unquote(curried)) -> apply(__MODULE__, unquote(fun_name), unquote(args) ++ [curried]) end
        fn(unquote(curried)) -> unquote(tracked).(unquote_splicing(args ++ [curried])) end
      end
      unquote do: make_curried_clauses(rest, tracking_name, tracked)
    end
  end
  # defp add_overrides_if_not_empty(list, []) do
  #   list
  # end
  # defp add_overrides_if_not_empty([l, _] = list, [overrides: news]) do
  #   IO.inspect(length(l))
  #   IO.inspect(news)

  #   overrides =
  #     Enum.map(
  #       1..length(l),
  #       fn(arity) ->
  #         case news[:"new/#{arity}"] do
  #           nil -> :noop
  #           fun -> fun
  #         end
  #       end
  #     )

  #   IO.inspect(overrides)

  #   list ++ [overrides]
  # end

  # length(types) == length(args) or something is messed up during client-side declaration
  # `override` is the overriding fun for the `new` constructor with a specific arity (eg., new/2)
  # defp override_newp({args, types}) do
  #   override_newp({args, types, :noop})
  # end
  # defp override_newp({args, types, override}) do

  # Why does this work?
  # -------------------
  # Had  to spent  half an  hour figuring  it out,  even
  # though I wrote this only 2 days ago...
  #
  # `defpartialx` works along the example below, and the
  # it is marked where `do_typecheck` is inserted:
  #
  #                 do_typecheck
  #                     V
  #   def new(),        | do: fn(a) -> apply(__MODULE__, :new, [a]      ) end
  #   def new(a),       | do: fn(b) -> apply(__MODULE__, :new, [a, b]   ) end
  #   def new(a,b),     | do: fn(c) -> apply(__MODULE__, :new, [a, b, c]) end
  #   def new(a, b, c), | do: a - b - c
  #                     ^
  #
  # All `newp`  variants succeed  in doing  type checks.
  # Taking the `Person` example in "scratch":
  #
  #     (It may be obvious why  this works, but I'm not that
  #     smart, and have to write it down.)
  #
  #     `Person.newp.(27)`:
  #     > `newp/0` returns an `fn/1`  that calls `newp/1`, and
  #     > then the inserted type check.
  #
  #     `Person.newp(27)`:
  #     > Calls `newp/1` with the inserted type check directly.
  #
  #     `Person.newp.("lofa").(:a)`:
  #     `Person.newp("lofa").(:a)`:
  #     `Person.newp("lofa", :a)`:
  #     > Maybe there is no need to expand on these after all.
  #
  #     (`Person.newp.("lofa",27)` not possible yet.)

  defp track_newp({{[], []}, _}) do
    quote do
      def new() do
        fn(curried) -> apply(__MODULE__, :new, [curried]) end
      end
    end
  end

  defp track_newp({{args, types}, 0}) do
    quote do
      def new(unquote_splicing(args)) do
        # no need to typecheck because newp/(last_arity) has already does it
        apply(__MODULE__, :newp, unquote(args))
      end
    end
  end

  defp track_newp({{args, types}, n}) do
    IO.puts("track_newp's n: #{n}")
    quote do
      def new(unquote_splicing(args)) do
        unquote do: do_typecheck(types, args)
        fn(curried) ->
          apply(__MODULE__, :newp, unquote(args) ++ [curried])
        end
      end
    end
  end

  defp override_newp({args, types}) do
    IO.inspect("override_newp's args: ")
    # IO.inspect({args, types, override})
    IO.inspect({args, types})

    # new_args =
    #   case override do
    #     :noop ->
    #       args
    #     fun ->
    #         fun.(args)
    #   end

    # IO.inspect("override_newp's new_args: ")
    # IO.inspect(new_args)

    quote do

      def newp(unquote_splicing(args)) do
        # IO.inspect("new/*'s override: ")
        # IO.inspect(unquote(override))
        IO.inspect("new/*'s arguments: ")
        IO.inspect(unquote(args))
        # IO.inspect("new_args unquoted: ")
        # IO.inspect(unquote(new_args))

        # new_args =
        #   case unquote(override) do
        #     :noop ->
        #       (fn(unquote_splicing(args)) -> args end).(unquote_splicing(args))
        #       # (fn(unquote_splicing(args)) ->
        #       #   raise(UndefinedFunctionError, "constructor redefined")
        #       # end).(unquote_splicing(args))
        #     fun ->
        #       fun.(unquote_splicing(args))
        #   end

        # IO.inspect("new_args: ")
        # IO.inspect(new_args)

        unquote do: do_typecheck(types, args)
        super(unquote_splicing(args))
      end
    end
  end

  defp do_typecheck(type_functions, functions_args) do
    IO.puts("do_typecheck's type_functions before quote:")
    IO.inspect(type_functions)
    IO.puts("---")
    quote do
      IO.puts("do_typecheck's type_functions after quote:")
      IO.inspect(unquote(type_functions))
      IO.puts("---")
      for {type, arg} <- Enum.zip(unquote(type_functions), unquote(functions_args)) do
        # can't put Prim.integer, etc., for now because something `module_elements` parser doesn't like it
        # apply(Algae.Prim, type, [arg])
        # Algae.Typechecker.check(type, arg)
        case type do
          {:prim, type} ->
            apply(Algae.Prim, type, [arg])
          {:algae, module} ->
            module.type(arg)
        end
      end
    end
  end

  def data_ast(modules, {:none, _, _}) do
    full_module = modules |> List.wrap() |> Module.concat()

    quote do
      defmodule unquote(full_module) do
        @type t :: %__MODULE__{}

        defstruct []

        @doc "Default #{__MODULE__} struct"
        @spec new() :: t()
        def new, do: struct(__MODULE__)

        defoverridable [new: 0]
      end
    end
  end

  def data_ast(caller_module, type) do
    default = default_value(type)
    field = module_to_field(caller_module)

    quote do
      @type t :: %unquote(caller_module){
        unquote(field) => unquote(type)
      }

      defstruct [{unquote(field), unquote(default)}]

      @doc "Default #{__MODULE__} struct"
      @spec new() :: t()
      def new, do: struct(__MODULE__)

      @doc "Constructor helper for piping"
      @spec new(unquote(type)) :: t()
      def new(field), do: struct(__MODULE__, [unquote(field), field])

      defoverridable [new: 0, new: 1]
    end
  end

  @spec data_ast([module()], any(), ast()) :: ast()
  def data_ast(name, default, type_ctx) do
    full_module = Module.concat(name)
    field = module_to_field(name)

    quote do
      defmodule unquote(full_module) do
        @type t :: %unquote(full_module){
          unquote(field) => unquote(type_ctx)
        }

        defstruct [{unquote(field), unquote(default)}]

        @doc "Default #{__MODULE__} struct. Value defaults to #{inspect unquote(default)}."
        @spec new() :: t()
        def new, do: struct(__MODULE__)

        @doc "Helper for initializing struct with a specific value"
        @spec new(unquote(type_ctx)) :: t()
        def new(value), do: struct(__MODULE__, [{unquote(field), value}])
      end
    end
  end

  @spec embedded_data_ast() :: ast()
  def embedded_data_ast do
    quote do
      @type t :: %__MODULE__{}
      defstruct []

      @doc "Default #{__MODULE__} struct"
      @spec new() :: t()
      def new, do: struct(__MODULE__)
    end
  end

  def embedded_data_ast(module_ctx, default, type_ctx) do
    field = module_to_field(module_ctx)
    quote do
      @type t :: %__MODULE__{
        unquote(field) => unquote(type_ctx)
      }

      defstruct [{unquote(field), unquote(default)}]

      @doc "Default #{__MODULE__} struct"
      @spec new(unquote(type_ctx)) :: t()
      def new(field \\ unquote(default)), do: struct(__MODULE__, [field])

      defoverridable [new: 1]
    end
  end

  @type field :: {atom(), [any()], [any()]}
  @type type  :: {atom(), [any()], [any()]}

  @spec module_elements([ast()], Macro.Env.t())
     :: {
          [{field(), any()}],
          [{field(), type()}],
          [type],
          [{:\\, [], any()}],
          [{field(), any()}]
        }
  def module_elements(lines, caller) do
        # import IEx; pry
    List.foldr(lines, {[], [], [], [], []},
      fn(line, {value_acc, type_acc, typespec_acc, acc_arg, acc_mapping}) ->
        {field, type, default_value} = normalize_elements(line, caller)
        # import IEx; pry
        arg = {field, [], Elixir}

        {
          [{field, default_value} | value_acc],
          [{field, type} | type_acc],
          [type | typespec_acc],
          [{:\\, [], [arg, default_value]} | acc_arg],
          [{field, arg} | acc_mapping]
        }
      end)
  end

  @spec normalize_elements(ast(), Macro.Env.t()) :: {atom(), type(), any()}
  # {:::, [line: 6], [{:minute, [line: 6], nil}, {:__aliases__, [line: 6], [:Minute]}]}
  # {:::, [line: 8], [{:minute, [line: 8], nil}, {:__aliases__, [line: 8], [:Clock, :Minute]}]}
  # {:::, _,         [{field,    _,          _}, type                                        ]}, caller do
  def normalize_elements({:::, _, [{field, _, _}, type]}, caller) do
    expanded_type = resolve_alias(type, caller)
    {field, expanded_type, default_value(expanded_type)}
    # expanded_type ==
    # {:__aliases__, [line: 6], [:Minute]}
    # {:__aliases__, [line: 8], [:Clock, :Minute]}
  end

  def normalize_elements({:\\, _, [{:::, _, [{field, _, _}, type]}, default]}, _) do
    {field, type, default}
  end

  @spec resolve_alias(ast(), Macro.Env.t()) :: ast()
  def resolve_alias({{_, _, _} = a, b, c}, caller) do
        # import IEx; pry
    {resolve_alias(a, caller), b, c}
  end

  def resolve_alias({:. = a, b, [{:__aliases__, _, _} = the_alias | rest]}, caller) do
        # import IEx; pry
    resolved_alias = Macro.expand(the_alias, caller)
    {a, b, [resolved_alias | rest]}
  end

  # TODO the above resolve clauses never seem to match
  # {:__aliases__, [line: 6], [:Minute]}
  # {:__aliases__, [line: 8], [:Clock, :Minute]}
  # def resolve_alias({:__aliases__, _ctx, _mod_atom_list} = the_alias, caller) do
  #   Macro.expand_once(the_alias, caller)
  # end

  def resolve_alias(a, caller) do
      # import IEx; pry
    a
  end

  @spec or_types([ast()], module()) :: [ast()]
  def or_types({:\\, _, [{:::, _, [_, types]}, _]}, module_ctx) do
    or_types(types, module_ctx)
  end

  def or_types([head | tail], module_ctx) do
    Enum.reduce(tail, call_type(head, module_ctx), fn(module, acc) ->
      {:|, [], [call_type(module, module_ctx), acc]}
    end)
  end

  @spec modules(module(), [module()]) :: [module()]
  def modules(top, module_ctx), do: [top | extract_name(module_ctx)]

  @spec call_type(module(), [module()]) :: ast()
  def call_type(new_module, module_ctx) do
    full_module = List.wrap(module_ctx) ++ submodule_name(new_module)
    {{:., [], [{:__aliases__, [alias: false], full_module}, :t]}, [], []}
  end

  @spec submodule_name({:defdata, any(), [{:::, any(), [any()]}]})
     :: [module()]
  def submodule_name({:defdata, _, [{:::, _, [body, _]}]}) do
    body
    |> case do
      {:\\, _, [inner_module_ctx, _]} -> inner_module_ctx
      {:__aliases__, _, module} -> module
      outer_module_ctx -> outer_module_ctx
    end
    |> List.wrap()
  end

  def submodule_name({:defdata, _, [{:\\, _, [{:::, _, [{:__aliases__, _, module}, _]}, _]}]}) do
    List.wrap(module)
  end

  def submodule_name({:defdata, _, [{:__aliases__, _, module}, _]}) do
    List.wrap(module)
  end

  @spec extract_name({any(), any(), atom()} | [module()]) :: [module()]
  def extract_name({_, _, inner_name}), do: List.wrap(inner_name)
  def extract_name(module_chain) when is_list(module_chain), do: module_chain

  def module_to_field(modules) when is_list(modules) do
    modules
    |> List.last()
    |> module_to_field()
  end

  def module_to_field(module) do
    module
    |> Atom.to_string()
    |> String.split(".")
    |> List.last()
    |> String.downcase()
    |> String.trim_leading("elixir.")
    |> String.to_atom()
  end

  # credo:disable-for-lines:21 Credo.Check.Refactor.CyclomaticComplexity
  def default_value({{:., _, [{_, _, [:String]}, :t]}, _, _}), do: ""
  def default_value({{:., _, [String, :t]}, _, _}), do: ""

  def default_value({{:., _, [{_, _, adt}, :t]}, _, []}) do
    quote do: unquote(Module.concat(adt)).new()
  end

  def default_value({{:., _, [module, :t]}, _, []}) do
    quote do: unquote(module).new()
  end

  def default_value([_]), do: []

  # {:__aliases__, [line: 6], [:Minute]}
  # {:__aliases__, [line: 8], [:Clock, :Minute]}
  def default_value({type, _, _}) do
    type
    |> case do
      :boolean -> false

      :number  -> 0
      :integer -> 0

      :float -> 0.0

      :pos_integer     -> 1
      :non_neg_integer -> 0

      :bitstring -> ""
      :charlist  -> []

      []    -> []
      :list -> []

      :map  -> %{}

      :fun -> &Quark.id/1
      :->  -> &Quark.id/1

      :any -> nil
      :t   -> raise %Algae.Internal.NeedsExplicitDefaultError{message: "Type is lone `t`"}

      atom -> atom
    end
    |> Macro.escape()
  end

  # `default_value` not needed for the type checker version
  def default_value(_), do: :noop
end
