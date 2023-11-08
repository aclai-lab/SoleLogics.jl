
"""
    @atoms(ps...)

Instantiate a collection of [`Atom`](@ref)s and return them as a vector.

!!! info
    Atoms instantiated with this macro are defined in the global scope as constants.

# Examples
```julia-repl
julia> SoleLogics.@atoms String p q r s
4-element Vector{Atom{String}}:
 Atom{String}("p")
 Atom{String}("q")
 Atom{String}("r")
 Atom{String}("s")

julia> p
Atom{String}("p")
```
"""
macro atoms(ps...)
    quote
        $(map(p -> :(const $p = $(string(p) |> Atom)), ps)...)
        [$(ps...)]
    end |> esc
end

# Source:
#   Symbolics.jl  (https://github.com/JuliaSymbolics/Symbolics.jl)
#   PAndQ.jl      (https://github.com/jakobjpeters/PAndQ.jl)
atomize(p::Symbol) = :((@isdefined $p) ? $p : $(string(p) |> Atom))
atomize(x) = x
atomize(x::Expr) = Meta.isexpr(x, [:(=), :kw]) ?
    Expr(x.head, x.args[1], map(atomize, x.args[2:end])...) :
    Expr(x.head, map(atomize, x.args)...)

"""
    @synexpr(expression)

Return an expression after automatically instantiating undefined [`Atom`]s.

!!! info
Every identified atom is of type `Atom{String}`.

!!! warning
To perform parsing, Base.operator_precedence and Base.operator_associativity are considered
for each `Connective`-type token, instead of SoleLogics.precedence and SoleLogics.associativity.
This behaviour might interfere with custom made `Connective`s.
To know more, see `Connective` documentation.

# Examples
```julia-repl
julia> @synexpr x = p # Atom{String}("p") is assigned to the global variable x
Atom{String}("p")

julia> @synexpr st = p ∧ q → r
(p ∧ q) → r

julia> typeof(st)
SyntaxBranch{SoleLogics.NamedConnective{:→}}
```

See also [`Atom`](@ref), [`Connective`](@ref), [`precedence`](@ref), [`associativity`](@ref).
"""
macro synexpr(expression)
    quote
        $(expression |> atomize)
    end |> esc
end
