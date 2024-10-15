
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
        $(map(p -> quote
        if !(@isdefined $p)
            const $p = $(string(p) |> Atom)
        end
    end, ps)...)
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

Return an expression after automatically instantiating undefined [`Atom`](@ref)s.

!!! info
All atoms are parsed as `Atom{String}` objects.

!!! warning
The Julia parser can parse some (infix) `NamedConnective`s such as ∧, →, but
has a few limitations, including:
- inexact precedence and associativity for some operators (e.g.,
    in fact, as of Julia 1.9, despite `(@synexpr ¬ p ∧ q) == @synexpr ¬(p) ∧ q`,
    Base.operator_precedence(:(¬)) < Base.operator_precedence(:(∧)));
- inability to parse most multi-character, custom-made `AbstractConnective`s (e.g., ⟨=⟩, [G]);
For a more flexible parsing, consider using `parseformula`.

# Examples
```julia-repl
julia> @synexpr x = p # Atom{String}("p") is assigned to the global variable x
Atom{String}("p")

julia> @synexpr st = p ∧ q → r
(p ∧ q) → r

julia> token(st)
→
```

See also [`parseformula`](@ref), [`Atom`](@ref), [`AbstractConnective`](@ref),
[`precedence`](@ref), [`associativity`](@ref).
"""
macro synexpr(expression)
    quote
        $(expression |> atomize)
    end |> esc
end
