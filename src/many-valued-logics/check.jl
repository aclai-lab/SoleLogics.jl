function SoleLogics.collatetruth(
    c::NamedConnective,
    (α, β)::Tuple{FiniteTruth,T},
    a::FiniteFLewAlgebra
) where {
    T <: Truth
}
    if !isa(β,FiniteTruth) convert(FiniteTruth, β) end
    return SoleLogics.collatetruth(c, (α, β), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective,
    (α, β)::Tuple{T,FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    T <: Truth
}
    if !isa(α,FiniteTruth) convert(FiniteTruth, α) end
    return SoleLogics.collatetruth(c, (α, β), a)
end

# Meet (greatest lower bound) between values α and β
function SoleLogics.collatetruth(
    ::typeof(∧),
    (α, β)::NTuple{N, T where T<:FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.meet(α, β)
end

# Join (least upper bound) between values α and β
function SoleLogics.collatetruth(
    ::typeof(∨),
    (α, β)::NTuple{N, T where T<:FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.join(α, β)
end

# Implication/pseudo-complement α → β = join(γ | meet(α, γ) ⪯ β)
function SoleLogics.collatetruth(
    ::typeof(→),
    (α, β)::NTuple{N, T where T<:FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.implication(α, β)
end

"""
    alphacheck(
        α::FiniteTruth,
        φ::Formula,
        i::AbstractInterpretation,
        a::FiniteAlgebra
        args...;
        kwargs...
    )::Bool

Check a formula on a logical interpretation (or model), returning `true` if the truth value
for the formula is at least `α`` in the algebra `a`.

# Examples
```julia-repl
julia> @atoms String p q
2-element Vector{Atom{String}}:
 Atom{String}("p")
 Atom{String}("q")

julia> td = TruthDict([p => TOP, q => BOT])
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│      ⊥ │      ⊤ │
└────────┴────────┘

julia> check(CONJUNCTION(p,q), td)
false
```

See also [`check`](@ref), [`FiniteTruth`](@ref), [`Formula`](@ref),
[`AbstractInterpretation`](@ref), [`TruthDict`](@ref), [`FiniteAlgebra`](@ref).
"""
function alphacheck(
    α::Truth,
    φ::Formula,
    i::SoleLogics.AbstractInterpretation,
    a::FiniteAlgebra,
    args...;
    kwargs...
)::Bool
    if !isa(α, FiniteTruth) α = convert(FiniteTruth, α) end
    precedeq(a, α, interpret(φ, i, a, args...; kwargs...))
end