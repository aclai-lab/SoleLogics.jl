function SoleLogics.collatetruth(
    c::NamedConnective,
    (α, β)::Tuple{FiniteTruth,T},
    a::FiniteFLewAlgebra
) where {
    T <: Truth
}
    if !isa(β,FiniteTruth) β = convert(FiniteTruth, β) end
    return SoleLogics.collatetruth(c, (α, β), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective,
    (x, y)::Tuple{ContinuousTruth, T},
    a::FuzzyLogic
) where {
    T <: Truth
}
    if !isa(y, ContinuousTruth) y = convert(ContinuousTruth, y) end
    return SoleLogics.collatetruth(c, (x, y), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective,
    (x, y)::Tuple{ContinuousTruth, T},
    a::ManyExpertAlgebra
) where {
    T <: Truth,
}
    if !isa(y, ContinuousTruth) y = convert(ContinuousTruth, y) end 
    return SoleLogics.collatetruth(c, (x, y), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective,
    (α, β)::Tuple{T,FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    T <: Truth
}
    if !isa(α,FiniteTruth) α = convert(FiniteTruth, α) end
    return SoleLogics.collatetruth(c, (α, β), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective,
    (x, y)::Tuple{T, ContinuousTruth},
    a::FuzzyLogic
) where {
    T <: Truth
}
    if !isa(x, ContinuousTruth) x = convert(ContinuousTruth, x) end
    return SoleLogics.collatetruth(c, (x, y), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective,
    (x, y)::Tuple{T, ContinuousTruth},
    a::ManyExpertAlgebra
) where {
    T <: Truth
}
    if !isa(x, ContinuousTruth) x = convert(ContinuousTruth, x) end
    return SoleLogics.collatetruth(c, (x, y), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective,
    (x, y)::NTuple{N, ContinuousTruth},
    a::ManyExpertAlgebra
) where {
    N
}
    x = ntuple(_ -> ContinuousTruth(x.value), length(a.experts))
    y = ntuple(_ -> ContinuousTruth(y.value), length(a.experts))
    return SoleLogics.collatetruth(c, (x, y), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective, 
    (x, y)::Tuple{ContinuousTruth, NTuple{M, ContinuousTruth}},
    a::ManyExpertAlgebra
) where {
    M
}
    x = ntuple(_ -> ContinuousTruth(x.value), M)
    return SoleLogics.collatetruth(c, (x, y), a)
end

function SoleLogics.collatetruth(
    c::NamedConnective, 
    (x, y)::Tuple{NTuple{M, ContinuousTruth}, ContinuousTruth},
    a::ManyExpertAlgebra
) where {
    M
}
    y = ntuple(_ -> ContinuousTruth(y.value), M)
    return SoleLogics.collatetruth(c, (x, y), a)
end

function SoleLogics.collatetruth(
    ::typeof(∧),
    (α, β)::NTuple{N, FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.monoid(α, β)
end

function SoleLogics.collatetruth(
    ::typeof(∧),
    (x, y)::NTuple{N, ContinuousTruth},
    a::FuzzyLogic
) where {
    N
}
    a.tnorm(x, y)
end

function SoleLogics.collatetruth(
    ::typeof(∧),
    (x, y)::NTuple{N, NTuple{M, ContinuousTruth}},
    a::ManyExpertAlgebra
) where {
    N, M
}
    ntuple(i -> SoleLogics.collatetruth(∧,(x[i], y[i]), a.experts[i]), M)
end

function SoleLogics.collatetruth(
    ::typeof(∨),
    (α, β)::NTuple{N, FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.join(α, β)
end

function SoleLogics.collatetruth(
    ::typeof(∨),
    (x, y)::NTuple{N, ContinuousTruth},
    a::FuzzyLogic
) where {
    N
}
    ContinuousTruth(max(x.value, y.value))
end

function SoleLogics.collatetruth(
    ::typeof(∨), 
    (x, y)::NTuple{N, NTuple{M, ContinuousTruth}}, 
    a::ManyExpertAlgebra
) where {
    N, M
}
    ntuple(i -> SoleLogics.collatetruth(∨,(x[i], y[i]), a.experts[i]), M)
end

function SoleLogics.collatetruth(
    ::typeof(→),
    (α, β)::NTuple{N, FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.implication(α, β)
end

function SoleLogics.collatetruth(
    ::typeof(→),
    (x, y)::NTuple{N, ContinuousTruth}, 
    a::FuzzyLogic
) where {
    N
}
    
    error("Implication not implemented for FuzzyLogic with t-norm $(a.tnorm). " *
          "Please provide a specific implementation by defining a method for " *
          "SoleLogics.collatetruth(::typeof(→), ::NTuple{N, ContinuousTruth}, ::$(typeof(a)))")
end

function SoleLogics.collatetruth(
    ::typeof(→),
    (x, y)::NTuple{N, ContinuousTruth}, 
    ::typeof(GodelLogic)
) where {
    N
}
    x.value <= y.value ? ContinuousTruth(1.0) : y
end

function SoleLogics.collatetruth(
    ::typeof(→),
    (x, y)::NTuple{N, ContinuousTruth},
    ::typeof(LukasiewiczLogic)
) where {
    N
} 
    x.value <= y.value ? ContinuousTruth(1.0) : ContinuousTruth(1 - x.value + y.value)
end


function SoleLogics.collatetruth(
    ::typeof(→),
    (x, y)::NTuple{N, ContinuousTruth}, 
    ::typeof(ProductLogic)
) where {
    N
}
    x.value <= y.value ? ContinuousTruth(1.0) : ContinuousTruth(y.value / x.value)
end

function SoleLogics.collatetruth(
    ::typeof(→),
    (x, y)::NTuple{N, NTuple{M, ContinuousTruth}}, 
    a::ManyExpertAlgebra
) where {
    N, M
} 
    ntuple(i -> SoleLogics.collatetruth(→, (x[i], y[i]), a.experts[i]), M)
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

function SoleLogics.simplify(::typeof(∧), φs::Tuple{FiniteTruth,BooleanTruth}, args...; kwargs...)
    SoleLogics.simplify(∧, (φs[1], convert(FiniteTruth, φs[2])), args...; kwargs...)
end

function SoleLogics.simplify(::typeof(∧), φs::Tuple{BooleanTruth,FiniteTruth}, args...; kwargs...)
    SoleLogics.simplify(∧, (convert(FiniteTruth, φs[1]), φs[2]), args...; kwargs...)
end

# Simplify overrides for many-expert algebras

function SoleLogics.simplify(::typeof(∧), (t1, t2)::Tuple{BooleanTruth, BooleanTruth}, a::ManyExpertAlgebra, args...; kwargs...)
    istop(t1) && istop(t2) ? top(a) : bot(a)
end

function SoleLogics.simplify(::typeof(∧), (t1, t2)::Tuple{BooleanTruth, Formula}, a::ManyExpertAlgebra, args...; kwargs...)
    istop(t1) ? t2 : bot(a)
end

function SoleLogics.simplify(::typeof(∧), (t1, t2)::Tuple{Formula, BooleanTruth}, a::ManyExpertAlgebra, args...; kwargs...)
    istop(t2) ? t1 : bot(a)
end

function SoleLogics.simplify(::typeof(∧), φs::Tuple{NTuple{M, ContinuousTruth}, BooleanTruth}, args...; kwargs...) where {M}
    SoleLogics.simplify(∧, (φs[1], ntuple(_ -> convert(ContinuousTruth, φs[2]), M)), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(∧), φs::Tuple{BooleanTruth, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.simplify(∧, (ntuple(_ -> convert(ContinuousTruth, φs[1]), M), φs[2]), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(∧), φs::Tuple{NTuple{M, ContinuousTruth}, ContinuousTruth}, args...; kwargs...) where {M}
    SoleLogics.simplify(∧, (φs[1], ntuple(_ -> ContinuousTruth(φs[2].value), M)), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(∧), φs::Tuple{ContinuousTruth, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.simplify(∧, (ntuple(_ -> ContinuousTruth(φs[1].value), M), φs[2]), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(∧), φs::Tuple{NTuple{M, ContinuousTruth}, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.collatetruth(∧, (φs[1], φs[2]), args...; kwargs...) 
end

function SoleLogics.simplify(::typeof(∨), (t1, t2)::Tuple{BooleanTruth, Formula}, a::ManyExpertAlgebra, args...; kwargs...)
    isbot(t1) ? t2 : top(a)
end

function SoleLogics.simplify(::typeof(∨), (t1, t2)::Tuple{Formula, BooleanTruth}, a::ManyExpertAlgebra, args...; kwargs...) 
    isbot(t2) ? t1 : top(a)
end

function SoleLogics.simplify(::typeof(∨), φs::Tuple{FiniteTruth,BooleanTruth}, args...; kwargs...)
    SoleLogics.simplify(∨, (φs[1], convert(FiniteTruth, φs[2])), args...; kwargs...)
end

function SoleLogics.simplify(::typeof(∨), φs::Tuple{BooleanTruth,FiniteTruth}, args...; kwargs...)
    SoleLogics.simplify(∨, (convert(FiniteTruth, φs[1]), φs[2]), args...; kwargs...)
end

function SoleLogics.simplify(::typeof(∨), (t1, t2)::Tuple{BooleanTruth, BooleanTruth}, a::ManyExpertAlgebra, args...; kwargs...)
    isbot(t1) && isbot(t2) ? bot(a) : top(a)
end

function SoleLogics.simplify(::typeof(∨), φs::Tuple{NTuple{M, ContinuousTruth}, BooleanTruth}, args...; kwargs...) where {M}
    SoleLogics.simplify(∨, (φs[1], ntuple(_ -> convert(ContinuousTruth, φs[2]), M)), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(∨), φs::Tuple{BooleanTruth, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.simplify(∨, (ntuple(_ -> convert(ContinuousTruth, φs[1]), M), φs[2]), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(∨), φs::Tuple{NTuple{M, ContinuousTruth}, ContinuousTruth}, args...; kwargs...) where {M}
    SoleLogics.simplify(∨, (φs[1], ntuple(_ -> ContinuousTruth(φs[2].value), M)), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(∨), φs::Tuple{ContinuousTruth, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.simplify(∨, (ntuple(_ -> ContinuousTruth(φs[1].value), M), φs[2]), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(∨), φs::Tuple{NTuple{M, ContinuousTruth}, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.collatetruth(∨, (φs[1], φs[2]), args...; kwargs...) 
end

function SoleLogics.simplify(::typeof(→), φs::Tuple{FiniteTruth,BooleanTruth}, args...; kwargs...)
    SoleLogics.simplify(→, (φs[1], convert(FiniteTruth, φs[2])), args...; kwargs...)
end

function SoleLogics.simplify(::typeof(→), φs::Tuple{BooleanTruth,FiniteTruth}, args...; kwargs...)
    SoleLogics.simplify(→, (convert(FiniteTruth, φs[1]), φs[2]), args...; kwargs...)
end

function SoleLogics.simplify(::typeof(→), (t1, t2)::Tuple{BooleanTruth, BooleanTruth}, a::ManyExpertAlgebra, args...; kwargs...)
    SoleLogics.simplify(→, (ntuple(_ -> convert(ContinuousTruth, t1), length(a.experts)), ntuple(_ -> convert(ContinuousTruth, t2), length(a.experts))), a, args...; kwargs...)
end

function SoleLogics.simplify(::typeof(→), φs::Tuple{NTuple{M, ContinuousTruth}, BooleanTruth}, args...; kwargs...) where {M}
    SoleLogics.simplify(→, (φs[1], ntuple(_ -> convert(ContinuousTruth, φs[2]), M)), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(→), φs::Tuple{BooleanTruth, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.simplify(→, (ntuple(_ -> convert(ContinuousTruth, φs[1]), M), φs[2]), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(→), φs::Tuple{NTuple{M, ContinuousTruth}, ContinuousTruth}, args...; kwargs...) where {M}
    SoleLogics.simplify(→, (φs[1], ntuple(_ -> ContinuousTruth(φs[2].value), M)), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(→), φs::Tuple{ContinuousTruth, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.simplify(→, (ntuple(_ -> ContinuousTruth(φs[1].value), M), φs[2]), args...; kwargs...)    
end

function SoleLogics.simplify(::typeof(→), φs::Tuple{NTuple{M, ContinuousTruth}, NTuple{M, ContinuousTruth}}, args...; kwargs...) where {M}
    SoleLogics.collatetruth(→, (φs[1], φs[2]), args...; kwargs...) 
end
