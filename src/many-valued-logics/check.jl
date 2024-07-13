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