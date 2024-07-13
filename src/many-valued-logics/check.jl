# Meet (greatest lower bound) between values α and β
function collatetruth(
    ::typeof(∧),
    (α, β)::NTuple{N, T where T<:FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.meet(α, β)
end

# Join (least upper bound) between values α and β
function collatetruth(
    ::typeof(∨),
    (α, β)::NTuple{N, T where T<:FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.join(α, β)
end

# Implication/pseudo-complement α → β = join(γ | meet(α, γ) ⪯ β)
function collatetruth(
    ::typeof(→),
    (α, β)::NTuple{N, T where T<:FiniteTruth},
    a::FiniteFLewAlgebra
) where {
    N
}
    a.implication(α, β)
end

function collatetruth(
    c::Connective,
    (α, β)::Tuple{FiniteTruth, BooleanTruth},
    a::FiniteFLewAlgebra
)
    return collatetruth(c, (α, convert(FiniteTruth, β)), a)
end

function collatetruth(
    c::Connective,
    (α, β)::Tuple{BooleanTruth, FiniteTruth},
    a::FiniteFLewAlgebra
)
    return collatetruth(c, (convert(FiniteTruth, α), β), a)
end

function collatetruth(
    c::Connective,
    (α, β)::Tuple{BooleanTruth, BooleanTruth},
    a::FiniteFLewAlgebra
)
    return collatetruth(c, (convert(FiniteTruth, α), convert(FiniteTruth, β)), a)
end

function simplify(
    c::Connective,
    (α, β)::Tuple{FiniteTruth,FiniteTruth},
    a::FiniteFLewAlgebra
)
    return collatetruth(c, (α, β), a)
end

    function simplify(
    c::Connective,
    (α, β)::Tuple{FiniteTruth,BooleanTruth},
    a::FiniteFLewAlgebra
)
    return collatetruth(c, (α, convert(FiniteTruth, β)), a)
end

    function simplify(
    c::Connective,
    (α, β)::Tuple{BooleanTruth,FiniteTruth},
    a::FiniteFLewAlgebra
)
    return collatetruth(c, (convert(FiniteTruth, α), β), a)
end

    function simplify(
    c::Connective,
    (α, β)::Tuple{BooleanTruth,BooleanTruth},
    a::FiniteFLewAlgebra
)
    return collatetruth(c, (convert(FiniteTruth, α), convert(FiniteTruth, β)), a)
end

function collatetruth(::typeof(¬), (α,)::Tuple{FiniteTruth}, a::FiniteFLewAlgebra)
    if isboolean(a)
        if istop(α)
            return ⊥
        else
            return ⊤
        end
    else
        return error("¬ operation isn't defined outside of BooleanAlgebra")
    end
end

function collatetruth(c::Connective, (α,)::Tuple{BooleanTruth}, a::FiniteFLewAlgebra)
    return collatetruth(c, convert(FiniteTruth, α), a)
end

function simplify(c::Connective, (α,)::Tuple{FiniteTruth}, a::FiniteFLewAlgebra)
    return collatetruth(c, (α,), a)
end

function simplify(c::Connective, (α,)::Tuple{BooleanTruth}, a::FiniteFLewAlgebra)
    return simplify(c, convert(FiniteTruth, α), a)
end
