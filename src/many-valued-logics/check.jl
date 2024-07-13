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

function SoleLogics.collatetruth(
    c::Connective,
    (α, β)::Tuple{FiniteTruth, BooleanTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(c, (α, convert(FiniteTruth, β)), a)
end

function SoleLogics.collatetruth(
    c::Connective,
    (α, β)::Tuple{BooleanTruth, FiniteTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(c, (convert(FiniteTruth, α), β), a)
end

function SoleLogics.collatetruth(
    c::Connective,
    (α, β)::Tuple{BooleanTruth, BooleanTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(c, (convert(FiniteTruth, α), convert(FiniteTruth, β)), a)
end

function SoleLogics.simplify(
    c::Connective,
    (α, β)::Tuple{FiniteTruth,FiniteTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(c, (α, β), a)
end

    function SoleLogics.simplify(
    c::Connective,
    (α, β)::Tuple{FiniteTruth,BooleanTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(c, (α, convert(FiniteTruth, β)), a)
end

    function SoleLogics.simplify(
    c::Connective,
    (α, β)::Tuple{BooleanTruth,FiniteTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(c, (convert(FiniteTruth, α), β), a)
end

function SoleLogics.simplify(
    c::Connective,
    (α, β)::Tuple{BooleanTruth,BooleanTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(c, (convert(FiniteTruth, α), convert(FiniteTruth, β)), a)
end

function SoleLogics.collatetruth(::typeof(¬), (α,)::Tuple{FiniteTruth}, a::FiniteFLewAlgebra)
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

function SoleLogics.collatetruth(c::Connective, (α,)::Tuple{BooleanTruth}, a::FiniteFLewAlgebra)
    return SoleLogics.collatetruth(c, convert(FiniteTruth, α), a)
end

function SoleLogics.simplify(c::Connective, (α,)::Tuple{FiniteTruth}, a::FiniteFLewAlgebra)
    return SoleLogics.collatetruth(c, (α,), a)
end

function SoleLogics.simplify(c::Connective, (α,)::Tuple{BooleanTruth}, a::FiniteFLewAlgebra)
    return SoleLogics.simplify(c, convert(FiniteTruth, α), a)
end

function SoleLogics.simplify(
    c::Connective,
    (α, β)::Tuple{FiniteTruth, BooleanTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.simplify(c, (α, convert(FiniteTruth, β)), a)
end

function SoleLogics.simplify(
    c::Connective,
    (α, β)::Tuple{BooleanTruth, FiniteTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.simplify(c, (convert(FiniteTruth, α), β), a)
end

function SoleLogics.simplify(
    c::Connective,
    (α, β)::Tuple{BooleanTruth, BooleanTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.simplify(c, (convert(FiniteTruth, α), convert(FiniteTruth, β)), a)
end

function SoleLogics.simplify(
    ::typeof(∧),
    (α, β)::Tuple{FiniteTruth,FiniteTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(∧, (α, β), a)
end

function SoleLogics.simplify(
    ::typeof(∨),
    (α, β)::Tuple{FiniteTruth,FiniteTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(∨, (α, β), a)
end

function SoleLogics.simplify(
    ::typeof(→),
    (α, β)::Tuple{FiniteTruth,FiniteTruth},
    a::FiniteFLewAlgebra
)
    return SoleLogics.collatetruth(→, (α, β), a)
end
