"""
A `FiniteTruth` is represented by its `index` in a `FiniteDomain`.

Therefore, a `FiniteTruth` by itself is just a symbol without meaning: it must be associated
with a structure comprising a `FiniteDomain`.

By convention, `⊤` (top, 1) and `⊥` (bot, 0) always have indexes `1` and `2`, respectively.
"""
struct FiniteTruth{T<:Integer} <: Truth
    index::T

    function FiniteTruth(index::T) where {T<:Integer}
        if index < 1 
            throw(ArgumentError("`index` must be greater than 0"))
        else
            return new{T}(index)
        end
    end
end

SoleLogics.istop(t::FiniteTruth) = t.index == 1
SoleLogics.isbot(t::FiniteTruth) = t.index == 2

function SoleLogics.syntaxstring(t::FiniteTruth; kwargs...)
    t.index < 3 ? Char(t.index + 8867) : "a$(t.index-2)"
end

Base.show(io::IO, t::FiniteTruth) = print(io, syntaxstring(t))

function Base.convert(::Type{FiniteTruth}, t::BooleanTruth)
    return istop(t) ? FiniteTruth(1) : FiniteTruth(2)
end

"""
A `FiniteDomain` is a set of `FiniteTruth` values.
"""
struct FiniteDomain{T<:Integer}
    values::Vector{FiniteTruth}

    function FiniteDomain(cardinality::T) where {T<:Integer} 
        if cardinality < 2
            throw(ArgumentError("`cardinality` must be greater than 1"))
        else
            return new{T}(FiniteTruth.([1:cardinality]...), cardinality)
        end
    end
end

Base.show(io::IO, d::FiniteDomain) = for t in d.values print(io, syntaxstring(t) * " ") end

"""
A `FiniteBinaryOperation` is defined over a `FiniteDomain`.

Each possible tuple of `FiniteTruth` values from the `FiniteDomain` is mapped to a
`FiniteTruth` value in the `FiniteDomain` through a `truthtable`.

Since all `FiniteDomain`s with the same number of elements are equivalent, cardinality of
the `FiniteDomain` is used as a type parameter to ease verifying wether algebraic structures
are defined over the same domain.
"""
struct FiniteBinaryOperation{N,T<:Integer}
    truthtable::Matrix{FiniteTruth{T}}

    function FiniteBinaryOperation(
        truthtable::Matrix{FiniteTruth{T}}
    ) where {
        T<:Integer
    }
        if size(truthtable)[1] != size(truthtable)[2]
            throw(ArgumentError("`truthtable` must be squared"))
        else
            return new{size(truthtable)[1], T}(truthtable)
        end
    end
end

getdomain(::FiniteBinaryOperation{N,T}) where {N,T} = FiniteDomain(N)

function Base.show(io::IO, o::FiniteBinaryOperation)
    domain = getdomain(o)
    for t in domain.values print(io, "\t" * syntaxstring(t)) end
    print(io, "\n")
    for i in eachindex(domain.values)
        print(io, syntaxstring(domain.values[i]))
        for j in eachindex(domain.values) 
            print(io, "\t" * syntaxstring(domain.values[o.truthtable[i, j].index]))
        end
        print(io, "\n")
    end
end

function (o::FiniteBinaryOperation)(t1::T1, t2::T2) where {T1<:Truth, T2<:Truth}
    if !isa(t1, FiniteTruth) t1 = convert(FiniteTruth, t1) end
    if !isa(t2, FiniteTruth) t2 = convert(FiniteTruth, t2) end
    return o.truthtable[t1.index, t2.index]
end

abstract type FiniteAlgebra{N} <: SoleLogics.AbstractAlgebra{FiniteTruth} end

getdomain(::A) where {N, A<:FiniteAlgebra{N}} = FiniteDomain(N)