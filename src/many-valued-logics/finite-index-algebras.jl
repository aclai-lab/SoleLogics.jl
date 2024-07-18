using ..SoleLogics: AbstractAlgebra
using StaticArrays
import ..SoleLogics: syntaxstring, istop, isbot
import Base: convert

############################################################################################
#### Finite index truth ####################################################################
############################################################################################

struct FiniteIndexTruth <: Truth
    index::UInt8

    function FiniteIndexTruth(index::UInt8)
        @assert index > 0 "0 is not a valid index in Julia"
        return new(index)
    end

    function FiniteIndexTruth(index::T) where {T<:Unsigned}
        return FiniteIndexTruth(convert(UInt8, index))
    end

    function FiniteIndexTruth(index::T) where {T<:Int}
        return FiniteIndexTruth(convert(UInt8, index))
    end
end

istop(t::FiniteIndexTruth) = t.index == UInt8(1)
isbot(t::FiniteIndexTruth) = t.index == UInt8(2)

function syntaxstring(t::FiniteIndexTruth)
    if t.index < UInt8(3)
        return Char(UInt16(8867) + t.index)
    else
        return Char(UInt16(942) + t.index)
    end
end

Base.show(io::IO, t::FiniteIndexTruth) = print(io, syntaxstring(t))

function Base.convert(::Type{FiniteIndexTruth}, t::BooleanTruth)
    return istop(t) ? FiniteIndexTruth(UInt8(1)) : FiniteIndexTruth(UInt8(2))
end

# Helper
function Base.convert(::Type{FiniteIndexTruth}, c::Char)
    if convert(UInt16, c) < 945
        error("Please, provide a character between α and ҭ, ⊤ and ⊥")
    elseif convert(UInt16, c) < 1198
        return FiniteIndexTruth(convert(Int16, c) - UInt16(942))
    elseif convert(UInt16, c) < 8868
        error("Please, provide a character between α and ҭ, ⊤ and ⊥")
    elseif convert(UInt16, c) < 8870
        return FiniteIndexTruth(convert(Int16, c) - UInt16(8867))
    else
        error("Please, provide a character between α and ҭ, ⊤ and ⊥")
    end
end

############################################################################################
#### Finite algebra ########################################################################
############################################################################################

abstract type FiniteIndexAlgebra{N} <: AbstractAlgebra{FiniteIndexTruth} end

############################################################################################
#### Binary index operation ################################################################
############################################################################################

struct BinaryIndexOperation{N} <: AbstractBinaryOperation
    domain::SVector{N, FiniteIndexTruth}
    truthtable::SMatrix{N, N, FiniteIndexTruth}

    function BinaryIndexOperation{N}(
        domain::SVector{N, FiniteIndexTruth},
        truthtable::SMatrix{N, N, FiniteIndexTruth}
    ) where {
        N
    }
        return new{N}(domain, truthtable)
    end
end

Base.show(io::IO, o::BinaryIndexOperation{N}) where {N} = print(io, "$(o.truthtable)")
arity(o::BinaryIndexOperation{N}) where {N} = 2

getdomain(o::BinaryIndexOperation{N}) where {N} = o.domain

function (o::BinaryIndexOperation{N})(t1::T1, t2::T2) where {N, T1<:Truth, T2<:Truth}
    if !isa(t1, FiniteIndexTruth) t1 = convert(FiniteIndexTruth, t1) end
    if !isa(t2, FiniteIndexTruth) t2 = convert(FiniteIndexTruth, t2) end
    return o.truthtable[t1.index, t2.index]
end

############################################################################################
#### Index monoid ##########################################################################
############################################################################################

struct IndexMonoid{N} <: FiniteIndexAlgebra{N}
    operation::BinaryIndexOperation{N}
    identityelement::FiniteIndexTruth

    function IndexMonoid{N}(
        operation::BinaryIndexOperation{N},
        identityelement::T
    ) where {
        N,
        T<:Truth
    }
        if !isa(identityelement, FiniteIndexTruth)
            identityelement = convert(FiniteIndexTruth, identityelement)
        end
        checkmonoidaxioms(operation, identityelement)
        return new{N}(operation, identityelement)
    end
end

ismonoid(::IndexMonoid{N}) where {N} = true

function Base.convert(
    ::Type{IndexMonoid{N}},
    m::M
) where {
    N,
    M<:FiniteIndexAlgebra{N}
}
    if ismonoid(m)
        return IndexMonoid{N}(m.operation, m.identityelement)
    else
        error("Cannot convert object of type $(typeof(m)) to a value of type Monoid{$T,$D).")
    end
end

function checkaxiom(a::Axiom, m::IndexMonoid{N}) where {N}
    return checkaxiom(typeof(a), m.operation)
end

(m::IndexMonoid{N})(t1::T1, t2::T2) where {N, T1<:Truth, T2<:Truth} = m.operation(t1, t2)

############################################################################################
#### Commutative index monoid ##############################################################
############################################################################################

struct CommutativeIndexMonoid{N} <: FiniteIndexAlgebra{N}
    operation::BinaryIndexOperation{N}
    identityelement::FiniteIndexTruth

    function CommutativeIndexMonoid{N}(
        operation::BinaryIndexOperation{N},
        identityelement::T
    ) where {
        N,
        T<:Truth
    }
        if !isa(identityelement, FiniteIndexTruth)
            identityelement = convert(FiniteIndexTruth, identityelement)
        end
        checkmonoidaxioms(operation, identityelement)
        @assert checkaxiom(Commutativity, operation) "Defined an operation for the " *
            "commutative monoid which is not commutative."
        return new{N}(operation, identityelement)
    end
end

ismonoid(::CommutativeIndexMonoid{N}) where {N} = true

function (m::CommutativeIndexMonoid{N})(t1::T1, t2::T2) where {N, T1<:Truth, T2<:Truth}
    return m.operation(t1, t2)
end

############################################################################################
#### Finite index lattice ##################################################################
############################################################################################

struct FiniteIndexLattice{N} <: FiniteIndexAlgebra{N}
    join::BinaryIndexOperation{N}
    meet::BinaryIndexOperation{N}

    function FiniteIndexLattice{N}(
        join::BinaryIndexOperation{N},
        meet::BinaryIndexOperation{N}
    ) where {
        N
    }
        checklatticeaxioms(join, meet)
        return new{N}(join, meet)
    end
end

islattice(::FiniteIndexLattice{N}) where {N} = true

function convert(
    ::Type{FiniteIndexLattice{N}},
    l::L
) where {
    N,
    L<:FiniteIndexAlgebra{N}
}
    if islattice(l)
        return FiniteIndexLattice{N}(l.join, l.meet)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type Lattice.")
    end
end


# julia> using SoleLogics
# [ Info: Precompiling SoleLogics [b002da8f-3cb3-4d91-bbe3-2953433912b5]

# julia> using SoleLogics.ManyValuedLogics

# julia> using SoleLogics.ManyValuedLogics: FiniteIndexTruth, BinaryIndexOperation, IndexMonoid, CommutativeIndexMonoid, FiniteIndexLattice

# julia> using StaticArrays

# julia> α = FiniteIndexTruth(3)
# α

# julia> domain = SVector{3, FiniteIndexTruth}([⊤, ⊥, α])
# 3-element SVector{3, FiniteIndexTruth} with indices SOneTo(3):
#  ⊤
#  ⊥
#  α

# julia> meettruthtable = SMatrix{3, 3, FiniteIndexTruth}([⊤, ⊥, α, ⊥, ⊥, ⊥, α, ⊥, α])
# 3×3 SMatrix{3, 3, FiniteIndexTruth, 9} with indices SOneTo(3)×SOneTo(3):
#  ⊤  ⊥  α
#  ⊥  ⊥  ⊥
#  α  ⊥  α

# julia> jointruthtable = SMatrix{3, 3, FiniteIndexTruth}([⊤, ⊤, ⊤, ⊤, ⊥, α, ⊤, α, α])
# 3×3 SMatrix{3, 3, FiniteIndexTruth, 9} with indices SOneTo(3)×SOneTo(3):
#  ⊤  ⊤  ⊤
#  ⊤  ⊥  α
#  ⊤  α  α

# julia> ∧ = BinaryIndexOperation{3}(domain, meettruthtable)
# FiniteIndexTruth[⊤ ⊥ α; ⊥ ⊥ ⊥; α ⊥ α]

# julia> ∨ = BinaryIndexOperation{3}(domain, jointruthtable)
# FiniteIndexTruth[⊤ ⊤ ⊤; ⊤ ⊥ α; ⊤ α α]

# julia> L = FiniteIndexLattice(∨, ∧)
# ERROR: MethodError: no method matching FiniteIndexLattice(::BinaryIndexOperation{3}, ::BinaryIndexOperation{3})
# Stacktrace:
#  [1] top-level scope
#    @ REPL[11]:1

# julia> L = FiniteIndexLattice{3}(∨, ∧)
# FiniteIndexLattice{3}(FiniteIndexTruth[⊤ ⊤ ⊤; ⊤ ⊥ α; ⊤ α α], FiniteIndexTruth[⊤ ⊥ α; ⊥ ⊥ ⊥; α ⊥ α])
