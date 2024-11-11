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
istop(t::UInt8) = t == UInt8(1)
isbot(t::UInt8) = t == UInt8(2)

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
Base.convert(::Type{FiniteIndexTruth}, index::UInt8) = FiniteIndexTruth(index)

############################################################################################
#### Binary index operation ################################################################
############################################################################################

struct BinaryIndexOperation{N} <: AbstractBinaryOperation
    truthtable::SMatrix{N, N, FiniteIndexTruth}

    function BinaryIndexOperation{N}(truthtable::SMatrix{N, N, FiniteIndexTruth}) where {N}
        return new{N}(truthtable)
    end

    function BinaryIndexOperation{N}(truthtable::Array{FiniteIndexTruth, 1}) where {N}
        return BinaryIndexOperation{N}(SMatrix{N, N, FiniteIndexTruth}(truthtable))
    end
end

Base.show(io::IO, o::BinaryIndexOperation{N}) where {N} = print(io, "$(o.truthtable)")
arity(o::BinaryIndexOperation{N}) where {N} = 2

function getdomain(::BinaryIndexOperation{N}) where {N}
    return SVector{N,FiniteIndexTruth}(FiniteIndexTruth.([1:N]...))
end

function (o::BinaryIndexOperation{N})(t1::UInt8, t2::UInt8) where {N}
    return o.truthtable[t1, t2]
end

function (o::BinaryIndexOperation{N})(t1::FiniteIndexTruth, t2::UInt8) where {N}
    return o.truthtable[t1.index, t2]
end

function (o::BinaryIndexOperation{N})(t1::UInt8, t2::FiniteIndexTruth) where {N}
    return o.truthtable[t1, t2.index]
end

function (o::BinaryIndexOperation{N})(t1::FiniteIndexTruth, t2::FiniteIndexTruth) where {N}
    return o.truthtable[t1.index, t2.index]
end

############################################################################################
#### Finite algebra ########################################################################
############################################################################################

abstract type FiniteIndexAlgebra{N} <: AbstractAlgebra{FiniteIndexTruth} end

function getdomain(::A) where {N, A<:FiniteIndexAlgebra{N}}
    return SVector{N,FiniteIndexTruth}(FiniteIndexTruth.([1:N]...))
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

(m::IndexMonoid{N})(t1::UInt8, t2::UInt8) where {N} = m.operation(t1, t2)
(m::IndexMonoid{N})(t1::UInt8, t2::FiniteIndexTruth) where {N} = m.operation(t1, t2.index)
(m::IndexMonoid{N})(t1::FiniteIndexTruth, t2::UInt8) where {N} = m.operation(t1.index, t2)
function (m::IndexMonoid{N})(t1::FiniteIndexTruth, t2::FiniteIndexTruth) where {N}
    return m.operation(t1.index, t2.index)
end

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

(m::CommutativeIndexMonoid{N})(t1::UInt8, t2::UInt8) where {N} = m.operation(t1, t2)
function (m::CommutativeIndexMonoid{N})(t1::UInt8, t2::FiniteIndexTruth) where {N}
    return m.operation(t1, t2.index)
end
function (m::CommutativeIndexMonoid{N})(t1::FiniteIndexTruth, t2::UInt8) where {N}
    return m.operation(t1.index, t2)
end
function (m::CommutativeIndexMonoid{N})(t1::FiniteIndexTruth, t2::FiniteIndexTruth) where {N}
    return m.operation(t1.index, t2.index)
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

############################################################################################
#### Finite index bounded lattice ##########################################################
############################################################################################

struct FiniteIndexBoundedLattice{N} <: FiniteIndexAlgebra{N}
    join::BinaryIndexOperation{N}
    meet::BinaryIndexOperation{N}
    bot::FiniteIndexTruth
    top::FiniteIndexTruth

    function FiniteIndexBoundedLattice{N}(
        join::BinaryIndexOperation{N},
        meet::BinaryIndexOperation{N},
        bot::T1,
        top::T2
    ) where {
        N,
        T1<:Truth,
        T2<:Truth
    }
        if !isa(bot, FiniteIndexTruth) bot = convert(FiniteIndexTruth, bot) end
        if !isa(top, FiniteIndexTruth) top = convert(FiniteIndexTruth, top) end
        checkboundedlatticeaxioms(join, meet, bot, top)
        return new{N}(join, meet, bot, top)
    end
end

islattice(::FiniteIndexBoundedLattice{N}) where {N} = true
isboundedlattice(::FiniteIndexBoundedLattice{N}) where {N} = true

function convert(
    ::Type{FiniteIndexBoundedLattice{N}},
    l::L
) where {
    N,
    L<:FiniteIndexAlgebra{N}
}
    if isboundedlattice(l)
        return FiniteIndexBoundedLattice{N}(l.join, l.meet, l.bot, l.top)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type Lattice.")
    end
end

############################################################################################
#### Finite index FLew algebra #############################################################
############################################################################################

struct FiniteIndexFLewAlgebra{N} <: FiniteIndexAlgebra{N}
    join::BinaryIndexOperation{N}
    meet::BinaryIndexOperation{N}
    monoid::CommutativeIndexMonoid{N}
    implication::BinaryIndexOperation{N}
    bot::FiniteIndexTruth
    top::FiniteIndexTruth

    function FiniteIndexFLewAlgebra{N}(
        join::BinaryIndexOperation{N},
        meet::BinaryIndexOperation{N},
        monoid::CommutativeIndexMonoid{N},
        bot::T1,
        top::T2
    ) where {
        N,
        T1<:Truth,
        T2<:Truth
    }
        if !isa(bot, FiniteIndexTruth) bot = convert(FiniteIndexTruth, bot) end
        if !isa(top, FiniteIndexTruth) top = convert(FiniteIndexTruth, top) end
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(RightResidual, meet, monoid) "Residuation property does not " *
            "hold for the defined monoid operation."

        implicationtruthtable = Array{FiniteIndexTruth}(undef, N, N)
        for z ∈ UInt8(1):UInt8(N)
            for x ∈ UInt8(1):UInt8(N)
                candidates = Vector{FiniteIndexTruth}()
                for y ∈ UInt8(1):UInt8(N)
                    meet(monoid(x, y), z) == monoid(x, y) && push!(candidates, y)
                end
                for y ∈ candidates
                    isgreatest = true
                    for w ∈ candidates
                        if meet(w, y) != w
                            isgreatest = false
                            break
                        end
                    end
                    if isgreatest
                        implicationtruthtable[x,z] = y
                        break
                    end
                end
            end
        end
        implication = BinaryIndexOperation{N}(SMatrix{3, 3, FiniteIndexTruth}(implicationtruthtable))
        return new{N}(join, meet, monoid, implication, bot, top)
    end

    function FiniteIndexFLewAlgebra{N}(
        join::BinaryIndexOperation{N},
        meet::BinaryIndexOperation{N},
        monoidoperation::BinaryIndexOperation{N},
        bot::T1,
        top::T2
    ) where {
        N,
        T1<:Truth,
        T2<:Truth
    }
        return FiniteIndexFLewAlgebra{N}(
            join,
            meet,
            CommutativeIndexMonoid{3}(monoidoperation, top),
            bot,
            top
        )
    end
end

islattice(::FiniteIndexFLewAlgebra{N}) where {N} = true
isboundedlattice(::FiniteIndexFLewAlgebra{N}) where {N} = true

function Base.show(io::IO, a::FiniteIndexFLewAlgebra{N}) where {N}
    println(io, string(typeof(a)))
    println(io, "Domain: " * string(getdomain(a)))
    println(io, "Bot: " * string(a.bot))
    println(io, "Top: " * string(a.top))
    println(io, "Join: " * string(a.join))
    println(io, "Meet: " * string(a.meet))
    println(io, "T-norm: " * string(a.monoid))
    println(io, "Implication: " * string(a.implication))
end

############################################################################################
#### Order utilities #######################################################################
############################################################################################

function precedeq(
    l::L,
    t1::FiniteIndexTruth,
    t2::FiniteIndexTruth
) where {
    N,
    L<:FiniteIndexAlgebra{N}
}
    !islattice(l) && error("Cannot convert object of type $(typeof(l)) to an object of " *
        "type FiniteLattice.")
    if l.meet(t1, t2) == t1
        return true
    else
        return false
    end
end

function precedes(
    l::L,
    t1::FiniteIndexTruth,
    t2::FiniteIndexTruth
) where {
    N,
    L<:FiniteIndexAlgebra{N}
}
    return t1 != t2 && precedeq(l, t1, t2)
end

function succeedeq(
    l::L,
    t1::FiniteIndexTruth,
    t2::FiniteIndexTruth
) where {
    N,
    L<:FiniteIndexAlgebra{N}
}
    return precedeq(l, t2, t1)
end

function succeedes(
    l::L,
    t1::FiniteIndexTruth,
    t2::FiniteIndexTruth
) where {
    N,
    L<:FiniteIndexAlgebra{N}
}
    return precedes(l, t2, t1)
end

function lesservalues(
    l::L,
    t::FiniteIndexTruth
) where {
    N,
    L<:FiniteIndexAlgebra{N},
}
    return filter(ti->precedes(l, ti, t), getdomain(l))
end

function maximalmembers(
    l::L,
    t::FiniteIndexTruth
) where {
    N,
    L<:FiniteIndexAlgebra{N},
}
    candidates = filter(ti->!succeedeq(l, ti, t), getdomain(l))
    mm = D()
    for c in candidates
        if isempty(filter(ti->succeedes(l, ti, c), candidates)) push!(mm, c) end
    end
    return mm
end

function minimalmembers(
    l::L,
    t::FiniteIndexTruth
) where {
    N,
    L<:FiniteIndexAlgebra{N},
}
    candidates = filter(ti->!precedeq(l, ti, t), getdomain(l))
    mm = D()
    for c in candidates
        if isempty(filter(ti->precedes(l, ti, c), candidates)) push!(mm, c) end
    end
    return mm
end
