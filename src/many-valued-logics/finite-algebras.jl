using ..SoleLogics: AbstractAlgebra
using StaticArrays
import ..SoleLogics: syntaxstring, istop, isbot
import Base: convert

############################################################################################
#### Finite algebra ########################################################################
############################################################################################

"""
    abstract type FiniteAlgebra{N} <: AbstractAlgebra{FiniteTruth} end

A finite algebra is an algebraic structure defined over a finite set.

See also [`AbstractAlgebra`](@ref).
"""
abstract type FiniteAlgebra{N} <: AbstractAlgebra{FiniteTruth} end

function getdomain(::A) where {N, A<:FiniteAlgebra{N}}
    return SVector{N,FiniteTruth}(FiniteTruth.([1:N]...))
end

############################################################################################
#### Monoid ################################################################################
############################################################################################

"""
    function checkmonoidaxioms(
        o::O,
        e::T
    ) where {
        O<:AbstractBinaryOperation,
        T<:Truth
    }

Check if given domain, operation and identity element form a monoid.

A monoid (S, ⋅, e) is a set S equipped with a binary operation S × S → S, denoted as ⋅,
satisfying the following axiomatic identities:
 - (Associativity) ∀ a, b, c ∈ S, the equation (a ⋅ b) ⋅ c = a ⋅ (b ⋅ c) holds.
 - (Identity element) There exists an element e ∈ L such that for every element a ∈ S, the
   equalities e ⋅ a = a and a ⋅ e = a hold. 

The identity element of a monoid is unique.

See also [`AbstractBinaryOperation`](@ref), [`Axiom`](@ref), [`checkaxiom`](@ref),
[`Associativity`](@ref), [`IdentityElement`](@ref).
"""
function checkmonoidaxioms(
    o::O,
    e::T
) where {
    O<:AbstractBinaryOperation,
    T<:Truth
}
    @assert checkaxiom(Associativity, o) "Defined an operation for the monoid which " *
        "is not associative."
    @assert checkaxiom(IdentityElement, o, e) "$e is not a valid identity element for " *
        "the defined operation."
    return nothing
end

"""
    struct Monoid{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T}}
        operation::B
        identityelement::T
    end

A monoid (S, ⋅, e) is a set S equipped with a binary operation S × S → S, denoted as ⋅,
satisfying the following axiomatic identities:
 - (Associativity) ∀ a, b, c ∈ S, the equation (a ⋅ b) ⋅ c = a ⋅ (b ⋅ c) holds.
 - (Identity element) There exists an element e ∈ L such that for every element a ∈ S, the
   equalities e ⋅ a = a and a ⋅ e = a hold. 

The identity element of a monoid is unique.

See also [`BinaryOperation`](@ref), [`Axiom`](@ref), [`checkaxiom`](@ref),
[`checkmonoidaxioms`](@ref), [`Associativity`](@ref), [`IdentityElement`](@ref).
"""
struct Monoid{N} <: FiniteAlgebra{N}
    operation::BinaryOperation{N}
    identityelement::FiniteTruth

    function Monoid{N}(
        operation::BinaryOperation{N},
        identityelement::T
    ) where {
        N,
        T<:Truth
    }
        if !isa(identityelement, FiniteTruth)
            identityelement = convert(FiniteTruth, identityelement)
        end
        checkmonoidaxioms(operation, identityelement)
        return new{N}(operation, identityelement)
    end
end

"""
Return true if the object can be converted to an object of type `Monoid`.

See also [`Monoid`](@ref).
"""
ismonoid(::Monoid{N}) where {N} = true
ismonoid(::T) where {T} = false  

"""
    function Base.convert(
        ::Type{Monoid{N}},
        m::M
    ) where {
        N,
        M<:FiniteAlgebra{N}
    }

Convert `m` to a value of type `Monoid`.

See also [`Monoid`](@ref), [`ismonoid`](@ref).
"""
function Base.convert(
    ::Type{Monoid{N}},
    m::M
) where {
    N,
    M<:FiniteAlgebra{N}
}
    if ismonoid(m)
        return Monoid{N}(m.operation, m.identityelement)
    else
        error("Cannot convert object of type $(typeof(m)) to a value of type Monoid{$T,$D).")
    end
end

"""
    function checkaxiom(a::Axiom, m::Monoid{N}) where {N}

Check if axiom `a` is satisfied by the operation of the monoid `m`.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(a::Axiom, m::Monoid{N}) where {N}
    return checkaxiom(typeof(a), m.operation)
end

"""
Helper allowing to use monoids with function notation.

See also [`Monoid`](@ref), [`BinaryOperation`](@ref).
"""
(m::Monoid{N})(t1::UInt8, t2::UInt8) where {N} = m.operation(t1, t2)
(m::Monoid{N})(t1::UInt8, t2::FiniteTruth) where {N} = m.operation(t1, t2.index)
(m::Monoid{N})(t1::FiniteTruth, t2::UInt8) where {N} = m.operation(t1.index, t2)
function (m::Monoid{N})(t1::FiniteTruth, t2::FiniteTruth) where {N}
    return m.operation(t1.index, t2.index)
end

############################################################################################
#### Commutative monoid ####################################################################
############################################################################################

"""
    struct CommutativeMonoid{N} <: FiniteAlgebra{N}
        operation::BinaryOperation{N}
        identityelement::FiniteTruth
    end

A commutative monoid (S, ⋅, e) is a monoid whose operation is commutative.

See also [`Monoid`](@ref), [`Commutativity`](@ref).
"""
struct CommutativeMonoid{N} <: FiniteAlgebra{N}
    operation::BinaryOperation{N}
    identityelement::FiniteTruth

    function CommutativeMonoid{N}(
        operation::BinaryOperation{N},
        identityelement::T
    ) where {
        N,
        T<:Truth
    }
        if !isa(identityelement, FiniteTruth)
            identityelement = convert(FiniteTruth, identityelement)
        end
        checkmonoidaxioms(operation, identityelement)
        @assert checkaxiom(Commutativity, operation) "Defined an operation for the " *
            "commutative monoid which is not commutative."
        return new{N}(operation, identityelement)
    end
end

ismonoid(::CommutativeMonoid{N}) where {N} = true


"""
Helper allowing to use commutative monoids with function notation.

See also [`Monoid`](@ref), [`BinaryOperation`](@ref).
"""
(m::CommutativeMonoid{N})(t1::UInt8, t2::UInt8) where {N} = m.operation(t1, t2)
function (m::CommutativeMonoid{N})(t1::UInt8, t2::FiniteTruth) where {N}
    return m.operation(t1, t2.index)
end
function (m::CommutativeMonoid{N})(t1::FiniteTruth, t2::UInt8) where {N}
    return m.operation(t1.index, t2)
end
function (m::CommutativeMonoid{N})(t1::FiniteTruth, t2::FiniteTruth) where {N}
    return m.operation(t1.index, t2.index)
end

############################################################################################
#### Finite lattice ########################################################################
############################################################################################

"""
    function checklatticeaxioms(
        join::O,
        meet::O
    ) where {
        O<:AbstractBinaryOperation
    }

Check if given domain, join and meet form a finite lattice.

A lattice is an algebraic structure (L, ∨, ∧) consisting of a set L and two binary,
commutative and associative operations ∨ and ∧ on L satisfying the following axiomatic
identities for all elements a, b ∈ L (sometimes called absorption laws):
- a ∨ (a ∧ b) = a
- a ∧ (a ∨ b) = a

See also [`FiniteLattice`](@ref), [`AbstractBinaryOperation`](@ref), [`Commutativity`](@ref),
[`Associativity`](@ref), [`AbsorptionLaw`](@ref),
"""
function checklatticeaxioms(
    join::O,
    meet::O
) where {
    O<:AbstractBinaryOperation
}
    @assert checkaxiom(Commutativity, join) "Defined a join operation which is not " *
        "commutative."
    @assert checkaxiom(Associativity, join) "Defined a join operation which is not " *
        "associative."
    @assert checkaxiom(Commutativity, meet) "Defined a meet operation which is not " *
        "commutative." 
    @assert checkaxiom(Associativity, meet) "Defined a meet operation which is not " *
        "associative."
    @assert checkaxiom(AbsorptionLaw, join, meet) "Absorption law does not hold between " *
        "join and meet."
    return nothing
end

"""
    struct FiniteLattice{N} <: FiniteAlgebra{N}
        join::BinaryOperation{N}
        meet::BinaryOperation{N}
    end

A finite lattice is a lattice defined over a finite set.

A lattice is an algebraic structure (L, ∨, ∧) consisting of a set L and two binary,
commutative and associative operations ∨ and ∧ on L satisfying the following axiomatic
identities for all elements a, b ∈ L (sometimes called absorption laws):
 - a ∨ (a ∧ b) = a
 - a ∧ (a ∨ b) = a

See also [`FiniteAlgebra`](@ref), [`BinaryOperation`](@ref).
"""
struct FiniteLattice{N} <: FiniteAlgebra{N}
    join::BinaryOperation{N}
    meet::BinaryOperation{N}

    function FiniteLattice{N}(
        join::BinaryOperation{N},
        meet::BinaryOperation{N}
    ) where {
        N
    }
        checklatticeaxioms(join, meet)
        return new{N}(join, meet)
    end
end

"""
Return true if the object can be converted to an object of type `FiniteLattice`.

See also [`FiniteLattice`](@ref).
"""
islattice(::FiniteLattice{N}) where {N} = true
islattice(::T) where {T} = false

"""
    function convert(
        ::Type{FiniteLattice{N}},
        l::L
    ) where {
        N,
        L<:FiniteAlgebra{N}
    }

Convert `l` to a value of type `FiniteLattice`.

See also [`FiniteLattice`](@ref), [`islattice`](@ref).
"""
function convert(
    ::Type{FiniteLattice{N}},
    l::L
) where {
    N,
    L<:FiniteAlgebra{N}
}
    if islattice(l)
        return FiniteLattice{N}(l.join, l.meet)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type Lattice.")
    end
end

############################################################################################
#### Finite bounded lattice ################################################################
############################################################################################

"""
    function checkboundedlatticeaxioms(
        join::O,
        meet::O,
        bot::T,
        top::T
    ) where {
        O<:AbstractBinaryOperation,
        T<:Truth
    }

Check if given domain, join, meet, bot and top form a bounded lattice.

A bounded lattice is an algebraic structure (L, ∨, ∧, ⊥, ⊤) such that (L, ∨, ∧) is a
lattice, the bottom element ⊥ is the identity element for the join operation ∨, and the top
element ⊤ is the identity element for the meet operation ∧:
 - a ∨ ⊥ = a
 - a ∧ ⊤ = a

See also [`FiniteBoundedLattice`](@ref), [`checklatticeaxioms`](@ref).
"""
function checkboundedlatticeaxioms(
    join::O,
    meet::O,
    bot::T,
    top::T
) where {
    O<:AbstractBinaryOperation,
    T<:Truth
}
    checklatticeaxioms(join, meet)
    @assert checkaxiom(IdentityElement, join, bot) "$bot is not a valid identity element " *
        "for the defined join operation."
    @assert checkaxiom(IdentityElement, meet, top) "$top is not a valid identity element " *
        "for the defined meet operation."
    return nothing
end

"""
    struct FiniteBoundedLattice{N} <: FiniteAlgebra{N}
        join::BinaryOperation{N}
        meet::BinaryOperation{N}
        bot::FiniteTruth
        top::FiniteTruth
    end

A finite bounded lattice is a bounded lattice defined over a finite set.

A bounded lattice is an algebraic structure (L, ∨, ∧, ⊥, ⊤) such that (L, ∨, ∧) is a
lattice, the bottom element ⊥ is the identity element for the join operation ∨, and the top
element ⊤ is the identity element for the meet operation ∧:
 - a ∨ ⊥ = a
 - a ∧ ⊤ = a

See also [`FiniteLattice`](@ref).
"""
struct FiniteBoundedLattice{N} <: FiniteAlgebra{N}
    join::BinaryOperation{N}
    meet::BinaryOperation{N}
    bot::FiniteTruth
    top::FiniteTruth

    function FiniteBoundedLattice{N}(
        join::BinaryOperation{N},
        meet::BinaryOperation{N},
        bot::T1,
        top::T2
    ) where {
        N,
        T1<:Truth,
        T2<:Truth
    }
        if !isa(bot, FiniteTruth) bot = convert(FiniteTruth, bot) end
        if !isa(top, FiniteTruth) top = convert(FiniteTruth, top) end
        checkboundedlatticeaxioms(join, meet, bot, top)
        return new{N}(join, meet, bot, top)
    end
end

islattice(::FiniteBoundedLattice{N}) where {N} = true

"""
Return true if the object can be converted to an object of type `FiniteBoundedLattice`.

See also [`FiniteBoundedLattice`](@ref).
"""
isboundedlattice(::FiniteBoundedLattice{N}) where {N} = true
isboundedlattice(::T) where {T} = false

"""
    function convert(
        ::Type{FiniteBoundedLattice{N}},
        l::L
    ) where {
        N,
        L<:FiniteAlgebra{N}
    }

Convert `l` to a value of type `FiniteBoundedLattice`.

See also [`FiniteBoundedLattice`](@ref), [`isboundedlattice`](@ref).
"""
function convert(
    ::Type{FiniteBoundedLattice{N}},
    l::L
) where {
    N,
    L<:FiniteAlgebra{N}
}
    if isboundedlattice(l)
        return FiniteBoundedLattice{N}(l.join, l.meet, l.bot, l.top)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type Lattice.")
    end
end

############################################################################################
#### Residuated lattice ####################################################################
############################################################################################

"""
    const RightResidual

The right residual between two elements z, x ∈ S is the greatest y ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref), [`checkaxiom`](@ref).
"""
const RightResidual = Axiom{:RR}()

"""
    function checkaxiom(
        ::typeof(RightResidual),
        m::Monoid{T}
    ) where {
        T<:Truth,
    }

Check that ∀ x ∈ S there exists for every x ∈ S a greatest y ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(
    ::typeof(RightResidual),
    meet::O,
    monoid::M
) where {
    O<:AbstractBinaryOperation,
    T<:Truth,
    M<:AbstractAlgebra{T}
}
    !ismonoid(monoid) && error("Cannot convert an object of type $(typeof(monoid)) to an "*
        "object of type Monoid.")
    for z ∈ getdomain(monoid)
        for x ∈ getdomain(monoid)
            candidates = Vector{T}()
            for y ∈ getdomain(monoid)
                meet(monoid(x, y), z) == monoid(x, y) && push!(candidates, y)
            end
            foundrr = false
            for y ∈ candidates
                isgreatest = true
                for w ∈ candidates
                    if meet(w, y) != w
                        isgreatest = false
                        break
                    end
                end
                if isgreatest
                    foundrr = true
                    break
                end
            end
            !foundrr && return false
        end
    end
    return true
end

"""
    const LeftResidual

The left residual between two elements z, y ∈ S is the greatest x ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref), [`checkaxiom`](@ref).
"""
const LeftResidual = Axiom{:LR}()

"""
    function checkaxiom(
        ::typeof(LeftResidual),
        m::Monoid{T}
    ) where {
        T<:Truth,
    }

Check that ∀ x ∈ S there exists for every y ∈ S a greatest x ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(
    ::typeof(LeftResidual),
    meet::O,
    monoid::M
) where {
    O<:AbstractBinaryOperation,
    T<:Truth,
    M<:AbstractAlgebra{T}
}
    !ismonoid(monoid) && error("Cannot convert an object of type $(typeof(monoid)) to an "*
        "object of type Monoid.")
    for z ∈ getdomain(monoid)
        for y ∈ getdomain(monoid)
            candidates = Vector{T}()
            for x ∈ getdomain(monoid)
                meet(monoid(x, y), z) == monoid(x, y) && push!(candidates, x)
            end
            foundlr = false
            for x ∈ candidates
                isgreatest = true
                for w ∈ candidates
                    if meet(w, x) != w
                        isgreatest = false
                        break
                    end
                end
                if isgreatest
                    foundlr = true
                    break
                end
            end
            !foundlr && return false
        end
    end
    return true
end

"""
    const ResiduationProperty

A lattice (L, ∨, ∧, ⋅, ⊥, →) is residuated if ∀ x ∈ S there exists for every x ∈ S a
greatest y ∈ S and for every y ∈ S a greatest x ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref), [`checkaxiom`](@ref).
"""
const ResiduationProperty = Axiom{:RP}()

"""
    function checkaxiom(
        ::typeof(ResiduationProperty),
        m::Monoid{N}
    ) where {
        N
    }

Check that ∀ x ∈ S there exists for every x ∈ S a greatest y ∈ S and for every y ∈ S a
greatest x ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(
    ::typeof(ResiduationProperty),
    meet::O,
    monoid::M
) where {
    O<:AbstractBinaryOperation,
    T<:Truth,
    M<:AbstractAlgebra{T}
}
    !ismonoid(monoid) && error("Cannot convert an object of type $(typeof(monoid)) to an "*
        "object of type Monoid.")
    if checkaxiom(RightResidual, meet, monoid) && checkaxiom(LeftResidual, meet, monoid)
        return true
    else
        return false
    end        
end

"""
    struct FiniteResiduatedLattice{N} <: FiniteAlgebra{N}
        join::BinaryOperation{N}
        meet::BinaryOperation{N}
        monoid::CommutativeMonoid{N}
        rightresidual::BinaryOperation{N}
        leftresidual::BinaryOperation{N}
        bot::FiniteTruth
        top::FiniteTruth
    end

A residuated lattice is an algebraic structure L = (L, ∨, ∧, ⋅, e) such that:
 - (L, ∨, ∧) is a lattice
 - (L, ⋅, e) is a monoid
 - ∀ x ∈ L there exists for every x ∈ L a greatest y ∈ L and for every y ∈ L a greatest
   x ∈ L such that x ⋅ y ≤ z

See also [`FiniteBoundedLattice`](@ref), 
"""
struct FiniteResiduatedLattice{N} <: FiniteAlgebra{N}
    join::BinaryOperation{N}
    meet::BinaryOperation{N}
    monoid::Monoid{N}
    rightresidual::BinaryOperation{N}
    leftresidual::BinaryOperation{N}
    bot::FiniteTruth
    top::FiniteTruth

    function FiniteResiduatedLattice{N}(
        join::BinaryOperation{N},
        meet::BinaryOperation{N},
        monoid::M,
        bot::T1,
        top::T2
    ) where {
        N,
        M<:FiniteAlgebra{N},
        T1<:Truth,
        T2<:Truth
    }
        if !isa(monoid, Monoid{N}) monoid = convert(Monoid{N}, monoid)::Monoid{N} end
        if !isa(bot, FiniteTruth) bot = convert(FiniteTruth, bot)::FiniteTruth end
        if !isa(top, FiniteTruth) top = convert(FiniteTruth, top)::FiniteTruth end
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(ResiduationProperty, meet, monoid) "Residuation property does " *
            "not hold for the defined monoid operation."

        rtruthtable = Array{FiniteTruth}(undef, N, N)
        ltruthtable = Array{FiniteTruth}(undef, N, N)
        for z ∈ UInt8(1):UInt8(N)
            for x ∈ UInt8(1):UInt8(N)
                candidates = Vector{FiniteTruth}()
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
                        rtruthtable[x,z] = y
                        break
                    end
                end
            end
            for y ∈ UInt8(1):UInt8(N)
                candidates = Vector{FiniteTruth}()
                for x ∈ UInt8(1):UInt8(N)
                    meet(monoid(x, y), z) == monoid(x, y) && push!(candidates, x)
                end
                for x ∈ candidates
                    isgreatest = true
                    for w ∈ candidates
                        if meet(w, x) != w
                            isgreatest = false
                            break
                        end
                    end
                    if isgreatest
                        ltruthtable[y,z] = x
                        break
                    end
                end
            end
        end
        rightresidual = BinaryOperation{N}(SMatrix{N, N, FiniteTruth}(rtruthtable))
        leftresidual = BinaryOperation{N}(SMatrix{N, N, FiniteTruth}(ltruthtable))
        return new{N}(join, meet, monoid, rightresidual, leftresidual, bot, top)
    end
end

############################################################################################
#### Finite FLew algebra ###################################################################
############################################################################################

"""
    struct FiniteFLewAlgebra{N} <: FiniteAlgebra{N}
        join::BinaryOperation{N}
        meet::BinaryOperation{N}
        monoid::CommutativeMonoid{N}
        implication::BinaryOperation{N}
        bot::FiniteTruth
        top::FiniteTruth
    end

An FLew-algebra is an algebra (L, ∨, ∧, ⋅, →, ⊥, ⊤), where
- (L, ∨, ∧, ⊥, ⊤) is a bounded lattice with top element ⊤ and bottom element ⊥
- (L, ⋅, ⊤) is a commutative monoid
- The residuation property holds: x ⋅ y ≤ z iff x ≤ y → z

See also [`FiniteBoundedLattice`](@ref), [`CommutativeMonoid`](@ref).
"""
struct FiniteFLewAlgebra{N} <: FiniteAlgebra{N}
    join::BinaryOperation{N}
    meet::BinaryOperation{N}
    monoid::CommutativeMonoid{N}
    implication::BinaryOperation{N}
    bot::FiniteTruth
    top::FiniteTruth

    function FiniteFLewAlgebra{N}(
        join::BinaryOperation{N},
        meet::BinaryOperation{N},
        monoid::CommutativeMonoid{N},
        bot::T1,
        top::T2
    ) where {
        N,
        T1<:Truth,
        T2<:Truth
    }
        if !isa(bot, FiniteTruth) bot = convert(FiniteTruth, bot) end
        if !isa(top, FiniteTruth) top = convert(FiniteTruth, top) end
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(RightResidual, meet, monoid) "Residuation property does not " *
            "hold for the defined monoid operation."

        implicationtruthtable = Array{FiniteTruth}(undef, N, N)
        for z ∈ UInt8(1):UInt8(N)
            for x ∈ UInt8(1):UInt8(N)
                candidates = Vector{FiniteTruth}()
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
        implication = BinaryOperation{N}(SMatrix{N, N, FiniteTruth}(implicationtruthtable))
        return new{N}(join, meet, monoid, implication, bot, top)
    end

    function FiniteFLewAlgebra{N}(
        join::BinaryOperation{N},
        meet::BinaryOperation{N},
        monoidoperation::BinaryOperation{N},
        bot::T1,
        top::T2
    ) where {
        N,
        T1<:Truth,
        T2<:Truth
    }
        return FiniteFLewAlgebra{N}(
            join,
            meet,
            CommutativeMonoid{N}(monoidoperation, top),
            bot,
            top
        )
    end
end

islattice(::FiniteFLewAlgebra{N}) where {N} = true
isboundedlattice(::FiniteFLewAlgebra{N}) where {N} = true

function Base.show(io::IO, a::FiniteFLewAlgebra{N}) where {N}
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
#### Finite Heyting algebra ################################################################
############################################################################################

"""
    const Implication1

Axiom Implication1 is satisfied if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) with largest and
smallest elements ⊤ and ⊥, and a binary operation →, a → a = ⊤ holds.

See also [`Axiom`](@ref), [`checkaxiom`](@ref).
"""
const Implication1 = Axiom{:I1}()

"""
    function checkaxiom(
        ::typeof(Implication1),
        o::B,
        top::Truth
    ) where {
        N,
        B<:BinaryOperation{N}
    }

Check if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) with largest and smallest elements ⊤ and ⊥,
and a binary operation →, a → a = ⊤ holds.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Implication1),
    o::B,
    top::Truth
) where {
    N,
    B<:BinaryOperation{N}
}
    for i ∈ getdomain(o)
        o(i, i) != top && return false
    end
    return true
end

"""
    const Implication2

Axiom Implication2 is satisfied if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) and two binary
operations ∧ (`o1`) and → (`o2`), a ∧ (a → b) = a ∧ b holds.

See also [`Axiom`](@ref), [`checkaxiom`](@ref).
"""
const Implication2 = Axiom{:I2}()

"""
    function checkaxiom(
        ::typeof(Implication2),
        o1::B,
        o2::B
    ) where {
        N,
        B<:BinaryOperation{N}
    }

Check if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) with largest and smallest elements ⊤ and ⊥,
and two binary operations ∧ (`o1`) and → (`o2`), a ∧ (a → b) = a ∧ b holds.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Implication2),
    o1::B,
    o2::B
) where {
    N,
    B<:BinaryOperation{N}
}
    for i ∈ getdomain(o1)
        for j ∈ getdomain(o1)
            o1(i, o2(i, j)) != o1(i, j) && return false
        end
    end
    return true
end

"""
    const Implication3

Axiom Implication3 is satisfied if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) and two binary
operations ∧ (`o1`) and → (`o2`), b ∧ (a → b) = b holds.

See also [`Axiom`](@ref), [`checkaxiom`](@ref).
"""
const Implication3 = Axiom{:I3}()

"""
    function checkaxiom(
        ::typeof(Implication3),
        o1::B,
        o2::B
    ) where {
        N,
        B<:BinaryOperation{N}
    }

Check if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) with largest and smallest elements ⊤ and ⊥,
and two binary operations ∧ (`o1`) and → (`o2`), b ∧ (a → b) = b holds.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Implication3),
    o1::B,
    o2::B
) where {
    N,
    B<:BinaryOperation{N}
}
    for i ∈ getdomain(o1)
        for j ∈ getdomain(o1)
            o1(j, o2(i, j)) != j && return false
        end
    end
    return true
end

"""
    const DistributiveLaw

Given a bounded lattice (H, ∨, ∧, ⊥, ⊤) and two binary operations ⋅ and *, ⋅ is
distributive over * if ∀ a, b, c ∈ L: a ⋅ (b * c) = (a ⋅ b) * (a ⋅ c).

See also [`Axiom`](@ref), [`checkaxiom`](@ref).
"""
const DistributiveLaw = Axiom{:DL}()

"""
    function checkaxiom(
        ::typeof(DistributiveLaw),
        o1::B,
        o2::B
    ) where {
        N,
        B<:BinaryOperation{N}
    }

Check if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) and two binary operations ⋅ and *, ⋅ is
distributive over * if ∀ a, b, c ∈ L: a ⋅ (b * c) = (a ⋅ b) * (a ⋅ c).

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(DistributiveLaw),
    o1::B,
    o2::B
) where {
    N,
    B<:BinaryOperation{N}
}
    for i ∈ getdomain(o1)
        for j ∈ getdomain(o1)
            for k ∈ getdomain(o1)
                o1(i, o2(j, k)) != o2(o1(i, j), o1(i, k)) && return false
            end
        end
    end
    return true
end

"""
    struct FiniteHeytingAlgebra{N} <: FiniteAlgebra{N}
        join::BinaryOperation{N}
        meet::BinaryOperation{N}
        implication::BinaryOperation{N}
        bot::FiniteTruth
        top::FiniteTruth
    end

A Heyting algebra (H, ∨, ∧, →, ⊥, ⊤) is a bounded lattice (H, ∨, ∧, ⊥, ⊤) equipped with a
binary operation a → b of implication such that (c ∧ a) ≤ b is equivalent to c ≤ (a → b).

Given a bounded lattice (H, ∨, ∧, ⊥, ⊤) with largest and smallest elements ⊤ and ⊥, and a
binary operation →, these together form a Heyting algebra if and only if the following hold:
 - (Implication1) a → a = ⊤
 - (Implication2) a ∧ (a → b) = a ∧ b
 - (Implication3) b ∧ (a → b) = b
 - (Distributive law for →) a → (b ∧ c) = (a → b) ∧ (a → c)

See also [`FiniteBoundedLattice`](@ref), [`BinaryOperation`](@ref).
"""
struct FiniteHeytingAlgebra{N} <: FiniteAlgebra{N}
    join::BinaryOperation{N}
    meet::BinaryOperation{N}
    implication::BinaryOperation{N}
    bot::FiniteTruth
    top::FiniteTruth

    function FiniteHeytingAlgebra{N}(
        join::BinaryOperation{N},
        meet::BinaryOperation{N},
        implication::BinaryOperation{N},
        bot::T1,
        top::T2
    ) where {
        N,
        T1<:Truth,
        T2<:Truth
    }
        if !isa(bot, FiniteTruth) bot = convert(FiniteTruth, bot)::FiniteTruth end
        if !isa(top, FiniteTruth) top = convert(FiniteTruth, top)::FiniteTruth end
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(Implication1, implication, top) "Axiom a → a = ⊤ does not " *
            "hold for given binary operation →."
        @assert checkaxiom(Implication2, meet, implication) "Axiom a ∧ (a → b) = a ∧ b " *
            "does not hold for given binary operation →."
        @assert checkaxiom(Implication3, meet, implication) "Axiom b ∧ (a → b) = b does " *
            "not hold for given binary operation →."
        @assert checkaxiom(DistributiveLaw, implication, meet) "Distributive law for → " *
            "does not hold for given binary operation →."
        return new{N}(join, meet, implication, bot, top)
    end
end

islattice(::FiniteHeytingAlgebra) = true
isboundedlattice(::FiniteHeytingAlgebra) = true

function Base.show(io::IO, a::FiniteHeytingAlgebra)
    println(string(typeof(a)))
    println(io, "Domain: " * string(getdomain(a)))
    println(io, "Bot: " * string(a.bot))
    println(io, "Top: " * string(a.top))
    println(io, "Join: " * string(a.join))
    println(io, "Meet: " * string(a.meet))
    println(io, "Implication: " * string(a.implication))
end

"""
    function convert(
        ::Type{FiniteFLewAlgebra{N}},
        l::FiniteHeytingAlgebra
    ) where {
        N
    }

Convert `l` of type `FiniteHeytingAlgebra` to a value of type `FiniteFLewAlgebra`.

See also [`FiniteFLewAlgebra`](@ref), [`FiniteHeytingAlgebra`](@ref).
"""
function convert(
    ::Type{FiniteFLewAlgebra{N}},
    l::FiniteHeytingAlgebra{N}
) where {
    N
}
    return FiniteFLewAlgebra(l.join, l.meet, l.meet, l.bot, l.top)
end


"""
    function convert(
        ::Type{FiniteHeytingAlgebra{N}},
        l::FiniteFLewAlgebra
    ) where {
        N
    }

Convert `l` of type `FiniteFLewAlgebra` to a value of type `FiniteHeytingAlgebra`.

See also [`FiniteHeytingAlgebra`](@ref), [`FiniteFLewAlgebra`](@ref).
"""
function convert(
    ::Type{FiniteHeytingAlgebra{N}},
    l::FiniteFLewAlgebra{N}
) where {
    N
}
    if (l.meet == l.monoid.operation)
        return FiniteHeytingAlgebra(l.join, l.meet, l.implication, l.bot, l.top)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type " *
              " FiniteFLewAlgebra: meet and monoid must be the same.")
    end
end
