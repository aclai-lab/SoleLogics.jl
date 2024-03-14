using ..SoleLogics: AbstractAlgebra
import ..SoleLogics: istop, isbot
import Base: convert

############################################################################################
#### Finite truth ##########################################################################
############################################################################################

struct FiniteTruth <: Truth
    label::String

    function FiniteTruth(label::String)
        return new(label)
    end

    function FiniteTruth(t::BooleanTruth)
        return convert(FiniteTruth, t)
    end 
end

istop(t::FiniteTruth) = t.label == "⊤"
isbot(t::FiniteTruth) = t.label == "⊥"

syntaxstring(t::FiniteTruth; kwargs...) = t.label
Base.show(io::IO, t::FiniteTruth) = print(io, syntaxstring(t))

function Base.convert(::Type{FiniteTruth}, t::BooleanTruth)
    return istop(t) ? FiniteTruth("⊤") : FiniteTruth("⊥")
end

function Base.isequal(t1::FiniteTruth, t2::BooleanTruth)
    return (istop(t1) && istop(t2)) || (isbot(t1) && isbot(t2))
end

############################################################################################
#### Finite algebra ########################################################################
############################################################################################

"""
    abstract type FiniteAlgebra{T<:Truth, D<:AbstractSet{T}} <: AbstractAlgebra{T} end

A finite algebra is an algebraic structure defined over a finite set.

See also [`AbstractAlgebra`](@ref).
"""
abstract type FiniteAlgebra{T<:Truth, D<:AbstractSet{T}} <: AbstractAlgebra{T} end

############################################################################################
#### Monoid ################################################################################
############################################################################################

"""
    function checkmonoidaxioms(
        o::BinaryOperation{T,D},
        e::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check if given domain, operation and identity element form a monoid.

A monoid (S, ⋅, e) is a set S equipped with a binary operation S × S → S, denoted as ⋅,
satisfying the following axiomatic identities:
 - (Associativity) ∀ a, b, c ∈ S, the equation (a ⋅ b) ⋅ c = a ⋅ (b ⋅ c) holds.
 - (Identity element) There exists an element e ∈ L such that for every element a ∈ S, the
   equalities e ⋅ a = a and a ⋅ e = a hold. 

The identity element of a monoid is unique.

See also [`BinaryOperation`](@ref), [`Axiom`](@ref), [`checkaxiom`](@ref),
[`Associativity`](@ref), [`IdentityElement`](@ref).
"""
function checkmonoidaxioms(
    o::BinaryOperation{T,D},
    e::T
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    @assert checkaxiom(Associativity, o) "Defined an operation for the monoid which " *
        "is not associative."
    @assert checkaxiom(IdentityElement, o, e) "$e is not a valid identity element for " *
        "the defined operation."
    return nothing
end

"""
    struct Monoid{T<:Truth, D<:AbstractSet{T}}
        operation::BinaryOperation{T,D}
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
struct Monoid{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    operation::BinaryOperation{T,D}
    identityelement::T

    function Monoid(
        operation::BinaryOperation{T,D},
        identityelement::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        checkmonoidaxioms(operation, identityelement)
        return new{T,D}(operation, identityelement)
    end
end

"""
Return true if the object can be converted to an object of type `Monoid`.

See also [`Monoid`](@ref).
"""
ismonoid(::Monoid{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true

"""
    function convert(
        ::Type{Monoid},
        m::M
    ) where {
        T<:Truth,
        D<:AbstractSet{T},
        M<:FiniteAlgebra{T,D}
    }

Convert `m` to a value of type `Monoid`.

See also [`Monoid`](@ref), [`ismonoid`](@ref).
"""
function convert(
    ::Type{Monoid},
    m::M
) where {
    T<:Truth,
    D<:AbstractSet{T},
    M<:FiniteAlgebra{T,D}
}
    if ismonoid(m)
        return Monoid(m.operation, m.identityelement)
    else
        error("Cannot convert object of type $(typeof(m)) to a value of type Monoid.")
    end
end

"""
    function checkaxiom(a::Axiom, m::Monoid{T,D}) where {T<:Truth, D<:AbstractSet{T}}

Check if axiom `a` is satisfied by the operation of the monoid `m`.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(a::Axiom, m::Monoid{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    return checkaxiom(typeof(a), m.operation)
end

"""
    function (m::Monoid{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractSet{T}}

Helper allowing to use monoids with function notation.

See also [`Monoid`](@ref), [`BinaryOperation`](@ref).
"""
(m::Monoid{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractSet{T}} = m.operation(t1, t2)

############################################################################################
#### Commutative monoid ####################################################################
############################################################################################

"""
    struct CommutativeMonoid{T<:Truth, D<:AbstractSet{T}}
        operation::BinaryOperation{T,D}
        identityelement::T
    end

A commutative monoid (S, ⋅, e) is a monoid whose operation is commutative.

See also [`Monoid`](@ref), [`Commutativity`](@ref).
"""
struct CommutativeMonoid{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    operation::BinaryOperation{T,D}
    identityelement::T

    function CommutativeMonoid(
        operation::BinaryOperation{T,D},
        identityelement
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        if identityelement isa T
            e = identityelement
        else
            e = convert(T, identityelement)::T
            if !(isequal(e, identityelement)::Bool)
                throw(ArgumentError("$(limitrepr(identityelement)) is not a valid key for type $T"))
            end
        end
        checkmonoidaxioms(operation, e)
        @assert checkaxiom(Commutativity, operation) "Defined an operation for the " *
            "commutative monoid which is not commutative."
        return new{T,D}(operation, e)
    end
end

ismonoid(::CommutativeMonoid{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true

"""
    function (m::CommutativeMonoid{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractSet{T}}

Helper allowing to use commutative monoids with function notation.

See also [`Monoid`](@ref), [`BinaryOperation`](@ref).
"""
function (m::CommutativeMonoid{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractSet{T}}
    return m.operation(t1, t2)
end

############################################################################################
#### Finite lattice ########################################################################
############################################################################################

"""
    function checklatticeaxioms(
        join::BinaryOperation{T,D},
        meet::BinaryOperation{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check if given domain, join and meet form a finite lattice.

A lattice is an algebraic structure (L, ∨, ∧) consisting of a set L and two binary,
commutative and associative operations ∨ and ∧ on L satisfying the following axiomatic
identities for all elements a, b ∈ L (sometimes called absorption laws):
- a ∨ (a ∧ b) = a
- a ∧ (a ∨ b) = a

See also [`FiniteLattice`](@ref), [`BinaryOperation`](@ref), [`Commutativity`](@ref),
[`Associativity`](@ref), [`AbsorptionLaw`](@ref),
"""
function checklatticeaxioms(
    join::BinaryOperation{T,D},
    meet::BinaryOperation{T,D}
) where {
    T<:Truth,
    D<:AbstractSet{T}
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
    struct FiniteLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
        join::BinaryOperation{T,D}
        meet::BinaryOperation{T,D}
    end

A finite lattice is a lattice defined over a finite set.

A lattice is an algebraic structure (L, ∨, ∧) consisting of a set L and two binary,
commutative and associative operations ∨ and ∧ on L satisfying the following axiomatic
identities for all elements a, b ∈ L (sometimes called absorption laws):
 - a ∨ (a ∧ b) = a
 - a ∧ (a ∨ b) = a

See also [`FiniteAlgebra`](@ref), [`BinaryOperation`](@ref).
"""
struct FiniteLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}

    function FiniteLattice(
        join::BinaryOperation{T,D},
        meet::BinaryOperation{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        checklatticeaxioms(join, meet)
        return new{T,D}(join, meet)
    end
end

"""
Return true if the object can be converted to an object of type `FiniteLattice`.

See also [`FiniteLattice`](@ref).
"""
islattice(::FiniteLattice{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true

"""
    function convert(
        ::Type{FiniteLattice},
        l::L
    ) where {
        T<:Truth,
        D<:AbstractSet{T},
        L<:FiniteAlgebra{T,D}
    }

Convert `l` to a value of type `FiniteLattice`.

See also [`FiniteLattice`](@ref), [`islattice`](@ref).
"""
function convert(
    ::Type{FiniteLattice},
    l::L
) where {
    T<:Truth,
    D<:AbstractSet{T},
    L<:FiniteAlgebra{T,D}
}
    if islattice(l)
        return FiniteLattice(l.join, l.meet)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type Lattice.")
    end
end

"""
    function getdomain(a::A) where {T<:Truth, D<:AbstractSet{T}, L<:FiniteAlgebra{T,D}}

Return the domain associated to `a`.

See also [`Monoid`](@ref), [`FiniteLattice`](@ref), [`ismonoid`](@ref), [`islattice`](@ref).
"""
function getdomain(a::A) where {T<:Truth, D<:AbstractSet{T}, A<:FiniteAlgebra{T,D}}
    if ismonoid(a)
        return getdomain(a.operation)
    elseif islattice(a)
        return getdomain(a.join)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type Monoid.")
    end
end

############################################################################################
#### Finite bounded lattice ################################################################
############################################################################################

"""
    function checkboundedlatticeaxioms(
        join::BinaryOperation{T,D},
        meet::BinaryOperation{T,D},
        bot::T,
        top::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
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
    join::BinaryOperation{T,D},
    meet::BinaryOperation{T,D},
    bot::T,
    top::T
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    checklatticeaxioms(join, meet)
    @assert checkaxiom(IdentityElement, join, bot) "$bot is not a valid identity element " *
        "for the defined join operation."
    @assert checkaxiom(IdentityElement, meet, top) "$top is not a valid identity element " *
        "for the defined meet operation."
    return nothing
end

"""
    struct FiniteBoundedLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
        join::BinaryOperation{T,D}
        meet::BinaryOperation{T,D}
        bot::T
        top::T
    end

A finite bounded lattice is a bounded lattice defined over a finite set.

A bounded lattice is an algebraic structure (L, ∨, ∧, ⊥, ⊤) such that (L, ∨, ∧) is a
lattice, the bottom element ⊥ is the identity element for the join operation ∨, and the top
element ⊤ is the identity element for the meet operation ∧:
 - a ∨ ⊥ = a
 - a ∧ ⊤ = a

See also [`FiniteLattice`](@ref).
"""
struct FiniteBoundedLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}
    bot::T
    top::T

    function FiniteBoundedLattice(
        join::BinaryOperation{T,D},
        meet::BinaryOperation{T,D},
        bot::T,
        top::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        checkboundedlatticeaxioms(join, meet, bot, top)
        return new{T,D}(join, meet, bot, top)
    end
end

islattice(::FiniteBoundedLattice{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true

"""
Return true if the object can be converted to an object of type `FiniteBoundedLattice`.

See also [`FiniteBoundedLattice`](@ref).
"""
isboundedlattice(::FiniteBoundedLattice{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true

"""
    function convert(
        ::Type{FiniteBoundedLattice},
        l::L
    ) where {
        T<:Truth,
        D<:AbstractSet{T},
        L<:FiniteAlgebra{T,D}
    }

Convert `l` to a value of type `FiniteBoundedLattice`.

See also [`FiniteBoundedLattice`](@ref), [`isboundedlattice`](@ref).
"""
function convert(
    ::Type{FiniteBoundedLattice},
    l::L
) where {
    T<:Truth,
    D<:AbstractSet{T},
    L<:FiniteAlgebra{T,D}
}
    if isboundedlattice(l)
        return FiniteBoundedLattice(l.join, l.meet, l.bot, l.top)
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
        m::Monoid{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check that ∀ x ∈ S there exists for every x ∈ S a greatest y ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(
    ::typeof(RightResidual),
    meet::BinaryOperation{T,D},
    monoid::M
) where {
    T<:Truth,
    D<:AbstractSet{T},
    M<:FiniteAlgebra{T,D}
}
    !ismonoid(monoid) && error("Cannot convert an object of type $(typeof(monoid)) to an "*
        "object of type Monoid.")
    for z ∈ getdomain(monoid)
        for x ∈ getdomain(monoid)
            candidates = Set{T}()
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
        m::Monoid{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check that ∀ x ∈ S there exists for every y ∈ S a greatest x ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(
    ::typeof(LeftResidual),
    meet::BinaryOperation{T,D},
    monoid::M
) where {
    T<:Truth,
    D<:AbstractSet{T},
    M<:FiniteAlgebra{T,D}
}
    !ismonoid(monoid) && error("Cannot convert an object of type $(typeof(monoid)) to an "*
        "object of type Monoid.")
    for z ∈ getdomain(monoid)
        for y ∈ getdomain(monoid)
            candidates = Set{T}()
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
        m::Monoid{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check that ∀ x ∈ S there exists for every x ∈ S a greatest y ∈ S and for every y ∈ S a
greatest x ∈ S such that x ⋅ y ≤ z.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(
    ::typeof(ResiduationProperty),
    meet::BinaryOperation{T,D},
    monoid::M
) where {
    T<:Truth,
    D<:AbstractSet{T},
    M<:FiniteAlgebra{T,D}
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
    struct FiniteResiduatedLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
        domain::D
        join::BinaryOperation{T,D}
        meet::BinaryOperation{T,D}
        monoid::Monoid{T,D}
        rightresidual::BinaryOperation{T,D}
        leftresidual::BinaryOperation{T,D}
        bot::T
        top::T
    end

A residuated lattice is an algebraic structure L = (L, ∨, ∧, ⋅, e) such that:
 - (L, ∨, ∧) is a lattice
 - (L, ⋅, e) is a monoid
 - ∀ x ∈ L there exists for every x ∈ L a greatest y ∈ L and for every y ∈ L a greatest
   x ∈ L such that x ⋅ y ≤ z

See also [`FiniteBoundedLattice`](@ref), 
"""
struct FiniteResiduatedLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    domain::D
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}
    monoid::Monoid{T,D}
    rightresidual::BinaryOperation{T,D}
    leftresidual::BinaryOperation{T,D}
    bot::T
    top::T

    function FiniteResiduatedLattice(
        join::BinaryOperation{T,D},
        meet::BinaryOperation{T,D},
        monoid::Monoid{T,D},
        bot::T,
        top::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(ResiduationProperty, meet, monoid) "Residuation property does " *
            "not hold for the defined monoid operation."
        rrtruthtable = Dict{Tuple{T, T}, T}()
        lrtruthtable = Dict{Tuple{T, T}, T}()
        for z ∈ getdomain(monoid)
            for x ∈ getdomain(monoid)
                candidates = Set{T}()
                for y ∈ getdomain(monoid)
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
                        rrtruthtable[(x,z)] = y
                        break
                    end
                end
            end
            for y ∈ getdomain(monoid)
                candidates = Set{T}()
                for x ∈ getdomain(monoid)
                    meet(monoid(x, y), z) == monoid(x, y) && push!(candidates, y)
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
                        lrtruthtable[(y,z)] = x
                        break
                    end
                end
            end
        end
        rightresidual = BinaryOperation(getdomain(monoid), rrtruthtable)
        leftresidual = BinaryOperation(getdomain(monoid), lrtruthtable)
        return new{T,D}(join, meet, monoid, rightresidual, leftresidual, bot, top)
    end
end

############################################################################################
#### Finite FLew algebra ###################################################################
############################################################################################

"""
    struct FiniteFLewAlgebra{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
        domain::D
        join::BinaryOperation{T,D}
        meet::BinaryOperation{T,D}
        monoid::Monoid{T,D}
        implication::BinaryOperation{T,D}
        bot::T
        top::T
    end

An FLew-algebra is an algebra (L, ∨, ∧, ⋅, →, ⊥, ⊤), where
- (L, ∨, ∧, ⊥, ⊤) is a bounded lattice with top element ⊤ and bottom element ⊥
- (L, ⋅, ⊤) is a commutative monoid
- The residuation property holds: x ⋅ y ≤ z iff x ≤ y → z

See also [`FiniteBoundedLattice`](@ref), [`CommutativeMonoid`](@ref).
"""
struct FiniteFLewAlgebra{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}
    monoid::CommutativeMonoid{T,D}
    implication::BinaryOperation{T,D}
    bot::T
    top::T

    function FiniteFLewAlgebra(
        join::BinaryOperation{T,D},
        meet::BinaryOperation{T,D},
        monoid::CommutativeMonoid{T,D},
        bot,
        top
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        if bot isa T
            b = bot
        else
            b = convert(T, bot)::T
            if !(isequal(b, bot)::Bool)
                throw(ArgumentError("$(limitrepr(bot)) is not a valid key for type $T"))
            end
        end
        if top isa T
            t = top
        else
            t = convert(T, top)::T
            if !(isequal(t, top)::Bool)
                throw(ArgumentError("$(limitrepr(top)) is not a valid key for type $T"))
            end
        end
        checkboundedlatticeaxioms(join, meet, b, t)
        @assert checkaxiom(RightResidual, meet, monoid) "Residuation property does not " *
            "hold for the defined monoid operation."
        implicationtruthtable = Dict{Tuple{T, T}, T}()
        for z ∈ getdomain(monoid)
            for x ∈ getdomain(monoid)
                candidates = Set{T}()
                for y ∈ getdomain(monoid)
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
                        implicationtruthtable[(x,z)] = y
                        break
                    end
                end
            end
        end
        implication = BinaryOperation(getdomain(monoid), implicationtruthtable)
        return new{T,D}(join, meet, monoid, implication, b, t)
    end
end

islattice(::FiniteFLewAlgebra{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true
isboundedlattice(::FiniteFLewAlgebra{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true

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
        o::BinaryOperation{T,D},
        top::Truth
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) with largest and smallest elements ⊤ and ⊥,
and a binary operation →, a → a = ⊤ holds.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Implication1),
    o::BinaryOperation{T,D},
    top::Truth
) where {
    T<:Truth,
    D<:AbstractSet{T}
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
        o1::BinaryOperation{T,D},
        o2::BinaryOperation{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) with largest and smallest elements ⊤ and ⊥,
and two binary operations ∧ (`o1`) and → (`o2`), a ∧ (a → b) = a ∧ b holds.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Implication2),
    o1::BinaryOperation{T,D},
    o2::BinaryOperation{T,D}
) where {
    T<:Truth,
    D<:AbstractSet{T}
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
        o1::BinaryOperation{T,D},
        o2::BinaryOperation{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) with largest and smallest elements ⊤ and ⊥,
and two binary operations ∧ (`o1`) and → (`o2`), b ∧ (a → b) = b holds.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Implication3),
    o1::BinaryOperation{T,D},
    o2::BinaryOperation{T,D}
) where {
    T<:Truth,
    D<:AbstractSet{T}
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
        o1::BinaryOperation{T,D},
        o2::BinaryOperation{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Check if given a bounded lattice (H, ∨, ∧, ⊥, ⊤) and two binary operations ⋅ and *, ⋅ is
distributive over * if ∀ a, b, c ∈ L: a ⋅ (b * c) = (a ⋅ b) * (a ⋅ c).

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(DistributiveLaw),
    o1::BinaryOperation{T,D},
    o2::BinaryOperation{T,D}
) where {
    T<:Truth,
    D<:AbstractSet{T}
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
    struct FiniteHeytingAlgebra{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
        domain::D
        join::BinaryOperation{T,D}
        meet::BinaryOperation{T,D}
        implication::BinaryOperation{T,D}
        bot::T
        top::T
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
struct FiniteHeytingAlgebra{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}
    implication::BinaryOperation{T,D}
    bot::T
    top::T

    function FiniteHeytingAlgebra(
        join::BinaryOperation{T,D},
        meet::BinaryOperation{T,D},
        implication::BinaryOperation{T,D},
        bot::T,
        top::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(Implication1, implication, top) "Axiom a → a = ⊤ does not " *
            "hold for given binary operation →."
        @assert checkaxiom(Implication2, meet, implication) "Axiom a ∧ (a → b) = a ∧ b " *
            "does not hold for given binary operation →."
        @assert checkaxiom(Implication3, meet, implication) "Axiom b ∧ (a → b) = b does " *
            "not hold for given binary operation →."
        @assert checkaxiom(DistributiveLaw, implication, meet) "Distributive law for → " *
            "does not hold for given binary operation →."
        return new{T,D}(join, meet, implication, bot, top)
    end
end

islattice(::FiniteHeytingAlgebra{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true
isboundedlattice(::FiniteHeytingAlgebra{T,D}) where {T<:Truth, D<:AbstractSet{T}} = true

"""
    function convert(
        ::Type{FiniteFLewAlgebra{T,D}},
        l::FiniteHeytingAlgebra
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Convert `l` of type `FiniteHeytingAlgebra` to a value of type `FiniteFLewAlgebra`.

See also [`FiniteFLewAlgebra`](@ref), [`FiniteHeytingAlgebra`](@ref).
"""
function convert(
    ::Type{FiniteFLewAlgebra{T,D}},
    l::FiniteHeytingAlgebra
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    return FiniteFLewAlgebra(l.join, l.meet, l.meet, l.bot, l.top)
end
