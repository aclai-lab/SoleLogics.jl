using ..SoleLogics: AbstractAlgebra
import ..SoleLogics: syntaxstring, istop, isbot
import Base: convert

############################################################################################
#### Finite truth ##########################################################################
############################################################################################

struct FiniteTruth <: Truth
    label::String

    function FiniteTruth(label::String)
        return new(label)
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
    abstract type FiniteAlgebra{T<:Truth,D<:AbstractVector{T}} <: AbstractAlgebra{T} end

A finite algebra is an algebraic structure defined over a finite set.

See also [`AbstractAlgebra`](@ref).
"""
abstract type FiniteAlgebra{T<:Truth,D<:AbstractVector{T}} <: AbstractAlgebra{T} end

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
struct Monoid{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
    operation::B
    identityelement::T

    function Monoid(
        operation::B,
        identityelement::T
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
    }
        if !isa(identityelement, T) identityelement = convert(T, identityelement)::T end
        checkmonoidaxioms(operation, identityelement)
        return new{T,D,B}(operation, identityelement)
    end
end

"""
Return true if the object can be converted to an object of type `Monoid`.

See also [`Monoid`](@ref).
"""
ismonoid(::Monoid) = true
ismonoid(::T) where {T} = false  

"""
    function convert(
        ::Type{Monoid},
        m::M
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
        M<:FiniteAlgebra{T}
    }

Convert `m` to a value of type `Monoid`.

See also [`Monoid`](@ref), [`ismonoid`](@ref).
"""
function Base.convert(
    ::Type{<:Monoid},
    m::M
) where {
    T<:Truth,
    M<:FiniteAlgebra{T}
}
    if ismonoid(m)
        return Monoid(m.operation, m.identityelement)
    else
        error("Cannot convert object of type $(typeof(m)) to a value of type Monoid with truth values of type $T.")
    end
end

"""
    function checkaxiom(a::Axiom, m::Monoid)

Check if axiom `a` is satisfied by the operation of the monoid `m`.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(a::Axiom, m::Monoid)
    return checkaxiom(typeof(a), m.operation)
end

"""
    function (m::Monoid{T,D})(
        t1::T1,
        t2::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        T1<:Truth,
        T2<:Truth
    }

Helper allowing to use monoids with function notation.

See also [`Monoid`](@ref), [`BinaryOperation`](@ref).
"""
function (m::Monoid{T,D})(
    t1::T1,
    t2::T2
) where {
    T<:Truth,
    D<:AbstractVector{T},
    T1<:Truth,
    T2<:Truth
}
    return m.operation(t1, t2)
end

############################################################################################
#### Commutative monoid ####################################################################
############################################################################################

"""
    struct CommutativeMonoid{T<:Truth,B<:BinaryOperation{T}}
        operation::B
        identityelement::T
    end

A commutative monoid (S, ⋅, e) is a monoid whose operation is commutative.

See also [`Monoid`](@ref), [`Commutativity`](@ref).
"""
struct CommutativeMonoid{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
    operation::B
    identityelement::T

    function CommutativeMonoid(
        operation::B,
        identityelement::T1
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
        T1<:Truth
    }
        if !isa(identityelement, T) identityelement = convert(T, identityelement)::T end
        checkmonoidaxioms(operation, identityelement)
        @assert checkaxiom(Commutativity, operation) "Defined an operation for the " *
            "commutative monoid which is not commutative."
        return new{T,D,B}(operation, identityelement)
    end
end

ismonoid(::CommutativeMonoid) = true

"""
    function(m::CommutativeMonoid{T,D})(
        t1::T1,
        t2::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        T1<:Truth,
        T2<:Truth
    }

Helper allowing to use commutative monoids with function notation.

See also [`Monoid`](@ref), [`BinaryOperation`](@ref).
"""
function (m::CommutativeMonoid{T,D})(
    t1::T1,
    t2::T2
) where {
    T<:Truth,
    D<:AbstractVector{T},
    T1<:Truth,
    T2<:Truth
}
    return m.operation(t1, t2)
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
    struct FiniteLattice{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
        join::B
        meet::B
    end

A finite lattice is a lattice defined over a finite set.

A lattice is an algebraic structure (L, ∨, ∧) consisting of a set L and two binary,
commutative and associative operations ∨ and ∧ on L satisfying the following axiomatic
identities for all elements a, b ∈ L (sometimes called absorption laws):
 - a ∨ (a ∧ b) = a
 - a ∧ (a ∨ b) = a

See also [`FiniteAlgebra`](@ref), [`BinaryOperation`](@ref).
"""
struct FiniteLattice{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
    join::B
    meet::B

    function FiniteLattice(
        join::B,
        meet::B
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D}
    }
        checklatticeaxioms(join, meet)
        return new{T,D,B}(join, meet)
    end
end

"""
Return true if the object can be converted to an object of type `FiniteLattice`.

See also [`FiniteLattice`](@ref).
"""
islattice(::FiniteLattice) = true
islattice(::T) where {T} = false

"""
    function convert(
        ::Type{FiniteLattice},
        l::L
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
        L<:FiniteAlgebra{T}
    }

Convert `l` to a value of type `FiniteLattice`.

See also [`FiniteLattice`](@ref), [`islattice`](@ref).
"""
function convert(
    ::Type{FiniteLattice},
    l::L
) where {
    L<:FiniteAlgebra
}
    if islattice(l)
        return FiniteLattice(l.join, l.meet)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type Lattice.")
    end
end

"""
    function getdomain(a::A) where {T<:Truth,B<:BinaryOperation{T}, L<:FiniteAlgebra{T}}

Return the domain associated to `a`.

See also [`Monoid`](@ref), [`FiniteLattice`](@ref), [`ismonoid`](@ref), [`islattice`](@ref).
"""
function getdomain(a::A) where {T<:Truth,A<:FiniteAlgebra{T}}
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
    struct FiniteBoundedLattice{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
        join::B
        meet::B
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
struct FiniteBoundedLattice{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
    join::B
    meet::B
    bot::T
    top::T

    function FiniteBoundedLattice(
        join::B,
        meet::B,
        bot::T1,
        top::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
        T1<:Truth,
        T2<:Truth
    }
        if !isa(bot, T) bot = convert(T, bot)::T end
        if !isa(top, T) top = convert(T, top)::T end
        checkboundedlatticeaxioms(join, meet, bot, top)
        return new{T,D,B}(join, meet, bot, top)
    end
end

islattice(::FiniteBoundedLattice) = true

"""
Return true if the object can be converted to an object of type `FiniteBoundedLattice`.

See also [`FiniteBoundedLattice`](@ref).
"""
isboundedlattice(::FiniteBoundedLattice) = true
isboundedlattice(::T) where {T} = false

"""
    function convert(
        ::Type{FiniteBoundedLattice},
        l::L
    ) where {
        L<:FiniteAlgebra
    }

Convert `l` to a value of type `FiniteBoundedLattice`.

See also [`FiniteBoundedLattice`](@ref), [`isboundedlattice`](@ref).
"""
function convert(
    ::Type{FiniteBoundedLattice},
    l::L
) where {
    L<:FiniteAlgebra
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
        m::Monoid{T,D,B}
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D}
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
    struct FiniteResiduatedLattice{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
        join::B
        meet::B
        monoid::Monoid{T,D,B}
        rightresidual::B
        leftresidual::B
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
struct FiniteResiduatedLattice{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
    join::B
    meet::B
    monoid::Monoid{T,D,B}
    rightresidual::B
    leftresidual::B
    bot::T
    top::T

    function FiniteResiduatedLattice(
        join::B,
        meet::B,
        monoid::M,
        bot::T1,
        top::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
        M<:FiniteAlgebra{T},
        T1<:Truth,
        T2<:Truth
    }
        if !isa(monoid, Monoid{T,D,B}) monoid = convert(Monoid{T,D,B}, monoid)::Monoid{T,D,B} end
        if !isa(bot, T) bot = convert(T, bot)::T end
        if !isa(top, T) top = convert(T, top)::T end
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(ResiduationProperty, meet, monoid) "Residuation property does " *
            "not hold for the defined monoid operation."
        rrtruthtable = Dict{Tuple{T, T}, T}()
        lrtruthtable = Dict{Tuple{T, T}, T}()
        for z ∈ getdomain(monoid)
            for x ∈ getdomain(monoid)
                candidates = Vector{T}()
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
                candidates = Vector{T}()
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
        return new{T,D,B}(join, meet, monoid, rightresidual, leftresidual, bot, top)
    end
end

############################################################################################
#### Finite FLew algebra ###################################################################
############################################################################################

"""
    struct FiniteFLewAlgebra{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
        join::B
        meet::B
        monoid::Monoid{T,D,B}
        implication::B
        bot::T
        top::T
    end

An FLew-algebra is an algebra (L, ∨, ∧, ⋅, →, ⊥, ⊤), where
- (L, ∨, ∧, ⊥, ⊤) is a bounded lattice with top element ⊤ and bottom element ⊥
- (L, ⋅, ⊤) is a commutative monoid
- The residuation property holds: x ⋅ y ≤ z iff x ≤ y → z

See also [`FiniteBoundedLattice`](@ref), [`CommutativeMonoid`](@ref).
"""
struct FiniteFLewAlgebra{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
    join::B
    meet::B
    monoid::CommutativeMonoid{T,D,B}
    implication::B
    bot::T
    top::T

    function FiniteFLewAlgebra(
        join::B,
        meet::B,
        monoid::CommutativeMonoid{T,D,B},
        bot::T1,
        top::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
        T1<:Truth,
        T2<:Truth
    }
        if !isa(bot, T) bot = convert(T, bot)::T end
        if !isa(top, T) top = convert(T, top)::T end
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(RightResidual, meet, monoid) "Residuation property does not " *
            "hold for the defined monoid operation."
        implicationtruthtable = Dict{Tuple{T, T}, T}()
        for z ∈ getdomain(monoid)
            for x ∈ getdomain(monoid)
                candidates = Vector{T}()
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
        return new{T,D,B}(join, meet, monoid, implication, bot, top)
    end

    function FiniteFLewAlgebra(
        join::B,
        meet::B,
        monoidoperation::B,
        bot::T1,
        top::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
        T1<:Truth,
        T2<:Truth
    }
        return FiniteFLewAlgebra(
            join,
            meet,
            CommutativeMonoid(monoidoperation, top),
            bot,
            top
        )
    end
end

islattice(::FiniteFLewAlgebra) = true
isboundedlattice(::FiniteFLewAlgebra) = true

function Base.show(io::IO, a::FiniteFLewAlgebra)
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
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D}
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
    T<:Truth,
    B<:BinaryOperation{T}
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
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D}
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
    T<:Truth,
    B<:BinaryOperation{T}
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
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D}
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
    T<:Truth,
    B<:BinaryOperation{T}
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
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D}
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
    T<:Truth,
    B<:BinaryOperation{T}
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
    struct FiniteHeytingAlgebra{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
        join::B
        meet::B
        implication::B
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
struct FiniteHeytingAlgebra{T<:Truth,D<:AbstractVector{T},B<:BinaryOperation{T,D}} <: FiniteAlgebra{T,D}
    join::B
    meet::B
    implication::B
    bot::T
    top::T

    function FiniteHeytingAlgebra(
        join::B,
        meet::B,
        implication::B,
        bot::T1,
        top::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        B<:BinaryOperation{T,D},
        T1<:Truth,
        T2<:Truth
    }
        if !isa(bot, T) bot = convert(T, bot)::T end
        if !isa(top, T) top = convert(T, top)::T end
        checkboundedlatticeaxioms(join, meet, bot, top)
        @assert checkaxiom(Implication1, implication, top) "Axiom a → a = ⊤ does not " *
            "hold for given binary operation →."
        @assert checkaxiom(Implication2, meet, implication) "Axiom a ∧ (a → b) = a ∧ b " *
            "does not hold for given binary operation →."
        @assert checkaxiom(Implication3, meet, implication) "Axiom b ∧ (a → b) = b does " *
            "not hold for given binary operation →."
        @assert checkaxiom(DistributiveLaw, implication, meet) "Distributive law for → " *
            "does not hold for given binary operation →."
        return new{T,D,B}(join, meet, implication, bot, top)
    end

    function FiniteHeytingAlgebra(
        a::FiniteFLewAlgebra{T,D}
    ) where {
        T<:Truth,
        D<:AbstractVector{T}
    }
        return convert(FiniteHeytingAlgebra{T,D}, a)
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
        ::Type{FiniteFLewAlgebra{T,D}},
        l::FiniteHeytingAlgebra
    ) where {
        T<:Truth,
        D<:AbstractVector{T}
    }

Convert `l` of type `FiniteHeytingAlgebra` to a value of type `FiniteFLewAlgebra`.

See also [`FiniteFLewAlgebra`](@ref), [`FiniteHeytingAlgebra`](@ref).
"""
function convert(
    ::Type{FiniteFLewAlgebra{T,D}},
    l::FiniteHeytingAlgebra{T,D}
) where {
    T<:Truth,
    D<:AbstractVector{T}
}
    return FiniteFLewAlgebra(l.join, l.meet, l.meet, l.bot, l.top)
end


"""
    function convert(
        ::Type{FiniteHeytingAlgebra{T,D}},
        l::FiniteFLewAlgebra
    ) where {
        T<:Truth,
        D<:AbstractVector{T}
    }

Convert `l` of type `FiniteFLewAlgebra` to a value of type `FiniteHeytingAlgebra`.

See also [`FiniteHeytingAlgebra`](@ref), [`FiniteFLewAlgebra`](@ref).
"""
function convert(
    ::Type{FiniteHeytingAlgebra{T,D}},
    l::FiniteFLewAlgebra{T,D}
) where {
    T<:Truth,
    D<:AbstractVector{T}
}
    if (l.meet == l.monoid.operation)
        return FiniteHeytingAlgebra(l.join, l.meet, l.implication, l.bot, l.top)
    else
        error("Cannot convert object of type $(typeof(l)) to a value of type " *
              " FiniteFLewAlgebra: meet and monoid must be the same.")
    end
end
