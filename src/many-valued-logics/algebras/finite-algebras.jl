using ..SoleLogics: AbstractAlgebra

############################################################################################
#### Operation #############################################################################
############################################################################################

"""
    abstract type Operation end

An operation is a function which takes zero or more operands to a well-defined output value.

See also [`BinaryOperation`](@ref), [`arity`](@ref).
"""
abstract type Operation end

"""
    function Base.show(io::IO, o::O) where {O<:Operation}

Write a text representation of an operation `o` to the output stream `io`.

See also [`Operation`](@ref), [`BinaryOperation`](@ref), [`arity`](@ref).
"""
function Base.show(io::IO, o::O) where {O<:Operation}
    print(io, "$(typeof(o)) without a show function")
    @warn "Please, provide a show function for operation $(typeof(o))."
end

"""
    function arity(o::O) where {O<:Operation}

Return the arity of an operation `o`.

See also [`Operation`](@ref), [`BinaryOperation`](@ref), [`arity`](@ref).
"""
function arity(o::O) where {O<:Operation}
    error("Please, provide an arity for operation $o.")
end

"""
    struct BinaryOperation{T<:Truth, D<:AbstractSet{T}} <: Operation
        domain::D
        truthtable::AbstractDict{Tuple{T, T}, T}
    end

A binary operation on a set S is a mapping of the elements of the Cartesian product
S × S → S. The closure property of a binary operation expresses the existence of a result
for the operation given any pair of operands. Binary operations are required to be defined
on all elements of S × S.

See also [`Operation`](@ref), [`arity`](@ref).
"""
struct BinaryOperation{T<:Truth, D<:AbstractSet{T}} <: Operation
    domain::D
    truthtable::AbstractDict{Tuple{T, T}, T}

    function BinaryOperation(
        domain::D,
        truthtable::Dict{Tuple{T, T}, T}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        for i ∈ domain
            for j ∈ domain
                @assert (i, j) ∈ keys(truthtable) "truthtable[($i, $j)] is not defined."
            end
        end
        @assert length(truthtable) == length(domain)^2 "Found truthtable[(i, j)] where i " *
            "or j ∉ domain."
        return new{T,D}(domain, truthtable)
    end

    function BinaryOperation(
        domain::D,
        operation::F
    ) where {
        T<:Truth,
        D<:AbstractSet{T},
        F<:Function
    }
        truthtable = Dict{Tuple{T, T}, T}()
        for i ∈ domain
            for j ∈ domain
                truthtable[(i, j)] = operation(i, j)
            end
        end
        return BinaryOperation(domain, truthtable)
    end
end

Base.show(io::IO, o::BinaryOperation) = print(io, "$(o.truthtable)")
arity(o::BinaryOperation) = 2

"""
    function (o::BinaryOperation{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractSet{T}}

Helper allowing to use binary operations with function notation.

See also [`Operation`](@ref), [`BinaryOperation`](@ref), [`arity`](@ref).
"""
function (o::BinaryOperation{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractSet{T}}
    return o.truthtable[(t1, t2)]
end

############################################################################################
#### Axiom #################################################################################
############################################################################################

"""
    struct Axiom{Symbol} end

An axiom is a statement that is taken to be true, to serve as a premise or starting point
for further reasoning and arguments. Axioms aim to capture what is special about
a particular structure (or set of structures).

See also [`checkaxiom`](@ref).
"""
struct Axiom{Symbol} end

"""
    function checkaxiom(a::Axiom, args...)

Checks if axiom `a` is satisfied.

See also [`Axiom`](@ref).
"""
function checkaxiom(a::Axiom, args...)
    error("Please, provide a checkaxiom method for axiom $a.")
end

"""
    function Base.show(io::IO, a::Axiom)

Write a text representation of an axiom to the output stream `io`.

See also [`Axiom`](@ref).
"""
function Base.show(io::IO, ::Axiom{S}) where {S}
    print(io, string(S))
end

"""
    const Commutativity

A binary operation * on a set S is called commutative if x * y = y * x ∀ x, y ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref), [`checkaxiom`](@ref).
"""
const Commutativity = Axiom{:COM}()

"""
    function checkaxiom(
        ::typeof(Commutativity),
        o::BinaryOperation{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

A binary operation * on a set S is called commutative if x * y = y * x ∀ x, y ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Commutativity),
    o::BinaryOperation{T,D}
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    for i ∈ o.domain
        for j ∈ o.domain
            o(i, j) != o(j, i) && return false
        end
    end
    return true
end

# Helper
function iscommutative(o::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    return checkaxiom(Commutativity, o)
end

"""
    const Associativity

A binary operation * on a set S is called associative if it satisfies the associative law:
(x * y) * z = x * (y * z) ∀ x, y, z ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref), [`checkaxiom`](@ref).
"""
const Associativity = Axiom{:ASS}()

"""
    function checkaxiom(
        ::typeof(Associativity),
        o::BinaryOperation{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

A binary operation * on a set S is called associative if it satisfies the associative law:
(x * y) * z = x * (y * z) ∀ x, y, z ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Associativity),
    o::BinaryOperation{T,D}
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    for i ∈ o.domain
        for j ∈ o.domain
            for k ∈ o.domain
                o(o(i, j), k) != o(i, o(j, k)) && return false
            end
        end
    end
    return true
end

# Helper
function isassociative(o::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    checkaxiom(Associativity, o)
end

"""
    const AbsorptionLaw

The absorption law or absorption identity is an identity linking a pair of binary
operations. Two binary operations, * and ⋅, are said to be connected by the absorption law
if a * (a ⋅ b) = a ⋅ (a * b) = a.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref), [`checkaxiom`](@ref).
"""
const AbsorptionLaw = Axiom{:AL}()

"""
    function checkaxiom(
        ::typeof(AbsorptionLaw),
        o1::BinaryOperation{T,D},
        o2::BinaryOperation{T,D}
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

The absorption law or absorption identity is an identity linking a pair of binary
operations. Two binary operations, * and ⋅, are said to be connected by the absotprion law
if a * (a ⋅ b) = a ⋅ (a * b) = a.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(AbsorptionLaw),
    o1::BinaryOperation{T,D},
    o2::BinaryOperation{T,D}
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    for i ∈ o1.domain
        for j ∈ o1.domain
            o1(i, o2(i, j)) != i && return false
            o2(i, o1(i, j)) != i && return false
        end
    end
    return true
end

"""
    const LeftIdentity

Let (S, *) be a set S equipped with a binary operation *. Then an element e of S is called a
left identity if e * s = s ∀ s ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref), [`checkaxiom`](@ref).
"""
const LeftIdentity = Axiom{:LI}()

"""
    function checkaxiom(
        ::typeof(LeftIdentity),
        o::BinaryOperation{T,D},
        e::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Let (S, *) be a set S equipped with a binary operation *. Then an element e of S is called a
left identity if e * s = s ∀ s ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(LeftIdentity),
    o::BinaryOperation{T,D},
    e::T
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    for i ∈ o.domain
        o(e, i) != i && return false
    end
    return true
end

"""
    const RightIdentity

Let (S, *) be a set S equipped with a binary operation *. Then an element e of S is called a
right identity if s * e = s ∀ s ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref), [`checkaxiom`](@ref).
"""
const RightIdentity = Axiom{:RI}()

"""
    function checkaxiom(
        ::typeof(RightIdentity),
        o::BinaryOperation{T,D},
        e::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

Let (S, *) be a set S equipped with a binary operation *. Then an element e of S is called a
right identity if s * e = s ∀ s ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(RightIdentity),
    o::BinaryOperation{T,D},
    e::T
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    for i ∈ o.domain
        o(i, e) != i && return false
    end
    return true
end

"""
    const IdentityElement

An identity element or neutral element of a binary operation is an element that leaves
unchanged every element when the operation is applied. I.e., let (S, *) be a set S equipped
with a binary operation *. Then an element e of S is called a two-sided identity, or simply
identity, if e is both a left identity and a right identity.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref), [`LeftIdentity`](@ref),
[`RightIdentity`](@ref), [`checkaxiom`](@ref).
"""
const IdentityElement = Axiom{:IE}()

"""
    function checkaxiom(
        ::typeof(IdentityElement),
        o::BinaryOperation{T,D},
        e::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

An identity element or neutral element of a binary operation is an element that leaves
unchanged every element when the operation is applied. I.e., let (S, *) be a set S equipped
with a binary operation *. Then an element e of S is called a two-sided identity, or simply
identity, if e is both a left identity and a right identity.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref), [`LeftIdentity`](@ref),
[`RightIdentity`](@ref).
"""
function checkaxiom(
    ::typeof(IdentityElement),
    o::BinaryOperation{T,D},
    e::T
) where {
    T<:Truth,
    D<:AbstractSet{T}
}
    if checkaxiom(LeftIdentity, o, e) && checkaxiom(RightIdentity, o, e)
        return true
    else
        return false
    end
end

############################################################################################
#### Monoid ################################################################################
############################################################################################

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
struct Monoid{T<:Truth, D<:AbstractSet{T}}
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
    function checkmonoidaxioms(
        o::BinaryOperation{T,D},
        e::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }

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
    function checkaxiom(a::Axiom, m::Monoid{T,D}) where {T<:Truth, D<:AbstractSet{T}}

Checks if axiom `a` is satisfied by the operation of the monoid `m`.

See also [`Axiom`](@ref), [`Monoid`](@ref).
"""
function checkaxiom(a::Axiom, m::Monoid{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    return checkaxiom(typeof(a), m.operation)
end

"""
    struct CommutativeMonoid{T<:Truth, D<:AbstractSet{T}}
        operation::BinaryOperation{T,D}
        identityelement::T
    end

A commutative monoid (S, ⋅, e) is a monoid whose operation is commutative.

See also [`Monoid`](@ref), [`Commutativity`](@ref).
"""
struct CommutativeMonoid{T<:Truth, D<:AbstractSet{T}}
    operation::BinaryOperation{T,D}
    identityelement::T

    function CommutativeMonoid(
        operation::BinaryOperation{T,D},
        identityelement::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        checkmonoidaxioms(operation, identityelement)
        checkaxiom(Commutativity, operation)
        return new{T,D}(operation, identityelement)
    end
end

############################################################################################
#### Finite algebra ########################################################################
############################################################################################

"""
    abstract type FiniteAlgebra{T<:Truth, D<:AbstractSet{T}} <: AbstractAlgebra{T} end

A finite algebra is an algebraic structure defined on a finite set.

See also [`AbstractAlgebra`](@ref).
"""
abstract type FiniteAlgebra{T<:Truth, D<:AbstractSet{T}} <: AbstractAlgebra{T} end

"""
    FiniteLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
        domain::D
        join::BinaryOperation{T,D}
        meet::BinaryOperation{T,D}
    end

A lattice is an algebraic structure (L, ∨, ∧) consisting of a set L and two binary,
commutative and associative operations ∨ and ∧ on L satisfying the following axiomatic
identities for all elements a, b ∈ L (sometimes called absorption laws):
 - a ∨ (a ∧ b) = a
 - a ∧ (a ∨ b) = a

The following two identities are also usally regarded as axioms, even though they follow
from the two absorption laws taken together. These are called idempotent laws:
 - a ∨ a = a
 - a ∧ a = a
"""
struct FiniteLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    domain::D
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}

    function FiniteLattice(domain::D, join::BinaryOperation{T,D}, meet::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
        checklatticeaxioms(join, meet)
        return new{T,D}(domain, join, meet)
    end
end

function checklatticeaxioms(join::BinaryOperation{T,D}, meet::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    @assert iscommutative(join) "Defined a join operation which is not commutative."
    @assert isassociative(join) "Defined a join operation which is not associative."
    @assert iscommutative(meet) "Defined a meet operation which is not commutative." 
    @assert isassociative(meet) "Defined a meet operation which is not associative."
    @assert checkaxiom(AbsorptionLaw, join, meet) "Absorption law doesn't hold between join and meet"
    @assert checkaxiom(AbsorptionLaw, meet, join) "Absorption law doesn't hold between meet and join"
    return nothing
end

"""
    struct FiniteBoundedLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
        domain::D
        join::BinaryOperation{T,D}
        meet::BinaryOperation{T,D}
        bot::T
        top::T
    end

A bounded lattice is an algebraic structure (L, ∨, ∧, ⊥, ⊤) such that (L, ∨, ∧) is a
lattice, the bottom element ⊥ is the identity element for the join operation ∨, and the top
element ⊤ is the identity element for the meet operation ∧:
 - a ∨ ⊥ = a
 - a ∧ ⊤ = a
"""
struct FiniteBoundedLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    domain::D
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}
    bot::T
    top::T

    function FiniteBoundedLattice(domain::D, join::BinaryOperation{T,D}, meet::BinaryOperation{T,D}, bot::T, top::T) where {T<:Truth, D<:AbstractSet{T}}
        checkboundedlatticeaxioms(join, meet, bot, top)
        return new{T,D}(domain, join, meet, bot, top)
    end
end

"""
Checks if given domain, join, meet, bot and top form a bounded lattice.
If not, returns an error.

A bounded lattice is an algebraic structure (L, ∨, ∧, ⊥, ⊤) such that (L, ∨, ∧) is a
lattice, the bottom element ⊥ is the identity element for the join operation ∨, and the top
element ⊤ is the identity element for the meet operation ∧:
 - a ∨ ⊥ = a
 - a ∧ ⊤ = a

See also [`FiniteBoundedLattice`](@ref), [`checklatticeaxioms`](@ref).
"""
function checkboundedlatticeaxioms(join::BinaryOperation{T,D}, meet::BinaryOperation{T,D}, bot::T, top::T) where {T<:Truth, D<:AbstractSet{T}}
    checklatticeaxioms(join, meet)
    checkaxiom(IdentityElement, join, bot)
    checkaxiom(IdentityElement, meet, top)
    return nothing
end

const RightResidual = Axiom{:RR}()

function checkaxiom(::typeof(RightResidual), a::A) where {T<:Truth, D<:AbstractSet{T}, A<:FiniteAlgebra{T,D}}
    @assert islattice(a) "Trying to check an axiom for a lattice over something which is not a lattice"
    for z ∈ m.domain
        for x ∈ m.domain
            candidates = Set{T}()
            for y ∈ m.domain
                if precedes(a, m.operation(x, y), z)
                    candidate = true
                    for i ∈ m.domain
                        if !precedes(a, i, z)
                            candidate = false
                            break
                        end
                    end
                    candidate && push!(candidates, y)
                end
            end
            if length(candidates) != 1
                return false
            end            
        end
    end
end

const LeftResidual = Axiom{:LR}()

function checkaxiom(::typeof(LeftResidual), a::A) where {T<:Truth, D<:AbstractSet{T}, A<:FiniteAlgebra{T,D}}
    @assert islattice(a) "Trying to check an axiom for a lattice over something which is not a lattice"
    for z ∈ m.domain
        for y ∈ m.domain
            candidates = Set{T}()
            for x ∈ m.domain
                if precedes(a, m.operation(x, y), z)
                    candidate = true
                    for i ∈ m.domain
                        if !precedes(a, i, z)
                            candidate = false
                            break
                        end
                    end
                    candidate && push!(candidates, x)
                end
            end
            if length(candidates) != 1
                return false
            end            
        end
    end
end

struct FiniteFLewAlgebra{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    domain::D
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}
    star::Monoid{T,D}
    implication::BinaryOperation{T,D}
    bot::T
    top::T

    function FiniteFLewAlgebra(domain::D, join::BinaryOperation{T,D}, meet::BinaryOperation{T,D}, monoid::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
        @assert iscommutative(join) "Defined a join operation which is not commutative."
        @assert isassociative(join) "Defined a join operation which is not associative."
        @assert iscommutative(meet) "Defined a meet operation which is not commutative." 
        @assert isassociative(meet) "Defined a meet operation which is not associative."
        @assert checkaxiom(AbsorptionLaw, join, meet) "Absorption law doesn't hold between join and meet"
        @assert checkaxiom(AbsorptionLaw, meet, join) "Absorption law doesn't hold between meet and join"
        @assert iscommutative(monoid) "Defined a monoid operation which is not commutative"
        @assert checkaxiom(RightResidual, star) "Residuation property does not hold for the defined monoid operation."
        return new{T,D}(domain, join, meet, star)
    end
end

convert(::Type{FiniteLattice}, l::FiniteFLewAlgebra) = FiniteLattice(domain, join, meet)

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

Given a bounded lattice A with largest and smallest elements ⊤ and ⊥, and a binary operation
→, these together form a Heyting algebra if and only if the following hold:
 - a → a = 1
 - a ∧ (a → b) = a ∧ b
 - b ∧ (a → b) = b
 - (Distributive law for →) a → (b ∧ c) = (a → b) ∧ (a → c)
"""
struct FiniteHeytingAlgebra{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    domain::D
    join::BinaryOperation{T,D}
    meet::BinaryOperation{T,D}
    implication::BinaryOperation{T,D}
    bot::T
    top::T

    function FiniteHeytingAlgebra(domain::D,
        join::BinaryOperation{T,D},
        meet::BinaryOperation{T,D},
        implication::BinaryOperation{T,D},
        bot::T,
        top::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T}
    }
        isboundedlattice(join, meet, bot, top)
        @assert checkaxiom(Implication1, implication, top) "Axiom a → a = ⊤ does not hold for given binary operation →."
        @assert checkaxiom(Implication2, meet, implication) "Axiom a ∧ (a → b) = a ∧ b does not hold for given binary operation →."
        @assert checkaxiom(Implication3, meet, implication) "Axiom b ∧ (a → b) = b does not hold for given binary operation →."
        @assert checkaxiom(DistributiveLaw, implication, meet) "Distributive law for → does not hold for given binary operation →."
        return new{T,D}(domain, join, meet, implication, bot, top)
    end
end

const Implication1 = Axiom{:I1}()

function checkaxiom(::typeof(Implication1), o::BinaryOperation{T,D}, top::Truth) where {T<:Truth, D<:AbstractSet{T}}
    for i ∈ o.domain
        o(i, i) == top && return false
    end
    return true
end

const Implication2 = Axiom{:I2}()

function checkaxiom(::typeof(Implication2), o1::BinaryOperation{T,D}, o2::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    for i ∈ o.domain
        for j ∈ o.domain
            o1(i, o2(i, j)) == o1(i, j) && return false
        end
    end
    return true
end

const Implication3 = Axiom{:I3}()

function checkaxiom(::typeof(Implication3), o1::BinaryOperation{T,D}, o2::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    for i ∈ o.domain
        for j ∈ o.domain
            o1(j, o2(i, j)) == j && return false
        end
    end
    return true
end

const DistributiveLaw = Axiom{:DL}()

function checkaxiom(::typeof(DistributiveLaw), o1::BinaryOperation{T,D}, o2::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    for i ∈ o.domain
        for j ∈ o.domain
            for k ∈ o.domain
                o1(i, o2(j, k)) == o2(o1(i, j), o1(i, k)) && return false
            end
        end
    end
    return true
end

struct Ordered{T<:Truth, D<:AbstractSet{T}} end

isordered(::Type{FiniteLattice{T,D}}) where {T<:Truth, D<:AbstractSet{T}} = Ordered{T,D}()
isordered(::Type{FiniteFLewAlgebra{T,D}}) where {T<:Truth, D<:AbstractSet{T}} = Ordered{T,D}()
isordered(::Type{FiniteHeytingAlgebra{T,D}}) where {T<:Truth, D<:AbstractSet{T}} = Ordered{T,D}()

function precedes(a::Ordered{T,D}, t1::T, t2::T) where {T<:Truth, D<:AbstractSet{T}}
    if a.meet(t1, t2) == t1
        return true
    else
        return false
    end
end
