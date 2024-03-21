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

Check if axiom `a` is satisfied.

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

############################################################################################
#### Common axioms #########################################################################
############################################################################################

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
        D<:AbstractVector{T}
    }

A binary operation * on a set S is called commutative if x * y = y * x ∀ x, y ∈ S.

See also [`Axiom`](@ref), [`BinaryOperation`](@ref).
"""
function checkaxiom(
    ::typeof(Commutativity),
    o::BinaryOperation{T,D}
) where {
    T<:Truth,
    D<:AbstractVector{T}
}
    for i ∈ getdomain(o)
        for j ∈ getdomain(o)
            o(i, j) != o(j, i) && return false
        end
    end
    return true
end

# Helper
function iscommutative(o::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractVector{T}}
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
        D<:AbstractVector{T}
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
    D<:AbstractVector{T}
}
    for i ∈ getdomain(o)
        for j ∈ getdomain(o)
            for k ∈ getdomain(o)
                o(o(i, j), k) != o(i, o(j, k)) && return false
            end
        end
    end
    return true
end

# Helper
function isassociative(o::BinaryOperation{T,D}) where {T<:Truth, D<:AbstractVector{T}}
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
        D<:AbstractVector{T}
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
    D<:AbstractVector{T}
}
    for i ∈ getdomain(o1)
        for j ∈ getdomain(o1)
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
        D<:AbstractVector{T}
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
    D<:AbstractVector{T}
}
    for i ∈ getdomain(o)
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
        D<:AbstractVector{T}
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
    D<:AbstractVector{T}
}
    for i ∈ getdomain(o)
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
        D<:AbstractVector{T}
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
    D<:AbstractVector{T}
}
    if checkaxiom(LeftIdentity, o, e) && checkaxiom(RightIdentity, o, e)
        return true
    else
        return false
    end
end
