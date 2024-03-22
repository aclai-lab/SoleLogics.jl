using ..SoleLogics: AbstractAlgebra
import ..SoleLogics: syntaxstring
import Base: convert

struct FLewTruth <: Truth
    label::String

    function FLewTruth(label::String)
        return new(label)
    end

    function FLewTruth(t::BooleanTruth)
        return convert(FLewTruth, t)
    end
end

syntaxstring(t::FLewTruth; kwargs...) = t.label
convert(::Type{FLewTruth}, t::BooleanTruth) = istop(t) ? FLewTruth("⊤") : FLewTruth("⊥")

"""
    struct FLewAlgebra <: AbstractAlgebra{FLewTruth}
        domain::Set{FLewTruth}
        jointable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
        meettable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
        monoidtable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
        implicationtable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
        bot::FLewTruth
        top::FLewTruth
    end

An FLew-algebra is an algebra (L, ∨, ∧, ⋅, →, ⊥, ⊤), where
 - (L, ∨, ∧, ⊥, ⊤) is a lattice with top element ⊤ and bottom element ⊥
 - (L, ⋅, ⊤) is a commutative monoid
 - The residuation property holds: x ⋅ y ≤ z iff x ≤ y → z

A lattice is an algebraic structure (L, ∨, ∧) consisting of a set L and two binary,
commutative and associative operations ∨ and ∧ on L satisfying the following axiomatic
identities for all elements a, b ∈ L (sometimes called absorption laws):
 - a ∨ (a ∧ b) = a
 - a ∧ (a ∨ b) = a

The following two identities are also usally regarded as axioms, even though they follow
from the two absorption laws taken together. These are called idempotent laws:
 - a ∨ a = a
 - a ∧ a = a

A bounded lattice is an algebraic structure (L, ∨, ∧, ⊥, ⊤) such that (L, ∨, ∧) is a
lattice, the bottom element ⊥ is the identity element for the join operation ∨, and the top
element ⊤ is the identity element for the meet operation ∧:
 - a ∨ ⊥ = a
 - a ∧ ⊤ = a

A monoid (L, ⋅, e) is a set L equipped with a binary operation L × L → L, denoted as ⋅,
satisfying the following axiomatic identities:
 - (Associativity) ∀ a, b, c ∈ L, the equation (a ⋅ b) ⋅ c = a ⋅ (b ⋅ c) holds.
 - (Identity element) There exists an element e ∈ L such that for every element a ∈ L, the equalities e ⋅ a = a
   and a ⋅ e = a hold. 

The identity element of a monoid is unique.

A commutative monoid is a monoid whose operation is commutative.
"""
struct FLewAlgebra <: AbstractAlgebra{FLewTruth}
    domain::Set{FLewTruth}
    jointable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
    meettable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
    monoidtable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
    implicationtable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
    bot::FLewTruth
    top::FLewTruth

    function FLewAlgebra(
        domain::Set{FLewTruth},
        jointable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth},
        meettable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth},
        monoidtable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth},
        implicationtable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth},
        bot::FLewTruth,
        top::FLewTruth
    )
        for i ∈ domain
            for j ∈ domain
                @assert (i, j) ∈ keys(jointable) "jointable[($i, $j)] is not defined."
                @assert (i, j) ∈ keys(meettable) "meettable[($i, $j)] is not defined."
                @assert (i, j) ∈ keys(monoidtable) "monoidtable[($i, $j)] is not defined."
                @assert (i, j) ∈ keys(monoidtable) "implicationtable[($i, $j)] is not defined."
            end
        end
        @assert length(jointable) == length(domain)^2 "Found jointable[(i, j)] where i or j ∉ domain."
        @assert length(meettable) == length(domain)^2 "Found meettable[(i, j)] where i or j ∉ domain."
        @assert length(monoidtable) == length(domain)^2 "Found monoidtable[(i, j)] where i or j ∉ domain."
        @assert length(implicationtable) == length(domain)^2 "Found implicationtable[(i, j)] where i or j ∉ domain."
        for i ∈ domain
            @assert monoidtable[(i, FLewTruth(⊤))] == i "Defined monoid don't satisfy the neutral element rule: monoidtable[($i, ⊤) ≠ $i]."
            for j ∈ domain
                @assert jointable[(i, j)] == jointable[(j, i)] "Defined join is not commutative: jointable[($i,$j)] ≠ jointable[($j,$i)]."
                @assert meettable[(i, j)] == meettable[(j, i)] "Defined meet is not commutative: meettable[($i,$j)] ≠ meettable[($j,$i)]."
                @assert monoidtable[(i, j)] == monoidtable[(j, i)] "Defined monoid is not commutative: monoidtable[($i,$j)] ≠ monoidtable[($j,$i)]."
                for k ∈ domain
                    @assert jointable[(jointable[(i, j)], k)] == jointable[(i, jointable[(j, k)])] "Defined join is not associative: jointable[(jointable[(i, j)], k)] ≠ jointable[(i, jointable[(j, k)])]."
                    @assert meettable[(meettable[(i, j)], k)] == meettable[(i, meettable[(j, k)])] "Defined meet is not associative: meettable[(meettable[(i, j)], k)] ≠ meettable[(i, meettable[(j, k)])]."
                    @assert monoidtable[(monoidtable[(i,j)], k)] == monoidtable[(i, monoidtable[(j, k)])] "Defined monoid is not associative: monoidtable[(monoidtable[(i,j)], k)] ≠ monoidtable[(i, monoidtable[(j, k)])]."
                end
                @assert jointable[(i, meettable[(i, j)])] == i "Defined join and meet don't satisfy the asborption law: jointable[($i, meettable[($i, $j)])] ≠ $i."
                @assert meettable[(i, jointable[(i, j)])] == i "Defined join and meet don't satisfy the asborption law: meettable[($i, jointable[($i, $j)])] ≠ $i."
            end
            @assert jointable[(i, i)] == i "Defined join don't satisfy the idempotent law: jointable[($i, $i) ≠ $i]."
            @assert meettable[(i, i)] == i "Defined meet don't satisfy the idempotent law: meettable[($i, $i) ≠ $i]."
            @assert meettable[(bot, i)] == bot "$bot isn't a valid bottom element: $bot ≰ $i"
            @assert jointable[(i, top)] == top "$top isn't a valid top element: $i ≰ $top"
        end
        return new(domain, meettable, jointable, monoidtable, implicationtable, bot, top)
    end
end

join(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = a.jointable[(t1, t2)]
meet(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = a.meettable[(t1, t2)]
monoid(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = a.monoidtable[(t1, t2)]
implication(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = a.implicationtable[(t1, t2)]
isbot(a::FLewAlgebra, t::FLewTruth) = t == a.bot
itop(a::FLewAlgebra, t::FLewTruth) = t == a.top

"""
    precedes(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth)

Given an algebraically defined lattice (L, ∨, ∧), one can define a partial ordern ≤ on L by
setting:
 - a ≤ b if a = a ∧ b, or
 - a ≤ b if b = a ∨ b,

for all elements a, b ∈ L. The laws of absorption ensure that both definitions are
equivalent:
 - a = a ∧ b implies b = b ∨ (b ∧ a) = (a ∧ b) ∨ b = a ∨ b

and dually for the other direction.
"""
function precedes(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth)
    if meet(a, t1, t2) == t1
        return true
    else
        return false
    end
end
