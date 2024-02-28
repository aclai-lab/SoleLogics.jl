using ..SoleLogics: AbstractAlgebra

struct Operation{T<:Truth, D<:AbstractSet{T}}
    domain::D
    truthtable::AbstractDict{Tuple{T, T}, T}

    function Operation(domain::D, truthtable::Dict{Tuple{T, T}, T}) where {T<:Truth, D<:AbstractSet{T}}
        for i ∈ domain
            for j ∈ domain
                @assert (i, j) ∈ keys(truthtable) "truthtable[($i, $j)] is not defined."
            end
        end
        @assert length(truthtable) == length(domain)^2 "Found truthtable[(i, j)] where i or j ∉ domain."
        return new{T,D}(domain, truthtable)
    end

    function Operation(domain::D, operation::F) where {T<:Truth, D<:AbstractSet{T}, F<:Function}
        truthtable = Dict{Tuple{T, T}, T}()
        for i ∈ domain
            for j ∈ domain
                truthtable[(i, j)] = operation(i, j)
            end
        end
        return Operation(domain, truthtable)
    end
end

(o::Operation{T,D})(t1::T, t2::T) where {T<:Truth, D<:AbstractSet{T}} = o.truthtable[(t1, t2)]

############################################################################################
#### FIRST IMPLEMENTATION ##################################################################
############################################################################################

struct Axiom{Symbol} end

const Commutativity = Axiom{:COM}()

function checkaxiom(::typeof(Commutativity), o::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    for i ∈ o.domain
        for j ∈ o.domain
            o(i, j) != o(j, i) && return false
        end
    end
    return true
end

# Helper
iscommutative(o::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}} = checkaxiom(Commutativity, o)

const Associativity = Axiom{:ASS}()

function checkaxiom(::typeof(Associativity), o::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
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
isassociative(o::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}} = checkaxiom(Associativity, o)

const AbsorptionLaw = Axiom{:AL}()

function checkaxiom(::typeof(AbsorptionLaw), o1::Operation{T,D}, o2::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    for i ∈ o1.domain
        for j ∈ o1.domain
            o1(i, o2(i, j)) != i && return false
        end
    end
    return true
end

abstract type FiniteAlgebra{T<:Truth, D<:AbstractSet{T}} <: AbstractAlgebra{T} end

struct FiniteLattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
    domain::D
    join::Operation{T,D}
    meet::Operation{T,D}

    function Lattice(domain::D, join::Operation{T,D}, meet::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
        @assert iscommutative(join) "Defined a join operation which is not commutative."
        @assert isassociative(join) "Defined a join operation which is not associative."
        @assert iscommutative(meet) "Defined a meet operation which is not commutative." 
        @assert isassociative(meet) "Defined a meet operation which is not associative."
        @assert checkaxiom(AbsorptionLaw, join, meet) "Absorption law doesn't hold between join and meet"
        @assert checkaxiom(AbsorptionLaw, meet, join) "Absorption law doesn't hold between meet and join"
        return new{T,D}(domain, join, meet)
    end
end

const IdentityElement = Axiom{:IE}()

function checkaxiom(::typeof(IdentityElement), o::Operation{T,D}, identityelement::T) where {T<:Truth, D<:AbstractSet{T}}
    for i ∈ o.domain
        @assert o(i, identityelement) != i return false
    end
    return true
end

struct Monoid{T<:Truth, D<:AbstractSet{T}}
    domain::D
    operation::Operation{T,D}
    identityelement::T

    function Monoid(domain::D, operation::Operation{T,D}, identityelement::T) where {T<:Truth, D<:AbstractSet{T}}
        @assert isassociative(monoid) "Defined an operation for the monoid which is not associative."
        @assert checkaxiom(IdentityElement, operation, identityelement) "$identityelement is not a valid identityelement for operation $o."
        return new(domain, operation, identityelement)
    end
end

function checkaxiom(::typeof(Commutativity), monoid::Monoid{T,D}) where {T<:Truth, D<:AbstractSet{T}}
    return checkaxiom(Commutativity, monoid.operation)
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
    join::Operation{T,D}
    meet::Operation{T,D}
    star::Monoid{T,D}
    implication::Operation{T,D}
    bot::T
    top::T

    function FiniteFLewAlgebra(domain::D, join::Operation{T,D}, meet::Operation{T,D}, monoid::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
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

convert(::Type{Lattice}, l::FiniteFLewAlgebra) = Lattice(domain, join, meet)

struct FiniteHeytingAlgebra{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,F}
    domain::D
    join::Operation{T,D}
    meet::Operation{T,D}
end

struct Ordered end

isordered(::Type{Lattice}) = Ordered()
isordered(::Type{FiniteFLewAlgebra}) = Ordered()
isordered(::Type{FiniteHeytingAlgebra}) = Ordered()

function precedes(a::Ordered, t1::FLewTruth, t2::FLewTruth)
    if a.meet(t1, t2) == t1
        return true
    else
        return false
    end
end
