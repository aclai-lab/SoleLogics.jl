struct Operation{D} where {D<:AbstractSet{T<:Truth}}
    domain::D
    truthtable::Dict{Tuple{T, T}, T}

    function Operation(domain::D, truthtable::Dict{Tuple{T, T}, T}) where {D<:AbstractSet{T<:Truth}}
        for i ∈ domain
            for j ∈ domain
                @assert (i, j) ∈ keys(truthtable) "truthtable[($i, $j)] is not defined."
            end
        end
        @assert length(truthtable) == length(domain)^2 "Found truthtable[(i, j)] where i or j ∉ domain."
        return new(domain, truthtable)
    end

    function Operation(domain::D, operation::F) where {D<:AbstractSet{T<:Truth}, F<:Function}
        truthtable = Dict{Tuple{T, T}, T}()
        for i ∈ domain
            for j ∈ domain
                truthtable[(i, j)] = operation(i, j)
            end
        end
        return Operation(domain, truthtable)
    end
end

domain(o::Operation) = o.domain
(o::Operation{D})(t1::T, t2::T) where {D<:AbstractSet{T<:Truth}} = o.truthtable[(t1, t2)]

"""
TODO: an Axiom also depend on the operations on which it is defined
"""
abstract type Axiom end

struct Commutativity <: Axiom end

function checkaxiom(::typeof(Commutativity), o::Operation{T}) where {T<:Truth}
    for i ∈ domain(operation)
        for j ∈ domain(operation)
            o(i, j) != o(j, i) && return false
        end
    end
    return true
end

iscommutative(o::Operation) = checkaxiom(Commutativity, o)

struct Associativity <: Axiom end

function checkaxiom(::typeof(Associativity), o::Operation{T}) where {T<:Truth}
    for i ∈ domain(o)
        for j ∈ domain(o)
            for k ∈ domain(o)
                o(o(i, j), k) != o(i, o(j, k)) && return false
            end
        end
    end
    return true
end

isassociative(o::Operation) = checkaxiom(Associativity, o)

struct AbsorptionLaw <: Axiom end

function checkaxiom(::typeof(AbsorptionLaw), o1::Operation{T}, o2::Operation{T}) where {T<:Truth}
    for i ∈ domain(operation)
        for j ∈ domain(operation)
            o1(i, o2(i, j)) != i && return false
        end
    end
    return true
end

abstract type AlgebraicStructure{D<:AbstractSet{T<:Truth}, O<:AbstractSet{Operation{D}}} end

struct Lattice{D<:AbstractSet{T<:Truth}, O<:AbstractSet{Operation{D}}} <: AlgebraicStructure{D, O}
    domain::D
    join::Operation{D}
    meet::Operation{D}
    
    @assert iscommutative(join) "Defined a join operation which is not commutative."
    @assert isassociative(join) "Defined a join operation which is not associative."
    @assert iscommutative(meet) "Defined a meet operation which is not commutative." 
    @assert isassociative(meet) "Defined a meet operation which is not associative."
    @assert checkaxiom(AbsorptionLaw, join, meet) "Absorption law doesn't hold between join and meet"
    @assert checkaxiom(AbsorptionLaw, meet, join) "Absorption law doesn't hold between meet and join"
end
