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

Operation{D, D1} where D1<:AbstractSet{D}
struct Lattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
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

############################################################################################
#### SECOND IMPLEMENTATION #################################################################
############################################################################################

# abstract type Axiom{T<:Truth, D<:AbstractSet{T}} end

# struct Commutativity{T<:Truth, D<:AbstractSet{T}} <: Axiom{T, D}
#     o::Operation{T,D}

#     function Commutativity(o::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
#         for i ∈ o.domain
#             for j ∈ o.domain
#                 @assert o(i, j) == o(j, i) "Operation $o is not commutative."
#             end
#         end
#         return new{T,D}(o)
#     end
# end

# struct Associativity{T<:Truth, D<:AbstractSet{T}} <: Axiom{T, D}
#     o::Operation{T,D}

#     function Associativity(o::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
#         for i ∈ o.domain
#             for j ∈ o.domain
#                 for k ∈ o.domain
#                     @assert o(o(i, j), k) == o(i, o(j, k)) "Operation $o is not associative."
#                 end
#             end
#         end
#         return new{T,D}(o)
#     end
# end

# struct AbsorptionLaw{T<:Truth, D<:AbstractSet{T}} <: Axiom{T, D}
#     o1::Operation{T,D}
#     o2::Operation{T,D}

#     function AbsorptionLaw(o1::Operation{T,D}, o2::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}
#         for i ∈ o1.domain
#             for j ∈ o1.domain
#                 @assert o1(i, o2(i, j)) == i "Absorption law doesn't hold between $o1 and $o2"
#             end
#         end
#         return new{T,D}(o1, o2)
#     end
# end

# abstract type FiniteAlgebra{T<:Truth, D<:AbstractSet{T}} <: AbstractAlgebra{T} end

# struct Lattice{T<:Truth, D<:AbstractSet{T}} <: FiniteAlgebra{T,D}
#     domain::D
#     join::Operation{T,D}
#     meet::Operation{T,D}

#     function Lattice(domain::D, join::Operation{T,D}, meet::Operation{T,D}) where {T<:Truth, D<:AbstractSet{T}}    
#         Commutativity(join)
#         associativity(join)
#         Commutativity(meet)
#         associativity(meet)
#         AbsorptionLaw(join, meet)
#         AbsorptionLaw(meet, join)
#         return new{T,D}(domain, join, meet)
#     end
# end
