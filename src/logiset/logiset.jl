"""
!!! warn
    TODO: explain which interface T should implement

# Examples
```julia
# Each instance is a single annotated graph
julia> kripkestructures = Logiset{Vector{KripkeStructure}}

# Each instance is a table of [`check`](@ref)able structures
julia> euclideanlogiset = Logiset{Vector{<:AbstractArray}}
```
"""
struct Logiset{T} <: AbstractLogiset{T}
    alphabet::Vector{Atom}
    collection::T
end

const LOGISET = Logiset{Vector{KripkeStructure}}
const EUCLIDEANLOGISET{N} = Logiset{Array{N,<:Truth}} where {N}
const PROPOSITIONALLOGISET = EUCLIDEANLOGISET{2}

# TODO: implement these...
#=
function worlds(logiset::LOGISET, instance::Int)
end

function featval(
        logiset::Logiset{T},
        instance::Int, world::W,) where {T, W <: AbstractWorld}
    throw(MethodError(worlds, (logiset, instance, world)))
end

function accessibles(
        logiset::Logiset{T},
        instance::Int, world::W,
        relation::Union{Nothing, R} = nothing,) where {
        T, W <: AbstractWorld, R <: AbstractRelation,}
    throw(MethodError(worlds, (logiset, instance, world, relation)))
end

function hasaccessibles(
        logiset::Logiset{T},
        instance::Int, world::W,
        relation::Union{Nothing, R} = nothing,) where {
        T, W <: AbstractWorld, R <: AbstractRelation,}
    return !isempty(accessibles(logiset, instance, world, relation))
end

function collection(logiset::Logiset{T}) where {T}
    return logiset.collection
end

function alphabet(logiset::Logiset{T}) where {T}
    return logiset.alphabet
end

function frame(logiset::Logiset{T}) where {T}
    throw(MethodError(frame, (logiset)))
end
=#
