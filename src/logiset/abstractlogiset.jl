"""
    abstract type AbstractLogiset{T} end

Generic type for a logiset, that is, a collection of logical models parametrized
over the representation of the model (e.g., [`KripkeStructure`](@ref)s).

# Interfaces

- worlds(logiset::AbstractLogiset{T}, instance::Int) where {T}
- featval(logiset::AbstractLogiset{T}, instance::Int, world::W)
- accessibles(logiset::AbstractLogiset{T}, instance::Int, world::W, relation::R}
- frame(logiset::AbstractLogiset{T}) where {T}

Each concrete type must offer the following fields:
- collection(logiset::AbstractLogiset{T}) where {T}
- relations(logiset::AbstractLogiset{T}) where {T}
- alphabet(logiset::AbstractLogiset{T}) where {T}

# Other Methods

- hasaccessibles(logiset::AbstractLogiset{T}, instance::Int, world::W, 
    relation::Union{Nothing, R} = nothing,)

"""
abstract type AbstractLogiset{T} end

"""
    iseuclidean(::AbstractLogiset{T}) where {T}

Distinguish between euclidean [`AbstractLogiset`](@ref)s (i.e., the frame is 
an array-like collection) and non-euclidean ones (i.e., the frame is a graph).
"""
iseuclidean(::AbstractLogiset{T}) where {T} = false
iseuclidean(::AbstractLogiset{T}) where {T <: AbstractArray} = true

function worlds(logiset::AbstractLogiset{T}, instance::Int) where {T}
    throw(MethodError(worlds, (logiset, instance)))
end

function featval(
        logiset::AbstractLogiset{T},
        instance::Int, world::W,) where {T, W <: AbstractWorld}
    throw(MethodError(worlds, (logiset, instance, world)))
end

function accessibles(
        logiset::AbstractLogiset{T},
        instance::Int, world::W,
        relation::Union{Nothing, R} = nothing,) where {
        T, W <: AbstractWorld, R <: AbstractRelation,}
    throw(MethodError(worlds, (logiset, instance, world, relation)))
end

function hasaccessibles(
        logiset::AbstractLogiset{T},
        instance::Int, world::W,
        relation::Union{Nothing, R} = nothing,) where {
        T, W <: AbstractWorld, R <: AbstractRelation,}
    return !isempty(accessibles(logiset, instance, world, relation))
end

function collection(logiset::AbstractLogiset{T}) where {T}
    return logiset.collection
end

function alphabet(logiset::AbstractLogiset{T}) where {T}
    return logiset.alphabet
end

function frame(logiset::AbstractLogiset{T}) where {T}
    throw(MethodError(frame, (logiset)))
end
