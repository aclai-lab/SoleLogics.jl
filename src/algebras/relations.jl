export AbstractRelation, GeometricalRelations, IntervalRelation, RCCRelation

export GlobalRel, IdentityRel
export globalrel, identityrel

# Interval algebra relations 1D

# IA
export IARelations
export IA_A, IA_L, IA_B, IA_E, IA_D, IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi
export IA_AorO, IA_DorBorE, IA_AiorOi, IA_DiorBiorEi
export IA_I

# Interval algebra relations 2D
export RectangleRelation
export IA2DRelations
# for relation in [IA2DRelations, IA2D_URelations]
#     @eval export $(Symbol(relation))
# end

# RCC8 relations
export RCC8Relations, RCC5Relations
export Topo_DC, Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi
export Topo_DR, Topo_PP, Topo_PPi

############################################################################################
# Singletons representing natural relations
############################################################################################

doc_identityrel = """
    struct IdentityRel <: AbstractRelation end;
    const identityrel   = IdentityRel();

Singleton type for the identity relation. This is a binary relation via which a world
accesses itself. The relation is also symmetric, reflexive and transitive.

# Examples
```
julia> syntaxstring(SoleLogics.identityrel)
"="
julia> SoleLogics.converse(IdentityRel)
IdentityRel
```

See also
[`GlobalRel`](@ref),
[`AbstractRelation`](@ref),
[`AbstractWorld`](@ref),
[`AbstractFrame`](@ref).
[`AbstractKripkeStructure`](@ref),
"""

"""$(doc_identityrel)"""
struct IdentityRel <: AbstractRelation end;
"""$(doc_identityrel)"""
const identityrel   = IdentityRel();

arity(::Type{IdentityRel}) = 2

syntaxstring(::Type{IdentityRel}; kwargs...) = "="

hasconverse(::Type{IdentityRel}) = true
converse(::Type{IdentityRel}) = IdentityRel
issymmetric(::IdentityRel) = true
isreflexive(::IdentityRel) = true
istransitive(::IdentityRel) = true

############################################################################################

doc_globalrel = """
    struct GlobalRel <: AbstractRelation end;
    const globalrel  = GlobalRel();

Singleton type for the global relation. This is a binary relation via which a world
accesses every other world within the frame.
The relation is also symmetric, reflexive and transitive.

# Examples
```
julia> syntaxstring(SoleLogics.globalrel)
"G"
julia> SoleLogics.converse(GlobalRel)
GlobalRel
```

See also
[`IdentityRel`](@ref),
[`AbstractRelation`](@ref),
[`AbstractWorld`](@ref),
[`AbstractFrame`](@ref).
[`AbstractKripkeStructure`](@ref),
"""

"""$(doc_globalrel)"""
struct GlobalRel <: AbstractRelation end;
"""$(doc_globalrel)"""
const globalrel  = GlobalRel();

arity(::Type{GlobalRel}) = 2

syntaxstring(::Type{GlobalRel}; kwargs...) = "G"

hasconverse(::Type{GlobalRel}) = true
converse(::Type{GlobalRel}) = GlobalRel
issymmetric(::GlobalRel) = true
isreflexive(::GlobalRel) = true
istransitive(::GlobalRel) = true

############################################################################################

"""
    struct NamedRelation{T} <: AbstractRelation
        name::T
    end

Type for relations that are solely defined by their name.

See also
[`AbstractRelation`](@ref),
[`AbstractWorld`](@ref),
[`AbstractFrame`](@ref).
[`AbstractKripkeStructure`](@ref),
"""
struct NamedRelation{T} <: AbstractRelation
    name::T
end

name(r::NamedRelation) = r.name

############################################################################################

# TODO add relation as union of relations.
# struct UnionOfRelations     <: AbstractRelation
#     relations :: NTuple{N,AbstractRelation} where {N}
# end;
# _accessibles(::AbstractMultiModalFrame, w::AbstractWorld, r::UnionOfRelations) =
#     Iterators.flatten((_accessibles(fr, w, sub_relation) for sub_relation in r.relations))

############################################################################################


include("relations/geometrical-relations.jl");
