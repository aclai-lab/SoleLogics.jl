export GeometricalRelations, IntervalRelation, RCCRelation

# Interval algebra relations 1D

# IA
export IARelations
export IA_A, IA_L, IA_B, IA_E, IA_D, IA_O
export IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi
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


include("relations/filtered-relations.jl");

############################################################################################


include("relations/geometrical-relations.jl");
