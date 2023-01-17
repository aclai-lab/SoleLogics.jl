# Abstract types for relations
abstract type AbstractRelation end

############################################################################################

"""
Each relation (type) must provide a method yielding its `arity`:

    arity(::Type{<:AbstractRelation})::Integer
    arity(t::AbstractRelation)::Integer = arity(typeof(t))

See also [`AbstractRelation`](@ref).
"""
arity(R::Type{<:AbstractRelation})::Integer = error("Please, provide method arity(::$(R)).")
arity(r::AbstractRelation)::Integer = arity(typeof(r))

# Relations must indicate their compatible world types via `goeswith`.
#  For example, if world type W is compatible with relation R
# goeswith(::Type{W}, ::R) = true
# Here's the fallback:
goeswith(::Type{W}, ::AbstractRelation) where {W<:AbstractWorld} = false

# Relations can be symmetric, reflexive and/or transitive.
# By default, none of this cases holds:
is_symmetric(r::AbstractRelation) = false
is_reflexive(r::AbstractRelation) = false
is_transitive(r::AbstractRelation) = false

# TODO add are_inverse_relation/inverse_relation trait

############################################################################################
# Singletons representing natural relations
############################################################################################

# Identity relation: any world -> itself
struct _RelationId <: AbstractRelation end; const RelationId   = _RelationId();

arity(::Type{_RelationId}) = 2

Base.show(io::IO, ::_RelationId) = print(io, "=")

is_symmetric(r::_RelationId) = true
is_reflexive(r::_RelationId) = true
is_transitive(r::_RelationId) = true

############################################################################################

# Global relation: any world -> all worlds
struct _RelationGlob <: AbstractRelation end; const RelationGlob  = _RelationGlob();

arity(::Type{_RelationGlob}) = 2

Base.show(io::IO, ::_RelationGlob) = print(io, "G")

is_symmetric(r::_RelationGlob) = true
is_reflexive(r::_RelationGlob) = true
is_transitive(r::_RelationGlob) = true

############################################################################################

# TODO add relation as union of relations.
# struct UnionOfRelations     <: AbstractRelation
#     relations :: NTuple{N,AbstractRelation} where {N}
# end;
# _accessibles(w::AbstractWorld, r::UnionOfRelations, args...) = Iterators.flatten((_accessibles(w, sub_relation,  args...) for sub_relation in topo2IARelations(r)))

############################################################################################


include("geometrical-relations.jl");

export AbstractRelation,
    IntervalRelation, RCCRelation

export _RelationGlob, _RelationId
export RelationGlob, RelationId
export goeswith

# Interval algebra relations 1D

# IA
export _IA_A, _IA_L, _IA_B, _IA_E, _IA_D, _IA_O, _IA_Ai, _IA_Li, _IA_Bi, _IA_Ei, _IA_Di, _IA_Oi
export IA_A, IA_L, IA_B, IA_E, IA_D, IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi

# IA7
export _IA7Rel
export _IA_AorO, _IA_DorBorE, _IA_AiorOi, _IA_DiorBiorEi
export IA_AorO, IA_DorBorE, IA_AiorOi, IA_DiorBiorEi

# IA3
export _IA3Rel
export _IA_I
export IA_I

export IARelations, IA7Relations, IA3Relations
export IA72IARelations # NOTE: read the comment about fixing IA32IARelations

# Interval algebra relations 2D

export _IABase
export RectangleRelation

# Export IA2DRelations and IA2D_URelations contents
for relation in [IA2DRelations, IA2D_URelations]
    @eval export $(Symbol(relation))
end
export IA2DRelations, IA2D_URelations, IA2DRelations_extended

# RCC8 relations
export RCC8Relation
export _Topo_DC, _Topo_EC, _Topo_PO, _Topo_TPP, _Topo_TPPi, _Topo_NTPP, _Topo_NTPPi
export Topo_DC, Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi

# RCC5 relations
export RCC5Relation
export _Topo_DR, _Topo_PP, _Topo_PPi
export Topo_DR, Topo_PP, Topo_PPi

export RCC8Relations, RCC5Relations
export RCC8RelationFromIA
export topo2IARelations, RCC52RCC8Relations, RCC52IARelations
