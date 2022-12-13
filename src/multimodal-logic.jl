
export ismultimodal
export Relation, RelationalOperator, DiamondRelationalOperator, BoxRelationalOperator
export relationtype

############################################################################################
############################################################################################
############################################################################################

abstract type Relation end # TODO: why exporting an abstract type? also if it is abstract it should be called something like AbstractRelation

abstract type RelationalOperator{R<:Relation} end # TODO: why the type parameter?

# TODO: the logic should be "multimodal" or not, not the operators (which, the latter, should be "modal" or not)
ismultimodal(::Type{<:AbstractOperator}) = false
ismultimodal(o::AbstractOperator)::Bool = ismultimodal(typeof(o))
ismultimodal(::Type{<:RelationalOperator}) = true

relationtype(::RelationalOperator{R}) where {R<:Relation} = R

struct DiamondRelationalOperator{R<:Relation} <: RelationalOperator{R} end

struct BoxRelationalOperator{R<:Relation} <: RelationalOperator{R} end

############################################################################################
######################################## BASE ##############################################
############################################################################################





# # Named-relation type
# struct NamedRelation <: Relation
#     name::Symbol
#     # adjacency matrix between NamedWorld's
# end
