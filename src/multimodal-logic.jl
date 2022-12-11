
export ismultimodal
export Relation, RelationalOperator, DiamondRelationalOperator, BoxRelationalOperator
export relationtype

############################################################################################
############################################################################################
############################################################################################

abstract type Relation end

abstract type RelationalOperator{R<:Relation} end

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
