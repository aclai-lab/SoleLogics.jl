
export ismultimodal
export AbstractRelationalOperator, DiamondRelationalOperator, BoxRelationalOperator
export relationtype

############################################################################################
############################################################################################
############################################################################################

abstract type AbstractRelation end

abstract type AbstractRelationalOperator{R<:AbstractRelation} end
# TODO: why the type parameter?
# TODO-reply: We want to dispatch on it. In this case, because different relations
#  carry different algorithmic behaviors (e.g., Later vs. After are computed in a
#  different way).

relationtype(::AbstractRelationalOperator{R}) where {R<:AbstractRelation} = R

struct DiamondRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end

struct BoxRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end

############################################################################################
######################################## BASE ##############################################
############################################################################################





# # Named-relation type
# struct NamedRelation <: AbstractRelation
#     name::Symbol
#     # adjacency matrix between NamedWorld's
# end
