
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

############################################################################################
############################ COULD BE IMPLEMENTED ##########################################
############################################################################################

#= This macro was previously used to generate collections of relations with "the same shape"
    such as [L, A, D, O, etc...].
    With "the same shape" I mean that every property related to the relation creation itself
    must be the same (e.g ismodal trait, arity, etc...)

macro modaloperators(R, d::Int)
    quote
        rels = vec(collect(Iterators.product([$(R) for _ = 1:$(d)]...)))

        # I think we should remove this block
            if "=" in $(R)
                rels = rels[1:end-1]
            end
            exrels = [EXMODOP(r) for r in rels]
            univrels = [UNIVMODOP(r) for r in rels]
            Operators(vcat(exrels, univrels))

        # And then add here the property initialization for each relation.
        # Eventually, also the export.
    end
end
=#
