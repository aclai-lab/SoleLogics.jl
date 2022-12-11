export ismodal
export DIAMOND, BOX, ◊, □

export ismultimodal
export Relation, RelationalOperator, DiamondRelationalOperator, BoxRelationalOperator
export relationtype

############################################################################################
############################################################################################
############################################################################################

ismodal(::Type{<:AbstractOperator}) = false
ismodal(o::AbstractOperator)::Bool = ismodal(typeof(o))

doc_DIAMOND = """
    const DIAMOND = NamedOperator{:◊}()
    const ◊ = DIAMOND
    arity(::NamedOperator{:◊}) = 1

Logical diamond operator, typically interpreted as the modal existential quantifier.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_DIAMOND)
"""
const DIAMOND = NamedOperator{:◊}()
"""
$(doc_DIAMOND)
"""
const ◊ = DIAMOND
ismodal(::NamedOperator{:◊}) = true
arity(::NamedOperator{:◊}) = 1


doc_BOX = """
    const BOX = NamedOperator{:□}()
    const □ = BOX
    arity(::NamedOperator{:□}) = 1

Logical box operator, typically interpreted as the modal universal quantifier.

See also [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""
$(doc_BOX)
"""
const BOX = NamedOperator{:□}()
"""
$(doc_BOX)
"""
const □ = BOX
ismodal(::NamedOperator{:□}) = true
arity(::NamedOperator{:□}) = 1


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


# TODO kripke frames represented by graphs of "named" worlds with labelled, "named" relations

# # Named-world type
# struct NamedWorld <: World end
#     name::Symbol
# end

# # Named-relation type
# struct NamedRelation <: Relation end
#     name::Symbol
#     adjacency matrix between NamedWorld's
# end
