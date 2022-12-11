#### Relations ####
abstract type Relation end


ismodal(::AbstractOperator) = false

doc_DIAMOND = """
    const DIAMOND = NamedOperator{:◊}()
    const ◊ = DIAMOND
    arity(::typeof(◊)) = 1

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
ismodal(::typeof(◊)) = true
arity(::typeof(◊)) = 1


doc_BOX = """
    const BOX = NamedOperator{:□}()
    const □ = BOX
    arity(::typeof(□)) = 1

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
ismodal(::typeof(□)) = true
arity(::typeof(□)) = 1


RelationalOperator
    relation(op::RelationalOperator) = op.relation
DiamondRelationalOperator
BoxRelationalOperator
    ismodal(...)

