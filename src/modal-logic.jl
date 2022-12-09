#### Relations ####
abstract type Relation end


is_modal(::AbstractOperator) = false

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
is_modal(::typeof(◊)) = true
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
is_modal(::typeof(□)) = true
arity(::typeof(□)) = 1


RelationalOperator
    relation(op::RelationalOperator) = op.relation
DiamondRelationalOperator
BoxRelationalOperator
    ismodal(...)

