const WEAK_CONJUNCTION = CONJUNCTION
const STRONG_CONJUNCTION = NamedConnective{:⋅}()
const ⋅ = STRONG_CONJUNCTION
arity(::typeof(⋅)) = 2

const BASE_MANY_VALUED_CONNECTIVES = [∨, ∧, ⋅, →]
const BaseManyValuedConnectives = Union{typeof.(BASE_MANY_VALUED_CONNECTIVES)...}
