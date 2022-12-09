
PropositionalLogic = Logic{G, A} where {ALP, G<:AbstractGrammar{ALP, BaseOperators}, A<:AbstractAlgebra}


TruthDictionary{L} <: AbstractLogicalModel{L}
    goeswith(::PropositionalLogic) = true
    check(::Formula{L}, ::TruthDictionary)
