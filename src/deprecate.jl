export global_diamond, global_box

Base.@deprecate check(
    φ::SoleLogics.AbstractFormula,
    X::AbstractInterpretationSet{<:AbstractKripkeStructure},
    i_instance::Integer,
    args...;
    kwargs...
) check(tree(φ), X, i_instance, args...; kwargs...)

const global_diamond = globaldiamond
const global_box = globalbox
