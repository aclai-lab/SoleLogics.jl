Base.@deprecate function check(
    φ::SoleLogics.AbstractFormula,
    X::AbstractInterpretationSet{<:AbstractKripkeStructure},
    i_instance::Integer,
    args...;
    kwargs...
)
