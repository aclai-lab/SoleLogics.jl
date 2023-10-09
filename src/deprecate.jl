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


export Proposition, atom

const Proposition = Atom
Base.@deprecate atom(p::Proposition) value(p)


op(::LeftmostLinearForm{C}) where {C} = C()

export BOTTOM, Bottom, bottom, isbottom

const BOTTOM = BOT
const Bottom = Bot

Base.@deprecate bottom(a) bot(a)
Base.@deprecate isbottom(a) isbot(a)
