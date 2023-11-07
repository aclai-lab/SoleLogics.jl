export global_diamond, global_box

Base.@deprecate parsetree(s::String, args...; kwargs...) parseformula(s, args...; kwargs...)

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
