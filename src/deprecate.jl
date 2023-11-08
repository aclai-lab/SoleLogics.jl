# core.jl
export atom, Proposition, NamedOperator

const NamedOperator = NamedConnective

const Proposition = Atom
Base.@deprecate atom(p::Proposition) value(p)

Base.@deprecate joinformulas(args...; kwargs...) composeformulas(args...; kwargs...)

# base-logic.jl
export BOTTOM, Bottom, bottom, isbottom

const BOTTOM = BOT
const Bottom = Bot

Base.@deprecate bottom(a) bot(a)
Base.@deprecate isbottom(a) isbot(a)

# parse.jl
Base.@deprecate parsetree(s::String, args...; kwargs...) parseformula(s, args...; kwargs...)

# modal-logic.jl
export global_diamond, global_box

const global_diamond = globaldiamond
const global_box = globalbox

# syntax-utils.jl
op(::LeftmostLinearForm{C}) where {C} = C()
