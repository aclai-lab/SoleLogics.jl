# core.jl
export atom, Proposition, NamedOperator

const SimpleModalFrame = SimpleModalFrame

const ToCenteredRel = ToCenterRel

const NamedOperator = NamedConnective
const AbstractSyntaxStructure = SyntaxStructure

# Helper
function Base.getindex(i::AbstractInterpretation, v, args...; kwargs...)
    Base.getindex(i, Atom(v), args...; kwargs...)
end


const Proposition = Atom
Base.@deprecate atom(p::Proposition) value(p)

Base.@deprecate joinformulas(args...; kwargs...) composeformulas(args...; kwargs...)

# base-logic.jl
export BOTTOM, Bottom, bottom, isbottom

const BOTTOM = BOT
const Bottom = typeof(BOT)

Base.@deprecate bottom(a) bot(a)
Base.@deprecate isbottom(a) isbot(a)

# # parse.jl
# Base.@deprecate parsetree(s::String, args...; kwargs...) parseformula(s, args...; kwargs...)

# modal-logic.jl
export global_diamond, global_box

const global_diamond = globaldiamond
const global_box = globalbox

# propositional-logic.jl
function Base.haskey(i::AbstractAssignment, v)::Bool
    Base.haskey(i, Atom(v))
end

"""
    struct TruthTable{A,T<:Truth}

Dictionary which associates an [`AbstractAssignment`](@ref)s to the truth value of the
assignment itself on a [`SyntaxStructure`](@ref).

See also [`AbstractAssignment`](@ref), [`SyntaxStructure`](@ref), [`Truth`](@ref).
"""
struct TruthTable{
    A,
    T<:Truth
} <: Formula # TODO is this correct? Remove?
    truth::Dict{<:AbstractAssignment,Vector{Pair{SyntaxStructure,T}}}
end

# syntax-utils.jl
op(::LeftmostLinearForm{C}) where {C} = C()
