
############################################################################################
#### Interpretation ################################################################
############################################################################################

"""
    abstract type Interpretation end

Abstract type for representing a [logical
interpretation](https://en.wikipedia.org/wiki/Interpretation_(logic)).
In the case of
[propositional logic](https://simple.wikipedia.org/wiki/Propositional_logic),
is essentially a map *atom → truth value*.

Properties expressed via logical formulas can be `check`ed on logical interpretations.

# Interface
- `valuetype(i::Interpretation)`
- `truthtype(i::Interpretation)`
- `interpret(φ::Formula, i::Interpretation, args...; kwargs...)::Formula`

# Utility functions
- `check(φ::Formula, i::Interpretation, args...; kwargs...)::Bool`

See also [`check`](@ref), [`AbstractAssignment`](@ref), [`AbstractKripkeStructure`](@ref).
"""
abstract type Interpretation end

function valuetype(i::Interpretation)
    return error("Please, provide method valuetype(::$(typeof(i))).")
end
function truthtype(i::Interpretation)
    return error("Please, provide method truthtype(::$(typeof(i))).")
end

############################################################################################
#### Interpret & Check #####################################################################
############################################################################################

"""
    interpret(
        φ::Formula,
        i::Interpretation,
        args...;
        kwargs...
    )::Formula

Return the truth value for a formula on a logical interpretation (or model).

# Examples
```julia-repl
julia> @atoms p q
2-element Vector{Atom{String}}:
 p
 q

julia> td = TruthDict([p => true, q => false])
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│      ⊥ │      ⊤ │
└────────┴────────┘

julia> interpret(CONJUNCTION(p,q), td)
⊥
```

See also [`check`](@ref), [`Formula`](@ref), [`Interpretation`](@ref),
[`AbstractAlgebra`](@ref).
"""
function interpret(
    φ::Formula,
    i::Interpretation,
    args...;
    kwargs...
)::Formula
    interpret(tree(φ), i, args...; kwargs...)
end

function interpret(
    φ::AbstractAtom,
    i::Interpretation,
    args...;
    kwargs...,
)::Formula
    return error("Please, provide method " *
                 "interpret(φ::$(typeof(φ)), i::$(typeof(i)), " *
                join(map(t->"::$(t)", typeof.(args)), ", ") * "; " *
                join(map(p->"$(p.first)::$(p.second)", kwargs), ", ") * ").")
end

function interpret(
    φ::AbstractSyntaxBranch,
    i::Interpretation,
    args...;
    kwargs...,
)
    connective = token(φ)
    ts = Tuple(
        [interpret(ch, i, args...; kwargs...) for ch in children(φ)]
    )
    return simplify(connective, ts, args...; kwargs...)
end


interpret(t::Truth, i::Interpretation, args...; kwargs...) = t

"""
Algorithm used for checking a formula on an interpretation.
"""
abstract type CheckAlgorithm end

"""
Default, general-purpose check algorithm.

This algorithm is not optimized for special cases.
"""
struct DefaultCheckAlgorithm <: CheckAlgorithm end

"""
    check(
        [algo::CheckAlgorithm,]
        φ::Formula,
        i::Interpretation,
        args...;
        kwargs...
    )::Bool

Check a [`Formula`](@ref) on a logical interpretation (or model), returning `true` if the truth value
for the formula `istop`.
This process is referred to as (finite)
[model checking](https://en.wikipedia.org/wiki/Model_checking), and there are many
algorithms for it, typically depending on the complexity of the logic.

# Examples
```julia-repl
julia> @atoms String p q
2-element Vector{Atom{String}}:
 Atom{String}("p")
 Atom{String}("q")

julia> td = TruthDict([p => TOP, q => BOT])
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│      ⊥ │      ⊤ │
└────────┴────────┘

julia> check(CONJUNCTION(p,q), td)
false
```

See also [`check`](@ref), [`interpret`](@ref), [`Interpretation`](@ref).
"""
function check(φ::Formula, args...; kwargs...)::Union{Bool, Vector{Bool}}
    check(DefaultCheckAlgorithm(), φ, args...; kwargs...)
end

function check(::CheckAlgorithm, φ::Formula, i::Interpretation, args...; kwargs...)::Bool
    istop(interpret(φ, i, args...; kwargs...))
end

############################################################################################
#### Utilities #############################################################################
############################################################################################

# Formula interpretation via i[φ] -> ψ
Base.getindex(i::Interpretation, φ::Formula, args...; kwargs...) =
    interpret(φ, i, args...; kwargs...)

# Formula interpretation via φ(i) -> ψ
(φ::Formula)(i::Interpretation, args...; kwargs...) =
    interpret(φ, i, args...; kwargs...)
