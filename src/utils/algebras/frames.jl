
############################################################################################
# Natural relations
############################################################################################

# Note keep separate to avoid ambiguity
accessibles(fr::AbstractMultiModalFrame, ::AbstractWorlds, r::GlobalRel) = accessibles(fr, r)
accessibles(fr::AbstractMultiModalFrame, S::AbstractWorlds, ::IdentityRel) = S # TODO try IterTools.imap(identity, S)


accessibles(fr::AbstractUniModalFrame, w::AbstractWorld,    ::IdentityRel) = [w]
accessibles(fr::AbstractUniModalFrame, ::GlobalRel) = allworlds(fr)

############################################################################################

doc_tocenterrel = """
    struct ToCenteredRel <: AbstractRelation end;
    const tocenterrel = ToCenteredRel();

Singleton type for a relation that leads to the world at the center of a frame.
The relation is transitive.

# Examples
```julia-repl
julia> syntaxstring(SoleLogics.tocenterrel)
"◉"
```

See also
[`identityrel`](@ref),
[`centralworld`](@ref),
[`AbstractRelation`](@ref),
[`AbstractWorld`](@ref),
[`AbstractFrame`](@ref).
[`AbstractKripkeStructure`](@ref),
"""

"""$(doc_tocenterrel)"""
struct ToCenteredRel <: AbstractRelation end;
"""$(doc_tocenterrel)"""
const tocenterrel = ToCenteredRel();

accessibles(fr::AbstractMultiModalFrame, ::AbstractWorld, r::ToCenteredRel) = [centralworld(fr)]

arity(::ToCenteredRel) = 2

syntaxstring(::ToCenteredRel; kwargs...) = "◉"

hasconverse(::ToCenteredRel) = false
istransitive(::ToCenteredRel) = true
isgrounding(::ToCenteredRel) = true

############################################################################################

# TODO remove
# Fix (not needed from Julia 1.7, see https://github.com/JuliaLang/julia/issues/34674 )
if length(methods(Base.keys, (Base.Generator,))) == 0
    Base.keys(g::Base.Generator) = g.iter
end

############################################################################################

include("frames/full-dimensional-frame/main.jl")
