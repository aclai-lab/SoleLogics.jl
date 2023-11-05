
############################################################################################
# Natural relations
############################################################################################

# Note keep separate to avoid ambiguity
accessibles(fr::AbstractMultiModalFrame, ::AbstractWorld, r::GlobalRel) = accessibles(fr, r)
accessibles(fr::AbstractMultiModalFrame, ::AbstractWorldSet, r::GlobalRel) = accessibles(fr, r)
accessibles(fr::AbstractMultiModalFrame, w::AbstractWorld,    ::IdentityRel) = [w] # TODO try IterTools.imap(identity, [w])
accessibles(fr::AbstractMultiModalFrame, S::AbstractWorldSet, ::IdentityRel) = S # TODO try IterTools.imap(identity, S)

# Shortcut: when enumerating accessibles through global relation, delegate to `allworlds`
accessibles(fr::AbstractMultiModalFrame, ::GlobalRel) = allworlds(fr)

accessibles(fr::AbstractUniModalFrame, w::AbstractWorld,    ::IdentityRel) = [w] # TODO try IterTools.imap(identity, [w])
accessibles(fr::AbstractUniModalFrame, ::GlobalRel) = allworlds(fr)

############################################################################################

# It is convenient to define methods for `accessibles` that take a world set instead of a
#  single world. Generally, this falls back to calling `_accessibles` on each world in
#  the set, and returning a constructor of wolds from the union; however, one may provide
#  improved implementations for special cases (e.g. ⟨L⟩ of a world set in interval algebra).
function accessibles(
    fr::AbstractMultiModalFrame{W},
    S::AbstractWorldSet,
    r::AbstractRelation,
) where {W<:AbstractWorld}
    IterTools.imap(W,
        IterTools.distinct(
            Iterators.flatten(
                (_accessibles(fr, w, r) for w in S)
            )
        )
    )
end

############################################################################################

"""
Return an empty world (e.g., `Interval(-1,0)`).
"""
function emptyworld(fr::AbstractMultiModalFrame)
    return error("Please, provide method emptyworld(::$(typeof(fr))).")
end

"""
Return the world at the *center* of the frame;
note that this does not always exist.
"""
function centralworld(fr::AbstractMultiModalFrame)
    return error("Please, provide method centralworld(::$(typeof(fr))).")
end

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
[`IdentityRel`](@ref),
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
