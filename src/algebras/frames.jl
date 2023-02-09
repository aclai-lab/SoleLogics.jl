
# Fix (not needed from Julia 1.7, see https://github.com/JuliaLang/julia/issues/34674 )
if length(methods(Base.keys, (Base.Generator,))) == 0
    Base.keys(g::Base.Generator) = g.iter
end

# TODO fix these...
abstract type InitCondition end

function initialworld() end
function initialworldset() end


# Note keep separate to avoid ambiguity
accessibles(fr::AbstractMultiModalFrame{W}, ::W, r::_RelationGlob) where {W<:AbstractWorld} = accessibles(fr, r)
accessibles(fr::AbstractMultiModalFrame{W}, ::AbstractWorldSet{W}, r::_RelationGlob) where {W<:AbstractWorld} = accessibles(fr, r)

############################################################################################

# Relations are defined via methods that return iterators to the accessible worlds.
# Each relation R<:AbstractRelation must provide a method for `accessibles`, which returns an iterator
#  to the worlds that are accessible from a given world w:
# `accessibles(fr::AbstractMultiModalFrame{W}, w::W,           r::R)::AbstractVector{W}`

# Alternatively, one can provide a *bare* definition, that is, method `_accessibles`,
#  returning an iterator of *tuples* which is then fed to a constructor of the same world type, as in:
# `_accessibles(fr::AbstractMultiModalFrame{W}, w::W,           r::R)::AbstractVector{Tuple}`

# The following fallback ensures that the two definitions are equivalent
accessibles(fr::AbstractMultiModalFrame{W}, w::W, r::AbstractRelation) where {W<:AbstractWorld} = begin
    IterTools.imap(W, _accessibles(fr, w, r))
end

#

# It is convenient to define methods for `accessibles` that take a world set instead of a
#  single world. Generally, this falls back to calling `_accessibles` on each world in
#  the set, and returning a constructor of wolds from the union; however, one may provide
#  improved implementations for special cases (e.g. ⟨L⟩ of a world set in interval algebra).
accessibles(fr::AbstractMultiModalFrame{W}, S::AbstractWorldSet{W}, r::AbstractRelation) where {W<:AbstractWorld} = begin
    IterTools.imap(W,
        IterTools.distinct(
            Iterators.flatten(
                (_accessibles(fr, w, r) for w in S)
            )
        )
    )
end

############################################################################################
# Singletons representing natural relations
############################################################################################

accessibles(fr::AbstractMultiModalFrame{W}, w::W,           ::_RelationId) where {W<:AbstractWorld} = [w] # TODO try IterTools.imap(identity, [w])
accessibles(fr::AbstractMultiModalFrame{W}, S::AbstractWorldSet{W}, ::_RelationId) where {W<:AbstractWorld} = S # TODO try IterTools.imap(identity, S)

############################################################################################

# Note: these methods must be defined for any newly defined world type W:
# `accessibles(fr::AbstractMultiModalFrame{W}, w::W,           ::_RelationGlob)`
# `accessibles(fr::AbstractMultiModalFrame{W}, S::AbstractWorldSet{W}, ::_RelationGlob)`

############################################################################################

# Shortcuts using global relation for enumerating all worlds
allworlds(fr::AbstractMultiModalFrame) = accessibles(fr, RelationGlob)

include("frames/full-dimensional-frame/main.jl")
