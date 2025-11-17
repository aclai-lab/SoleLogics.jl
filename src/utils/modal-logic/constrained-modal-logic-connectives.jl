
"""
    struct ConstrainedConnective{Symbol} <: Connective

Connective enriched with a `threshold`.

When evaluating a constrained connective on a world `w` of an [`AbstractFrame`](@ref),
the neighbors of `w`, named `nw`, are considered only if `condition(nw, threshold)` is true.

Two examples of built-in `ConstrainedConnectives` are [`DIAMOND2`](@ref) and [`BOX2`](@ref).

[`DIAMOND2`](@ref) is a special diamond operator (see [`DIAMOND`](@ref)), stating that there
are at least 2 accessible worlds where a formula holds.

[`BOX2`](@ref) is a special box operator (see [`BOX`](@ref)), stating that there are at
most 1 accessible worlds where a formula does not hold (¬◊₂¬).

See also [`AbstractFrame`](@ref) [`Connective`](@ref), [`DIAMOND`](@ref),
[`NamedConnective`](@ref).
"""
struct ConstrainedConnective{S,N,F<:Function} <: Connective
    condition::F

    ConstrainedConnective{S,N}(condition::F) where {S,N,F<:Function} = new{S,N,F}(condition)

    ConstrainedConnective{S}(
        threshold::Int,
        condition::F
    ) where {S,F<:Function} = new{S,threshold,F}(condition)

end

"""
    name(::ConstrainedConnective{S}) where {S} = S

Return the symbol identifying a specific [`ConstrainedConnective`](@ref).

See also [`BOX2`](@ref), [`Connective`](@ref), [`DIAMOND2`](@ref).
"""
name(::ConstrainedConnective{S,N}) where {S,N} = S

"""
    condition(cc::ConstrainedConnective) = cc.condition

Return a comparator wrapped within a [`ConstrainedConnective`](@ref).
It is a special function intended to compare the set of neighbors of a specific world within
 an [`AbstractFrame`](@ref), with respect to an integer `threshold`.

See also [`Connective`](@ref), [`DIAMOND`](@ref), [`DIAMOND2`](@ref), [`threshold`](@ref).
"""
condition(cc::ConstrainedConnective) = cc.condition

"""
    condition(cc::ConstrainedConnective, val::Int)

Shortcut for `condition(cc)(val, threshold(cc))`.

See also [`condition(cc::ConstrainedConnective)`](@ref), [`ConstrainedConnective`](@ref).
"""
condition(cc::ConstrainedConnective, val::Int) = condition(cc)(val, threshold(cc))


"""
    threshold(cc::ConstrainedConnective) = cc.threshold

Local argument of [`condition(cc::ConstrainedConnective)`](@ref).
"""
threshold(::ConstrainedConnective{S,N}) where {S,N} = N

syntaxstring(cc::ConstrainedConnective; kwargs...) = (name(cc), threshold(cc)) |> join
Base.show(io::IO, cc::ConstrainedConnective) = print(io, "$(syntaxstring(cc))")


"""
    const DIAMOND2 = NamedConnective{:◊}(>=, 2)
    const ◊₂ = DIAMOND2

Special [`DIAMOND`](@ref) connective, stating that there are at least 2 accessible worlds
(within an [`AbstractFrame`](@ref) where a formula holds.

See [`Connective`](@ref), [`DIAMOND`](@ref), [`ConstrainedConnective`](@ref).
"""
const DIAMOND2 = ConstrainedConnective{:◊,2}(>=)
const ◊₂ = DIAMOND2

"""
    const DIAMOND3 = ConstrainedConnective{:◊}(>=, 3)
    const ◊₃ = DIAMOND3

See [`DIAMOND2`](@ref), [`ConstrainedConnective`](@ref).
"""
const DIAMOND3 = ConstrainedConnective{:◊,3}(>=)
const ◊₃ = DIAMOND3

"""
    const ◊ₙ = ConstrainedConnective{:◊,n}(>=)

Generic [`ConstrainedConnective`](@ref) wrapping the lozenge glyph (◊).

See also [`ismodal`](@ref), [`isdiamond`](@ref), [`isbox`](@ref), [`arity`](@ref),
[`precedence`](@ref), [`associativity`](@ref), [`□ₙ`](@ref).
"""
const ◊ₙ(n::Int) = ConstrainedConnective{:◊,n}(>=)


ismodal(::ConstrainedConnective{:◊,N}) where {N} = true
isbox(::ConstrainedConnective{:◊,N}) where {N} = isbox(◊)
arity(::ConstrainedConnective{:◊,N}) where {N} = 1
precedence(::ConstrainedConnective{:◊,N}) where {N} = precedence(◊)
associativity(::ConstrainedConnective{:◊,N}) where {N} = associativity(◊)


"""
    const BOX2 = NamedConnective{:□}(<, 2)
    const □₂ = BOX2

Special [`BOX`](@ref) connective, stating that there are fewer than 2 worlds where a
formula does not hold (within an [`AbstractFrame`](@ref)).

The [`dual`](@ref) connective of `BOX2` is [`DIAMOND2`](@ref), as (¬◊₂¬φ) translates to
"it is not true that there are at least 2 accessible worlds whee not φ holds".

See [`Connective`](@ref), [`BOX`](@ref), [`ConstrainedConnective`](@ref), [`dual`](@ref).
"""
const BOX2 = ConstrainedConnective{:□,2}(<)
const □₂ = BOX2

"""
    const BOX3 = ConstrainedConnective{:◊,3}(<)
    const □₃ = BOX3

See [`BOX2`](@ref), [`ConstrainedConnective`](@ref).
"""
const BOX3 = ConstrainedConnective{:□,3}(<)
const □₃ = BOX3


"""
    const □ₙ = ConstrainedConnective{:□,n}(<)

Generic [`ConstrainedConnective`](@ref) wrapping the box glyph (□).

See also [`◊ₙ`](@ref).
"""
const □ₙ(n::Int) = ConstrainedConnective{:□,n}(<)

ismodal(::ConstrainedConnective{:□,N}) where {N} = ismodal(□)
isbox(::ConstrainedConnective{:□,N}) where {N} = isbox(□)
arity(::ConstrainedConnective{:□,N}) where {N} = arity(□)
precedence(::ConstrainedConnective{:□,N}) where {N} = precedence(□)
associativity(::ConstrainedConnective{:□,N}) where {N} = associativity(□)

hasdual(::ConstrainedConnective{:◊,N}) where {N} = true
dual(::typeof(DIAMOND2)) = BOX2
dual(::typeof(DIAMOND3)) = BOX3

hasdual(::ConstrainedConnective{:□,N}) where {N} = true
dual(::typeof(BOX2)) = DIAMOND2
dual(::typeof(BOX3)) = DIAMOND3


function _collateworlds(
    fr::AbstractFrame{W},
    op::ConstrainedConnective,
    (ws,)::NTuple{1,<:AbstractWorlds},
    aggregator::Function
) where {W<:AbstractWorld}
    filter(
        wi -> begin
            # for example, in the case of diamond, this variable encodes the collection of
            # worlds such that "op" is true on them, coming from wi
            _retrievedworlds = aggregator(fr, wi, ws)

            condition(op, _retrievedworlds |> length)
        end,
        fr |> allworlds |> collect
    )
end

function collateworlds(
    fr::AbstractFrame{W},
    op::ConstrainedConnective{:◊,N},
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld, N}
    # given the neighbors of a world, we want to retrieve the worlds in ws
    return _collateworlds(fr, op, (ws,), (fr, wi, ws) -> intersect(ws, accessibles(fr, wi)))
end

function collateworlds(
    fr::AbstractFrame{W},
    op::ConstrainedConnective{:□,N},
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld, N}
    # given the neighbors N of a world, we want to keep all the worlds where a certain
    # property is NOT true, so we pop off the elements of ws from N.
    return _collateworlds(fr, op, (ws,), (fr, wi, ws) -> setdiff(accessibles(fr, wi), ws))
end
