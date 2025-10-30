
"""
    struct ConstrainedConnective{Symbol} <: Connective

Connective enriched with a `threshold`.

When evaluating a constrained connective on a world `w` of an [`AbstractFrame`](@ref),
the neighbors of `w`, named `nw`, are considered only if `condition(nw, threshold)` is true.

Two examples of built-in `ConstrainedConnectives` are [`DIAMOND2`](@ref) and [`BOX2`](@ref).

[`DIAMOND2`](@ref) is a special diamond operator (see [`DIAMOND`](@ref)), stating that there
are at least 2 accessible worlds where a formula holds.

[`BOX2`](@ref) is a special box operator (see [`BOX`](@ref)), stating that there are at
most 2 accessible worlds where a formula does not hold (¬◊₂¬).

See also [`AbstractFrame`](@ref) [`Connective`](@ref), [`DIAMOND`](@ref),
[`NamedConnective`](@ref).
"""
struct ConstrainedConnective{S,N,F} <: Connective
    condition::F

    ConstrainedConnective{S,N}(condition::F) where {S,N,F<:Function} = new{S,N,F}(condition)

    ConstrainedConnective{S}(
        condition::Function,
        threshold::Int
    ) where {S} = new{S,threshold,typeof(condition)}(condition)

end

"""
    name(::ConstrainedConnective{S}) where {S} = S

Return the symbol encapsulated by a [`ConstrainedConnective`](@ref).

See also [`Connective`](@ref).
"""
name(::ConstrainedConnective{S,N}) where {S,N} = S

"""
    condition(gc::ConstrainedConnective) = gc.condition

Return a comparator wrapped within a [`ConstrainedConnective`](@ref).
It is a special function intended to compare the set of neighbors of a specific world within
 an [`AbstractFrame`](@ref), with respect to an integer `threshold`.

See also [`Connective`](@ref), [`DIAMOND`](@ref), [`DIAMOND2`](@ref), [`threshold`](@ref).
"""
condition(gc::ConstrainedConnective) = gc.condition

"""
    condition(gc::ConstrainedConnective, val::Int)

Shortcut for `condition(gc)(val, threshold(gc))`.

See also [`condition(gc::ConstrainedConnective)`](@ref), [`ConstrainedConnective`](@ref).
"""
condition(gc::ConstrainedConnective, val::Int) = condition(gc)(val, threshold(gc))


"""
    threshold(gc::ConstrainedConnective) = gc.threshold

Local argument of [`condition(gc::ConstrainedConnective)`](@ref).
"""
threshold(::ConstrainedConnective{S,N}) where {S,N} = N

syntaxstring(gc::ConstrainedConnective; kwargs...) = (name(gc), threshold(gc)) |> join
Base.show(io::IO, gc::ConstrainedConnective) = print(io, "$(syntaxstring(gc))")


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
    const ◊ₙ = DIAMOND2

This is just a placeholder for [`DIAMOND2`](@ref).
Semantically, you can use this to represent a generic [`ConstrainedConnective`](@ref) wrapping
the lozenge glyph.

When defining the traits for a `ConstrainedConnective{:◊}`, everything is forwarded from the
traits of `NamedConnective{:◊}` (whose placeholder is just `const ◊`, or [`DIAMOND`](@ref)).

See also [`ismodal`](@ref), [`isdiamond`](@ref), [`isbox`](@ref), [`arity`](@ref),
[`precedence`](@ref), [`associativity`](@ref), [`□ₙ`](@ref).
"""
const ◊ₙ = ◊₂

ismodal(::ConstrainedConnective{:◊,N}) where {N} = true
isbox(::ConstrainedConnective{:◊,N}) where {N} = isbox(◊)
arity(::ConstrainedConnective{:◊,N}) where {N} = 1
precedence(::ConstrainedConnective{:◊,N}) where {N} = precedence(◊)
associativity(::ConstrainedConnective{:◊,N}) where {N} = associativity(◊)


"""
    const BOX2 = NamedConnective{:□}(<=, 2)
    const □₂ = BOX2

Special [`BOX`](@ref) connective, stating that there are at most 2 accessible worlds
(within an [`AbstractFrame`](@ref), where a formula does not hold (¬◊₂¬).

See [`Connective`](@ref), [`BOX`](@ref), [`ConstrainedConnective`](@ref).
"""
const BOX2 = ConstrainedConnective{:□,2}(<=)
const □₂ = BOX2

"""
    const BOX3 = ConstrainedConnective{:◊,3}(>=)
    const □₃ = BOX3

See [`BOX2`](@ref), [`ConstrainedConnective`](@ref).
"""
const BOX3 = ConstrainedConnective{:□,3}(<=)
const □₃ = BOX3


"""
    const □ₙ = BOX2

Semantically expressive renaming of [`BOX2`](@ref).

See also [`◊ₙ`](@ref).
"""
const □ₙ = BOX2

ismodal(::ConstrainedConnective{:□,N}) where {N} = ismodal(□)
isbox(::ConstrainedConnective{:□,N}) where {N} = isbox(□)
arity(::ConstrainedConnective{:□,N}) where {N} = arity(□)
precedence(::ConstrainedConnective{:□,N}) where {N} = precedence(□)
associativity(::ConstrainedConnective{:□,N}) where {N} = associativity(□)

hasdual(::ConstrainedConnective{:◊,N}) where {N} = true
dual(::typeof(DIAMOND2)) = ConstrainedConnective{:□,1}(>) # beware, as this has a different
dual(::typeof(DIAMOND3)) = ConstrainedConnective{:□,2}(>) # condition w.r.t. □₂

hasdual(::typeof(□ₙ)) = true
dual(::typeof(BOX2)) = ConstrainedConnective{:◊,1}(<) # beware, as this has a different
dual(::typeof(BOX3)) = ConstrainedConnective{:◊,2}(<) # condition wrt ◊₂


function _collateworlds(
    fr::AbstractFrame{W},
    op::ConstrainedConnective,
    (ws,)::NTuple{1,<:AbstractWorlds},
    aggregator::Function
) where {W<:AbstractWorld}
    filter(
        w1 -> begin
            _from_w1_op_is_true_on_these_worlds = aggregator(ws, accessibles(fr, w1))
            condition(op, _from_w1_op_is_true_on_these_worlds |> length)
        end,
        fr |> allworlds |> collect
    )
end

function collateworlds(
    fr::AbstractFrame{W},
    op::ConstrainedConnective{:◊,N},
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld, N}
    return _collateworlds(fr, op, (ws,), intersect)
end

function collateworlds(
    fr::AbstractFrame{W},
    op::ConstrainedConnective{:□,N},
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld, N}
    # □p to ¬◊¬p
    collateworlds(fr, op |> NEGATION |> dual(op) |> NEGATION, (ws,))

    # this is not efficient, since a subset is computed both in issubset and setdiff
    # return _collateworlds(fr, op, (ws,), (a,b) -> issubset(a,b) ? b : setdiff(a,b))
end
