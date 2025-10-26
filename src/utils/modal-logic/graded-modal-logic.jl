
"""
    struct GradedConnective{Symbol} <: Connective

Connective enriched with a `grade`.

When evaluating a graded connective on a world `w` of an [`AbstractFrame`](@ref),
the neighbors of `w`, named `nw`, are considered only if `condition(nw, grade)` is true.

Two examples of built-in `GradedConnectives` are [`DIAMOND2`](@ref) and [`BOX2`](@ref).

[`DIAMOND2`](@ref) is a special diamond operator (see [`DIAMOND`](@ref)), stating that there
are at least 2 accessible worlds where a formula holds.

[`BOX2`](@ref) is a special box operator (see [`BOX`](@ref)), stating that there are at
most 2 accessible worlds where a formula does not hold (¬◊₂¬).

See also [`AbstractFrame`](@ref) [`Connective`](@ref), [`DIAMOND`](@ref),
[`NamedConnective`](@ref).
"""
struct GradedConnective{S,N} <: Connective
    condition::Function

    GradedConnective{S,N}(condition::Function) where {S,N} = new{S,N}(condition)

    GradedConnective{S}(
        condition::Function,
        grade::Int
    ) where {S} = new{S,grade}(condition)

end

"""
    name(::GradedConnective{S}) where {S} = S

Return the symbol encapsulated by a [`GradedConnective`](@ref).

See also [`Connective`](@ref).
"""
name(::GradedConnective{S,N}) where {S,N} = S

"""
    condition(gc::GradedConnective) = gc.condition

Return a comparator wrapped within a [`GradedConnective`](@ref).
It is a special function intended to compare the set of neighbors of a specific world within
 an [`AbstractFrame`](@ref), with respect to a threshold called `grade`.

See also [`Connective`](@ref), [`DIAMOND`](@ref), [`DIAMOND2`](@ref), [`grade`](@ref).
"""
condition(gc::GradedConnective) = gc.condition

"""
    condition(gc::GradedConnective, val::Int)

Shortcut for `condition(gc)(val, grade(gc))`.

See also [`condition(gc::GradedConnective)`](@ref), [`GradedConnective`](@ref).
"""
condition(gc::GradedConnective, val::Int) = condition(gc)(val, grade(gc))


"""
    grade(gc::GradedConnective) = gc.grade

Local argument of [`condition(gc::GradedConnective)`](@ref).
"""
grade(::GradedConnective{S,N}) where {S,N} = N

syntaxstring(gc::GradedConnective; kwargs...) = (name(gc), grade(gc)) |> join
Base.show(io::IO, gc::GradedConnective) = print(io, "$(syntaxstring(gc))")


"""
    const DIAMOND2 = NamedConnective{:◊}(>=, 2)
    const ◊₂ = DIAMOND2

Special [`DIAMOND`](@ref) connective, stating that there are at least 2 accessible worlds
(within an [`AbstractFrame`](@ref) where a formula holds.

See [`Connective`](@ref), [`DIAMOND`](@ref), [`GradedConnective`](@ref).
"""
const DIAMOND2 = GradedConnective{:◊,2}(>=)
const ◊₂ = DIAMOND2

"""
    const DIAMOND3 = GradedConnective{:◊}(>=, 3)
    const ◊₃ = DIAMOND3

See [`DIAMOND2`](@ref), [`GradedConnective`](@ref).
"""
const DIAMOND3 = GradedConnective{:◊,3}(>=)
const ◊₃ = DIAMOND3

"""
    const ◊ₙ = DIAMOND2

This is just a placeholder for [`DIAMOND2`](@ref).
Semantically, you can use this to represent a generic [`GradedConnective`](@ref) wrapping
the lozenge glyph.

When defining the traits for a `GradedConnective{:◊}`, everything is forwarded from the
traits of `NamedConnective{:◊}` (whose placeholder is just `const ◊`, or [`DIAMOND`](@ref)).

See also [`ismodal`](@ref), [`isdiamond`](@ref), [`isbox`](@ref), [`arity`](@ref),
[`precedence`](@ref), [`associativity`](@ref), [`□ₙ`](@ref).
"""
const ◊ₙ = ◊₂

ismodal(::GradedConnective{:◊,N}) where {N} = true
isbox(::GradedConnective{:◊,N}) where {N} = isbox(◊)
arity(::GradedConnective{:◊,N}) where {N} = 1
precedence(::GradedConnective{:◊,N}) where {N} = precedence(◊)
associativity(::GradedConnective{:◊,N}) where {N} = associativity(◊)


"""
    const BOX2 = NamedConnective{:□}(<=, 2)
    const □₂ = BOX2

Special [`BOX`](@ref) connective, stating that there are at most 2 accessible worlds
(within an [`AbstractFrame`](@ref), where a formula does not hold (¬◊₂¬).

See [`Connective`](@ref), [`BOX`](@ref), [`GradedConnective`](@ref).
"""
const BOX2 = GradedConnective{:□,2}(<=)
const □₂ = BOX2

"""
    const BOX3 = GradedConnective{:◊,3}(>=)
    const □₃ = BOX3

See [`BOX2`](@ref), [`GradedConnective`](@ref).
"""
const BOX3 = GradedConnective{:□,3}(<=)
const □₃ = BOX3


"""
    const □ₙ = BOX2

Semantically expressive renaming of [`BOX2`](@ref).

See also [`◊ₙ`](@ref).
"""
const □ₙ = BOX2

ismodal(::GradedConnective{:□,N}) where {N} = ismodal(□)
isbox(::GradedConnective{:□,N}) where {N} = isbox(□)
arity(::GradedConnective{:□,N}) where {N} = arity(□)
precedence(::GradedConnective{:□,N}) where {N} = precedence(□)
associativity(::GradedConnective{:□,N}) where {N} = associativity(□)

hasdual(::GradedConnective{:◊,N}) where {N} = true
dual(::typeof(DIAMOND2)) = GradedConnective{:□,1}(>) # beware, as this has a different
dual(::typeof(DIAMOND3)) = GradedConnective{:□,2}(>) # condition w.r.t. □₂

hasdual(::typeof(□ₙ)) = true
dual(::typeof(BOX2)) = GradedConnective{:◊,1}(<) # beware, as this has a different
dual(::typeof(BOX3)) = GradedConnective{:◊,2}(<) # condition wrt ◊₂


function _collateworlds(
    fr::AbstractFrame{W},
    op::GradedConnective,
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
    op::GradedConnective{:◊,N},
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld, N}
    return _collateworlds(fr, op, (ws,), intersect)
end

function collateworlds(
    fr::AbstractFrame{W},
    op::GradedConnective{:□,N},
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld, N}
    collateworlds(fr, dual(op), (ws,))

    # this is not efficient, since a subset is computed both in issubset and setdiff
    # return _collateworlds(fr, op, (ws,), (a,b) -> issubset(a,b) ? b : setdiff(a,b))
end
