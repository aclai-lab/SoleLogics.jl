
"""
    struct GradedConnective{Symbol} <: Connective

Connective enriched with a `grade`.

When evaluating a graded connective on a world `w` of an [`AbstractFrame`](@ref),
the neighbors of `w`, named `nw`, are considered only if `condition(nw, grade)` is true.

Two examples of built-in `GradedConnectives` are [`DIAMOND2`](@ref) and [`BOX2`](@ref).

[`DIAMOND2`](@ref) is a special diamond operator (see [`DIAMOND`](@ref)), stating that there
are at least 2 accessible worlds where a formula holds.

[`BOX2`](@ref) is a special box operator (see [`BOX`](@ref)), stating that there are at
most 2 accessible worlds where a formula does not holds (¬◊₂¬).

See also [`AbstractFrame`](@ref) [`Connective`](@ref), [`DIAMOND`](@ref),
[`NamedConnective`](@ref).
"""
struct GradedConnective{S} <: Connective
    condition::Function
    grade::Int

    GradedConnective{S}(grade::Int) where {S} = new{S}(>=, grade)
    GradedConnective{S}(
        condition::Function,
        grade::Int
    ) where {S} = new{S}(condition, grade)
end

"""
    name(::GradedConnective{S}) where {S} = S

Return the symbol encapsulated by a [`GradedConnective`](@ref).

See also [`Connective`](@ref).
"""
name(::GradedConnective{S}) where {S} = S


"""
    condition(gc::GradedConnective) = gc.condition

Return the conditionition within a [`GradedConnective`](@ref).
The condition is a special function `f` intended to compare the set of neighbors of a
specific world within an [`AbstractFrame`](@ref).

The difference between ◊ (i.e., DIAMOND) and ◊₂ (i.e., DIAMOND2) is that the second one
states that *at least two* neighbors of a world exists.

See also [`Connective`](@ref), [`DIAMOND`](@ref), [`DIAMOND2`](@ref).
"""
condition(gc::GradedConnective) = gc.condition

"""
    grade(gc::GradedConnective) = gc.grade

Local argument of [`condition(gc::GradedConnective)`](@ref).
"""
grade(gc::GradedConnective) = gc.grade

syntaxstring(gc::GradedConnective; kwargs...) = (name(gc), grade(gc)) |> join
Base.show(io::IO, gc::GradedConnective) = print(io, "$(syntaxstring(gc))")


"""
    const DIAMOND2 = NamedConnective{:◊}(>=, 2)
    const ◊₂ = DIAMOND2

See [`GradedConnective`](@ref).
"""
const DIAMOND2 = GradedConnective{:◊}(>=, 2)
const ◊₂ = DIAMOND2

"""
    const DIAMOND3 = GradedConnective{:◊}(>=, 3)
    const ◊₃ = DIAMOND3

See [`GradedConnective`](@ref).
"""
const DIAMOND3 = GradedConnective{:◊}(>=, 3)
const ◊₃ = DIAMOND3

"""
    const ◊ₙ = DIAMOND2

This is just a placeholder for [`DIAMOND2`](@ref).
Semantically, you can use this to represent a generic [`GradedConnective`](@ref) wrapping
the lozenge glyph.

When defining the traits for a `GradedConnective{:◊}`, everything is forwarded from the
traits of `NamedConnective{:◊}` (whose placeholder is just `const ◊`, or
[`DIAMOND`](@ref)).

See also [`ismodal`](@ref), [`isdiamond`](@ref), [`isbox`](@ref), [`arity`](@ref),
[`precedence`](@ref), [`associativity`](@ref).
"""
const ◊ₙ = ◊₂

ismodal(::Type{typeof(◊ₙ)}) = ismodal(◊)
ismodal(::Type{typeof(◊ₙ)}) = ismodal(◊)
isbox(::Type{typeof(◊ₙ)}) = isbox(◊)
arity(::typeof(◊ₙ)) = arity(◊)
precedence(::typeof(◊ₙ)) = precedence(◊)
associativity(::typeof(◊ₙ)) = associativity(◊)



"""
    const BOX2 = NamedConnective{:□}(<=, 2)
    const □₂ = BOX2

See [`GradedConnective`](@ref).
"""
const BOX2 = GradedConnective{:□}(<=, 2)
const □₂ = BOX2

"""
    const BOX3 = GradedConnective{:◊}(>=, 3)
    const □₃ = BOX3

See [`GradedConnective`](@ref).
"""
const BOX3 = GradedConnective{:◊}(>=, 3)
const □₃ = BOX3


"""
    const □ₙ = BOX2

This has the same exact purpose of [`◊ₙ`](@ref).
"""
const □ₙ = BOX2

ismodal(::Type{typeof(□ₙ)}) = ismodal(□)
ismodal(::Type{typeof(□ₙ)}) = ismodal(□)
isbox(::Type{typeof(□ₙ)}) = isbox(□)
arity(::typeof(□ₙ)) = arity(□)
precedence(::typeof(□ₙ)) = precedence(□)
associativity(::typeof(□ₙ)) = associativity(□)
