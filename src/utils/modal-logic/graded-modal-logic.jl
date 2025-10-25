
"""
    struct GradedConnective{Symbol} <: Connective

Connective enriched with a `grade`.

When evaluating a graded connective on a world `w` of an [`AbstractFrame`](@ref),
the neighbors of `w`, named `nw`, are considered only if `condition(nw, grade)` is true.

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

Base.show(gc::GradedConnective; kwargs...) = print(io, "$(syntaxstring(gc))")
syntaxstring(gc::GradedConnective; kwargs...) = (name(gc), grade(gc)) |> join


"""
    const DIAMOND2 = NamedConnective{:◊}(>=, 2)

See [`GradedConnective`](@ref).
"""
const DIAMOND2 = GradedConnective{:◊}(>=, 2)

"""
    const DIAMOND3 = GradedConnective{:◊}(>=, 3)

See [`GradedConnective`](@ref).
"""
const DIAMOND3 = GradedConnective{:◊}(>=, 3)
