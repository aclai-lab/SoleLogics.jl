
# Abstract type for relations with a geometrical interpretation
abstract type GeometricalRelation <: AbstractRelation end

# Geometrical relations can have geometrical properties such as being topological (i.e.,
#  invariant under homeomorphisms. # see https://en.m.wikipedia.org/wiki/Topological_property
# By default, this does not hold:
istopological(r::GeometricalRelation) = false

# All geometrical relations must define their converse...?
# hasconverse(::Type{<:GeometricalRelation}) = true

arity(::Type{<:GeometricalRelation}) = 2

# TODO add pattern: carthesian product of relations

# 1D Allen relations
include("IA.jl")

# 2D Allen relations
include("IA2.jl")

# RCC relations
include("RCC.jl")
