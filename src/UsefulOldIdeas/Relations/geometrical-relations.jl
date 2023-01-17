
# Abstract type for relations with a geometrical interpretation
abstract type GeometricalRelation <: AbstractRelation end

# Geometrical relations can have geometrical properties such as being topological (i.e.,
#  invariant under homeomorphisms. # see https://en.m.wikipedia.org/wiki/Topological_property
# By default, this does not hold:
is_topological(r::GeometricalRelation) = false

arity(::Type{<:GeometricalRelation}) = 2

# TODO add pattern: combinazione cartesiana/prodotto di relazioni.

# 1D Allen relations
include("IA.jl")

# 2D Allen relations
include("IA2.jl")

# RCC relations
include("RCC.jl")
