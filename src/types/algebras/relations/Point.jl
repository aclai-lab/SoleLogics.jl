"""1D Point relations"""
abstract type PointRelation <: GeometricalRelation end

arity(::PointRelation) = 2

"""2D Point relations (see [Compass logic](https://ieeexplore.ieee.org/abstract/document/8133753/))"""
abstract type Point2DRelation <: GeometricalRelation end

arity(::Point2DRelation) = 2
hasconverse(::Point2DRelation) = true
