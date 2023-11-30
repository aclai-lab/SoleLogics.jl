
"""1D Point relations"""
abstract type PointRelation <: GeometricalRelation end

arity(::PointRelation) = 2

"""Relation leading to the *minimum* `Point` (i.e., the *least* in the linear order)."""
struct _MinRel         <: PointRelation end; const MinRel         = _MinRel();         # Minimum

"""Relation leading to the *maximum* `Point` (i.e., the *greatest* in the linear order)."""
struct _MaxRel         <: PointRelation end; const MaxRel         = _MaxRel();         # Maximum

"""Relation leading to the *successor* `Point` (i.e., the *next* in the linear order)."""
struct _SuccessorRel   <: PointRelation end; const SuccessorRel   = _SuccessorRel();   # Successor
"""Relation leading to the *predecessor* `Point` (i.e., the *previous* in the linear order)."""
struct _PredecessorRel <: PointRelation end; const PredecessorRel = _PredecessorRel(); # Predecessor
"""Relation leading to the *greater* `Point`s in the linear order."""
struct _GreaterRel     <: PointRelation end; const GreaterRel     = _GreaterRel();     # Greater
"""Relation leading to the *lesser* `Point`s in the linear order."""
struct _LesserRel      <: PointRelation end; const LesserRel      = _LesserRel();      # Lesser


hasconverse(::_SuccessorRel) = true
converse(::_SuccessorRel) = PredecessorRel
hasconverse(::_PredecessorRel) = true
converse(::_PredecessorRel) = SuccessorRel
hasconverse(::_GreaterRel) = true
converse(::_GreaterRel) = LesserRel
hasconverse(::_LesserRel) = true
converse(::_LesserRel) = GreaterRel

istransitive(::_SuccessorRel) = true
istransitive(::_PredecessorRel) = true
istransitive(::_GreaterRel) = true
istransitive(::_LesserRel) = true

istoone(::_MinRel) = true
istoone(::_MaxRel) = true
isgrounding(::_MinRel) = true
isgrounding(::_MaxRel) = true
istransitive(::_MinRel) = true
istransitive(::_MaxRel) = true

syntaxstring(::_SuccessorRel; kwargs...) = "X"
syntaxstring(::_PredecessorRel; kwargs...) = "X̅"
syntaxstring(::_GreaterRel; kwargs...) = ">"
syntaxstring(::_LesserRel; kwargs...) = "<"
syntaxstring(::_MinRel; kwargs...) = "min"
syntaxstring(::_MaxRel; kwargs...) = "max"

"""
Vector of 6 point relations: min, max, successor, predecessor, >, <.

See also
[`Point2DRelations`](@ref).
"""
const PointRelations = [MinRel, MaxRel, SuccessorRel, PredecessorRel, GreaterRel, LesserRel]


"""2D Point relations (see [Compass logic](https://ieeexplore.ieee.org/abstract/document/8133753/))"""
abstract type Point2DRelation <: GeometricalRelation end

arity(::Point2DRelation) = 2
hasconverse(::Point2DRelation) = true

struct _CL_N  <: Point2DRelation end; """Relation leading to the closest northern `Point2D`.""" const CL_N  = _CL_N();  # North
struct _CL_S  <: Point2DRelation end; """Relation leading to the closest southern `Point2D`.""" const CL_S  = _CL_S();  # South
struct _CL_E  <: Point2DRelation end; """Relation leading to the closest eastern `Point2D`.""" const CL_E  = _CL_E();  # East
struct _CL_W  <: Point2DRelation end; """Relation leading to the closest western `Point2D`.""" const CL_W  = _CL_W();  # West

syntaxstring(::_CL_N; kwargs...) = "N"
istransitive(r::_CL_N) = true
converse(::typeof(CL_N)) = CL_S

syntaxstring(::_CL_S; kwargs...) = "S"
istransitive(r::_CL_S) = true
converse(::typeof(CL_S)) = CL_N

syntaxstring(::_CL_E; kwargs...) = "E"
istransitive(r::_CL_E) = true
converse(::typeof(CL_E)) = CL_W

syntaxstring(::_CL_W; kwargs...) = "W"
istransitive(r::_CL_W) = true
converse(::typeof(CL_W)) = CL_E

struct _CL_NE  <: Point2DRelation end; """Relation leading to the closest north-eastern `Point2D`.""" const CL_NE  = _CL_NE();  # North-East
struct _CL_NW  <: Point2DRelation end; """Relation leading to the closest north-western `Point2D`.""" const CL_NW  = _CL_NW();  # North-West
struct _CL_SE  <: Point2DRelation end; """Relation leading to the closest south-eastern `Point2D`.""" const CL_SE  = _CL_SE();  # South-East
struct _CL_SW  <: Point2DRelation end; """Relation leading to the closest south-western `Point2D`.""" const CL_SW  = _CL_SW();  # South-West

syntaxstring(::_CL_NE; kwargs...) = "NE"
istransitive(r::_CL_NE) = true
converse(::typeof(CL_NE)) = SW

syntaxstring(::_CL_NW; kwargs...) = "NW"
istransitive(r::_CL_NW) = true
converse(::typeof(CL_NW)) = SE

syntaxstring(::_CL_SE; kwargs...) = "SE"
istransitive(r::_CL_SE) = true
converse(::typeof(CL_SE)) = NW

syntaxstring(::_CL_SW; kwargs...) = "SW"
istransitive(r::_CL_SW) = true
converse(::typeof(CL_SW)) = NE

"""
Vector of 8 cardinal relations
from [Compass logic](https://ieeexplore.ieee.org/abstract/document/8133753/):
North, South, North-West, etc.

See also
[`PointRelations`](@ref).
"""
const Point2DRelations = [CL_N, CL_S, CL_E, CL_W, CL_NE, CL_NW, CL_SE, CL_SW]
