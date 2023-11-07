
"""TODO document"""
abstract type PointRelation <: GeometricalRelation end

arity(::PointRelation) = 2

struct _MinRel        <: PointRelation end; const MinRel         = _MinRel();         # Minimum
struct _MaxRel        <: PointRelation end; const MaxRel         = _MaxRel();         # Maximum

struct _SuccessorRel   <: PointRelation end; const SuccessorRel   = _SuccessorRel();   # Successor
struct _PredecessorRel <: PointRelation end; const PredecessorRel = _PredecessorRel(); # Predecessor
struct _GreaterRel     <: PointRelation end; const GreaterRel     = _GreaterRel();     # Greater
struct _LesserRel    <: PointRelation end; const LesserRel    = _LesserRel();    # Lesser


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

isfunctional(::_MinRel) = true
isfunctional(::_MaxRel) = true
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
