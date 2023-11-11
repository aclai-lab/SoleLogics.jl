############################################################################################
# Allen's Interval Algebra relations
############################################################################################

doc_IntervalRelation = """
    abstract type IntervalRelation <: GeometricalRelation end

Abstract type for interval binary relations.
Originally defined by Allen in 1983,
[interval algebra](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra)
comprehends 12 directional relations between intervals,
plus the identity (i.e., `identityrel`).

The 12 relations are
the 6 relations `after`, `later`, `begins`, `ends`, `during`, `overlaps`,
and their inverses.

If we consider a reference interval `(x,y)`, we can graphically represent the 6
base relations by providing an example of a world `(z,t)` that is accessible via each
of them:

RELATION    ABBR.     x                   y                     PROPERTY                  
                      |-------------------|                                               
                      .                   .                                               
                      .                   z        t            y = z                     
After       (A)       .                   |--------|                                      
                      .                   .                                               
                      .                   .   z         t       y < z                     
Later       (L)       .                   .   |---------|                                 
                      .                   .                                               
                      z     t             .                     x = z, t < y              
Begins      (B)       |-----|             .                                               
                      .                   .                                               
                      .             z     t                     y = t, x < z              
Ends        (E)       .             |-----|                                               
                      .                   .                                               
                      .   z        t      .                     x < z, t < y              
During      (D)       .   |--------|      .                                               
                      .                   .                                               
                      .           z       .    t                x < z < y < t             
Overlaps    (O)       .           |------------|                                          

Coarser relations can be defined by union of these 12 relations.

# Examples
```julia-repl
julia> IARelations
12-element Vector{IntervalRelation}:
 _IA_A()
 _IA_L()
 _IA_B()
 _IA_E()
 _IA_D()
 _IA_O()
 _IA_Ai()
 _IA_Li()
 _IA_Bi()
 _IA_Ei()
 _IA_Di()
 _IA_Oi()

julia> @assert SoleLogics._IA_L() == IA_L

julia> fr = SoleLogics.FullDimensionalFrame((10,),);

julia> collect(accessibles(fr, Interval(2,5), IA_L))
15-element Vector{Interval{Int64}}:
 (6−7)
 (6−8)
 (7−8)
 (6−9)
 (7−9)
 (8−9)
 (6−10)
 (7−10)
 (8−10)
 (9−10)
 (6−11)
 (7−11)
 (8−11)
 (9−11)
 (10−11)

julia> syntaxstring.(IARelations)
12-element Vector{String}:
 "A"
 "L"
 "B"
 "E"
 "D"
 "O"
 "A̅"
 "L̅"
 "B̅"
 "E̅"
 "D̅"
 "O̅"

julia> syntaxstring.(IA7Relations)
6-element Vector{String}:
 "A∨O"
 "L"
 "D∨B∨E"
 "A̅∨O̅"
 "L̅"
 "D̅∨B̅∨E̅"

julia> syntaxstring.(SoleLogics.IA3Relations)
3-element Vector{String}:
 "I"
 "L"
 "L̅"

```

See also [`IARelations`](@ref),
[`IA7Relations`](@ref), [`IA3Relations`](@ref),
[`Interval`](@ref), [`GeometricalRelation`](@ref).
"""

"""$(doc_IntervalRelation)"""
abstract type IntervalRelation <: GeometricalRelation end

arity(::IntervalRelation) = 2
hasconverse(::IntervalRelation) = true

struct _IA_A  <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_A  = _IA_A();  # After
struct _IA_L  <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_L  = _IA_L();  # Later
struct _IA_B  <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_B  = _IA_B();  # Begins
struct _IA_E  <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_E  = _IA_E();  # Ends
struct _IA_D  <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_D  = _IA_D();  # During
struct _IA_O  <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_O  = _IA_O();  # Overlaps

struct _IA_Ai <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_Ai = _IA_Ai(); # After inverse
struct _IA_Li <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_Li = _IA_Li(); # Later inverse
struct _IA_Bi <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_Bi = _IA_Bi(); # Begins inverse
struct _IA_Ei <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_Ei = _IA_Ei(); # Ends inverse
struct _IA_Di <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_Di = _IA_Di(); # During inverse
struct _IA_Oi <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_Oi = _IA_Oi(); # Overlaps inverse

syntaxstring(::_IA_A; kwargs...)  = "A"
syntaxstring(::_IA_L; kwargs...)  = "L"
syntaxstring(::_IA_B; kwargs...)  = "B"
syntaxstring(::_IA_E; kwargs...)  = "E"
syntaxstring(::_IA_D; kwargs...)  = "D"
syntaxstring(::_IA_O; kwargs...)  = "O"
syntaxstring(::_IA_Ai; kwargs...) = "A̅"
syntaxstring(::_IA_Li; kwargs...) = "L̅"
syntaxstring(::_IA_Bi; kwargs...) = "B̅"
syntaxstring(::_IA_Ei; kwargs...) = "E̅"
syntaxstring(::_IA_Di; kwargs...) = "D̅"
syntaxstring(::_IA_Oi; kwargs...) = "O̅"

# Properties
istransitive(r::_IA_L) = true
istransitive(r::_IA_Li) = true
istransitive(r::_IA_D) = true
istransitive(r::_IA_Di) = true
istransitive(r::_IA_B) = true
istransitive(r::_IA_Bi) = true
istransitive(r::_IA_E) = true
istransitive(r::_IA_Ei) = true
istopological(r::_IA_D) = true
istopological(r::_IA_Di) = true

converse(::_IA_A) = IA_Ai
converse(::_IA_L) = IA_Li
converse(::_IA_B) = IA_Bi
converse(::_IA_E) = IA_Ei
converse(::_IA_D) = IA_Di
converse(::_IA_O) = IA_Oi
converse(::_IA_Ai) = IA_A
converse(::_IA_Li) = IA_L
converse(::_IA_Bi) = IA_B
converse(::_IA_Ei) = IA_E
converse(::_IA_Di) = IA_D
converse(::_IA_Oi) = IA_O

############################################################################################

# Coarser relations: IA7
struct _IA_AorO       <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_AorO       = _IA_AorO();       # After ∪ Overlaps
struct _IA_DorBorE    <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_DorBorE    = _IA_DorBorE();    # During ∪ Begins ∪ Ends
struct _IA_AiorOi     <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_AiorOi     = _IA_AiorOi();     # (After ∪ Overlaps) inverse
struct _IA_DiorBiorEi <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_DiorBiorEi = _IA_DiorBiorEi(); # (During ∪ Begins ∪ Ends) inverse

# Even coarser relations: IA3
struct _IA_I          <: IntervalRelation end; """See [`IntervalRelation`](@ref)""" const IA_I          = _IA_I();   # Intersecting (ABEDO ∪ ABEDO inverse)

converse(::_IA_AorO) = IA_AiorOi
converse(::_IA_DorBorE) = IA_DiorBiorEi
converse(::_IA_AiorOi) = IA_AorO
converse(::_IA_DiorBiorEi) = IA_DorBorE
converse(::_IA_I) = IA_I

# Properties
istransitive(r::_IA_DorBorE) = true
istransitive(r::_IA_DiorBiorEi) = true
istopological(r::_IA_I) = true

IA72IARelations(::_IA_AorO)       = [IA_A,  IA_O]
IA72IARelations(::_IA_AiorOi)     = [IA_Ai, IA_Oi]
IA72IARelations(::_IA_DorBorE)    = [IA_D,  IA_B,  IA_E]
IA72IARelations(::_IA_DiorBiorEi) = [IA_Di, IA_Bi, IA_Ei]
IA32IARelations(::_IA_I)          = [
    IA_A,  IA_O,  IA_D,  IA_B,  IA_E,
    IA_Ai, IA_Oi, IA_Di, IA_Bi, IA_Ei
]

syntaxstring(r::Union{_IA_AorO,_IA_DorBorE,_IA_AiorOi,_IA_DiorBiorEi}; kwargs...) = join(map(_r->syntaxstring(_r; kwargs...), IA72IARelations(r)), "∨")
syntaxstring(::_IA_I; kwargs...)          = "I"

############################################################################################

"""
    const IARelations = [IA_A,  IA_L,  IA_B,  IA_E,  IA_D,  IA_O,
                         IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi]

Vector of the 12 interval relations from Allen's interval algebra.

See also
[`IA7Relations`](@ref), [`IA3Relations`](@ref), 
[`IntervalRelation`](@ref), [`GeometricalRelation`](@ref).
"""
const IARelations = [IA_A,  IA_L,  IA_B,  IA_E,  IA_D,  IA_O,
                     IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi]
IARelation = Union{typeof.(IARelations)...}

"""
    const IA7Relations = [IA_AorO,   IA_L,  IA_DorBorE,
                          IA_AiorOi, IA_Li, IA_DiorBiorEi]

Vector of 7 interval relations from a coarser version of Allen's interval algebra.

See also
[`IARelations`](@ref), [`IA3Relations`](@ref), 
[`IntervalRelation`](@ref), [`GeometricalRelation`](@ref).
"""
const IA7Relations = [IA_AorO,   IA_L,  IA_DorBorE,
                      IA_AiorOi, IA_Li, IA_DiorBiorEi]
IA7Relation = Union{typeof.(IA7Relations)...}

"""
    const IA3Relations = [IA_I, IA_L, IA_Li]

Vector of 3 interval relations from a coarser version of Allen's interval algebra.

See also
[`IARelations`](@ref), [`IA7Relations`](@ref), 
[`IntervalRelation`](@ref), [`GeometricalRelation`](@ref).
"""
const IA3Relations = [IA_I, IA_L, IA_Li]
IA3Relation = Union{typeof.(IA3Relations)...}

"""
    const IARelations_extended = [globalrel, IARelations...]

13 Interval Algebra extended with the global relation.
"""
const IARelations_extended = [globalrel, IARelations...]
IARelation_extended = Union{typeof.(IARelations_extended)...}

