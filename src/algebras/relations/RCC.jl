############################################################################################
# RCC topological relations
############################################################################################

"""
    abstract type RCCRelation <: GeometricalRelation end

Topological binary relations from
[Region Connection Calculus](https://en.wikipedia.org/wiki/Region_connection_calculus).
Region Connection Calculus (RCC) is most famous for RCC8, a set of 8 topological relations,
which comprehends the identity relation (i.e., `identityrel'), and the following 7 relations:
- Externally connected;
- Partially overlapping;
- Tangential proper part;
- Tangential proper part inverse;
- Non-tangential proper part;
- Non-tangential proper part inverse.

If we consider a reference interval `(x−y)`, we can graphically represent the 7
 relations by providing an example of a world `(z−t)` that is accessible via each
of them:

| Relation    | Full name                                |       Graphical Representation w.r.t (x−y) |
| :---------- |:-----------------------------------------|--------------------------------------------|
|             |                                          |`___x___________________y__________________`|
|             |                                          |`___∣−−−−−−−−−−−−−−−−−−−∣__________________`|
|             |                                          |`___.___________________.__________________`|
|             |                                          |`___.___________________.__z________t______`|
|    DC       | Disconnected                             |`___.___________________._∣−−−−−−−−∣_______`|
|             |                                          |`___.___________________.__________________`|
|             |                                          |`___.___________________z_________t________`|
|    EC       | Externally connected                     |`___.___________________∣−−−−−−−−−∣________`|
|             |                                          |`___.___________________.__________________`|
|             |                                          |`___.________________z_____t_______________`|
|    PO       | Partially overlapping                    |`___.________________∣−−−−−∣_______________`|
|             |                                          |`___.___________________.__________________`|
|             |                                          |`___._____________z_____t__________________`|
|    TPP      | Tangential proper part                   |`___._____________∣−−−−−∣__________________`|
|             |                                          |`___.___________________.__________________`|
|             |                                          |`___z___________________._____t____________`|
|    TPPi     | Tangential proper part inverse           |`___∣−−−−−−−−−−−−−−−−−−−−−−−−−∣____________`|
|             |                                          |`___.___________________.__________________`|
|             |                                          |`___.___________z_______.__________________`|
|    NTPP     | Non-tangential proper part               |`___.___________∣−−−−−∣_.__________________`|
|             |                                          |`___.___________________.__________________`|
|             |                                          |`_z_.___________________._t________________`|
|    NTPPi    | Non-tangential proper part inverse       |`_∣−−−−−−−−−−−−−−−−−−−−−−−∣________________`|


Methods for RCC8 relations and Interval2D's can be obtained by combining their 1D versions,
according to the following composition rules:

|      |  DC | EC | PO | TPP | TPP | NTPP | NTPP |  Id  |
|:-----|:----|:---|:---|:----|:----|:-----|:-----|:-----|
| DC   |  DC | DC | DC | DC  | DC  |  DC  |  DC  |  DC  |
| EC   |  DC | EC | EC | EC  | EC  |  EC  |  EC  |  EC  |
| PO   |  DC | EC | PO | PO  | PO  |  PO  |  PO  |  PO  |
| TPP  |  DC | EC | PO | TPP | PO  |  TPP |  PO  |  TPP |
| TPPi |  DC | EC | PO | PO  | TPP |  PO  |  TPP |  TPP |
| NTPP |  DC | EC | PO | TPP | PO  | NTPP |  PO  |  TPP |
| NTPPi|  DC | EC | PO | PO  | TPP |  PO  | NTPP |  TPP |
|  Id  |  DC | EC | PO | TPP | TPP |  TPP |  TPP |  Id  |


# Examples
```julia-repl
julia> RCC8Relations
7-element Vector{RCCRelation}:
 _Topo_DC()
 _Topo_EC()
 _Topo_PO()
 _Topo_TPP()
 _Topo_TPPi()
 _Topo_NTPP()
 _Topo_NTPPi()

julia> @assert SoleLogics._Topo_DC() == Topo_DC

julia> fr = SoleLogics.FullDimensionalFrame((10,),);

julia> collect(accessibles(fr, Interval(4,8), Topo_DC))
6-element Vector{Interval{Int64}}:
 (9−10)
 (9−11)
 (10−11)
 (1−2)
 (1−3)
 (2−3)

julia> syntaxstring.(RCC8Relations)
7-element Vector{String}:
 "DC"
 "EC"
 "PO"
 "TPP"
 "T̅P̅P̅"
 "NTPP"
 "N̅T̅P̅P̅"

julia> RCC5Relations
4-element Vector{RCCRelation}:
 _Topo_DR()
 _Topo_PO()
 _Topo_PP()
 _Topo_PPi()
```

See also 
[`RCC8Relations`](@ref), [`RCC5Relations`](@ref),
[`Interval`](@ref), [`IntervalRelation`](@ref), [`GeometricalRelation`](@ref).
"""
abstract type RCCRelation <: GeometricalRelation end

arity(::RCCRelation) = 2
hasconverse(::RCCRelation) = true

# Property: all RCC relations are topological
istopological(r::RCCRelation) = true

# Relations for RCC8
struct _Topo_DC     <: RCCRelation end; const Topo_DC     = _Topo_DC();     # Disconnected
struct _Topo_EC     <: RCCRelation end; const Topo_EC     = _Topo_EC();     # Externally connected
struct _Topo_PO     <: RCCRelation end; const Topo_PO     = _Topo_PO();     # Partially overlapping
struct _Topo_TPP    <: RCCRelation end; const Topo_TPP    = _Topo_TPP();    # Tangential proper part
struct _Topo_TPPi   <: RCCRelation end; const Topo_TPPi   = _Topo_TPPi();   # Tangential proper part inverse
struct _Topo_NTPP   <: RCCRelation end; const Topo_NTPP   = _Topo_NTPP();   # Non-tangential proper part
struct _Topo_NTPPi  <: RCCRelation end; const Topo_NTPPi  = _Topo_NTPPi();  # Non-tangential proper part inverse

syntaxstring(::_Topo_DC; kwargs...)    = "DC"
syntaxstring(::_Topo_EC; kwargs...)    = "EC"
syntaxstring(::_Topo_PO; kwargs...)    = "PO"
syntaxstring(::_Topo_TPP; kwargs...)   = "TPP"
syntaxstring(::_Topo_TPPi; kwargs...)  = "T̅P̅P̅"
syntaxstring(::_Topo_NTPP; kwargs...)  = "NTPP"
syntaxstring(::_Topo_NTPPi; kwargs...) = "N̅T̅P̅P̅"

# Properties
converse(r::_Topo_DC) = Topo_DC
converse(r::_Topo_EC) = Topo_EC
converse(r::_Topo_PO) = Topo_PO
converse(r::_Topo_TPP) = Topo_TPPi
converse(r::_Topo_TPPi) = Topo_TPP
converse(r::_Topo_NTPP) = Topo_NTPPi
converse(r::_Topo_NTPPi) = Topo_NTPP

issymmetric(r::_Topo_DC) = true
issymmetric(r::_Topo_EC) = true
issymmetric(r::_Topo_PO) = true
istransitive(r::_Topo_NTPP) = true
istransitive(r::_Topo_NTPPi) = true

############################################################################################

# Coarser relations for RCC5
struct _Topo_DR     <: RCCRelation end; const Topo_DR     = _Topo_DR();     # Disjointed
struct _Topo_PP     <: RCCRelation end; const Topo_PP     = _Topo_PP();     # Proper part
struct _Topo_PPi    <: RCCRelation end; const Topo_PPi    = _Topo_PPi();    # Proper part inverse

syntaxstring(::_Topo_DR; kwargs...)    = "DR"
syntaxstring(::_Topo_PP; kwargs...)    = "PP"
syntaxstring(::_Topo_PPi; kwargs...)   = "P̅P̅"

# Properties
converse(r::_Topo_DR) = Topo_DR
converse(r::_Topo_PP) = Topo_PPi
converse(r::_Topo_PPi) = Topo_PP
issymmetric(r::_Topo_DR) = true
istransitive(r::_Topo_PP) = true
istransitive(r::_Topo_PPi) = true
############################################################################################

"""
    const RCC8Relations = [Topo_DC, Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi]

Vector of the 7 relations from RCC8.

See also
[`RCC5Relations`](@ref), 
[`GeometricalRelation`](@ref).
"""
const RCC8Relations = [Topo_DC, Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi]
RCC8Relation = Union{typeof.(RCC8Relations)...}

"""
    const RCC5Relations = [Topo_DR, Topo_PO, Topo_PP, Topo_PPi]

Vector of the 4 relations from RCC5.

See also
[`RCC5Relations`](@ref), 
[`GeometricalRelation`](@ref).
"""
const RCC5Relations = [Topo_DR, Topo_PO, Topo_PP, Topo_PPi]
RCC5Relation = Union{typeof.(RCC5Relations)...}

############################################################################################

# It is conveniente to define RCC relations as unions of IA relations
const RCC8RelationFromIA = Union{_Topo_DC,_Topo_EC,_Topo_PO,_Topo_TPP,_Topo_TPPi}

topo2IARelations(::_Topo_DC)     = [IA_L,  IA_Li]
topo2IARelations(::_Topo_EC)     = [IA_A,  IA_Ai]
topo2IARelations(::_Topo_PO)     = [IA_O,  IA_Oi]
topo2IARelations(::_Topo_TPP)    = [IA_B,  IA_E]
topo2IARelations(::_Topo_TPPi)   = [IA_Bi, IA_Ei]
topo2IARelations(::_Topo_NTPP)   = [IA_D]
topo2IARelations(::_Topo_NTPPi)  = [IA_Di]

# TODO RCC5 can be better written as a combination of IA7 relations!
const RCC5RelationFromRCC8 = Union{_Topo_DR,_Topo_PP,_Topo_PPi}
RCC52RCC8Relations(::_Topo_DR)   = [Topo_DC,    Topo_EC]
RCC52RCC8Relations(::_Topo_PP)   = [Topo_TPP,   Topo_NTPP]
RCC52RCC8Relations(::_Topo_PPi)  = [Topo_TPPi,  Topo_NTPPi]
