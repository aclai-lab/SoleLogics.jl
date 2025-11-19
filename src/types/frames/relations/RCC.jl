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

julia> fr = FullDimensionalFrame((10,), Interval{Int});

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
