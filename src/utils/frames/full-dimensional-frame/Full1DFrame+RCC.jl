
# Refer to [`RCCRelation`](@ref).

# Enumerate accessible worlds from a single world
# _accessibles(fr::Full1DFrame, w::Interval, r::RCC8RelationFromIA) = Iterators.flatten((_accessibles(fr, w, IA_r) for IA_r in topo2IARelations(r)))
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_DC) = Iterators.flatten((_accessibles(fr, w, IA_L), _accessibles(fr, w, IA_Li)))
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_EC) = Iterators.flatten((_accessibles(fr, w, IA_A), _accessibles(fr, w, IA_Ai)))
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_PO) = Iterators.flatten((_accessibles(fr, w, IA_O), _accessibles(fr, w, IA_Oi)))
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_TPP) = Iterators.flatten((_accessibles(fr, w, IA_B), _accessibles(fr, w, IA_E)))
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_TPPi) = Iterators.flatten((_accessibles(fr, w, IA_Bi), _accessibles(fr, w, IA_Ei)))
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_NTPP) = _accessibles(fr, w, IA_D)
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_NTPPi) = _accessibles(fr, w, IA_Di)

# RCC5 computed as a combination
# _accessibles(fr::Full1DFrame, w::Interval, r::RCC5RelationFromRCC8) =
#     Iterators.flatten((_accessibles(Full1DFrame(fr), w, RCC8_r, ) for RCC8_r in RCC52RCC8Relations(r)))

# RCC5 computed standalone
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_DR) = Iterators.flatten((_accessibles(fr, w, IA_L), _accessibles(fr, w, IA_Li), _accessibles(fr, w, IA_A), _accessibles(fr, w, IA_Ai)))
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_PP) = Iterators.flatten((_accessibles(fr, w, IA_B), _accessibles(fr, w, IA_E), _accessibles(fr, w, IA_D)))
_accessibles(fr::Full1DFrame, w::Interval, ::_Topo_PPi) = Iterators.flatten((_accessibles(fr, w, IA_Bi), _accessibles(fr, w, IA_Ei), _accessibles(fr, w, IA_Di)))

# copied from RCC.jl just for reference in case of debug
# TODO RCC5 can be better written as a combination of IA7 relations!
# const RCC5RelationFromRCC8 = Union{_Topo_DR,_Topo_PP,_Topo_PPi}
# RCC52RCC8Relations(::_Topo_DR)   = [Topo_DC,    Topo_EC]
# RCC52RCC8Relations(::_Topo_PP)   = [Topo_TPP,   Topo_NTPP]
# RCC52RCC8Relations(::_Topo_PPi)  = [Topo_TPPi,  Topo_NTPPi]