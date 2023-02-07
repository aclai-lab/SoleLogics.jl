############################################################################################
# RCC topological relations
############################################################################################

abstract type RCCRelation <: GeometricalRelation end

# Property: all RCC relations are topological
istopological(r::RCCRelation) = true

############################################################################################
# RCC8 topological relations (plus equality, i.e. RelationId):
# - Externally connected
# - Partially overlapping
# - Tangential proper part
# - Tangential proper part inverse
# - Non-tangential proper part
# - Non-tangential proper part inverse
############################################################################################
# Graphical representation of R((x,y),(z,t)) for R ∈ RCC8
#
#                                                  x                   y
#                                                  |-------------------|
#                                                  .                   .
#                                                  .                   .  z        t
# Disconnected                         (DC)        .                   . |--------|
#                                                  .                   .
#                                                  .                   z         t
# Externally connected                 (EC)        .                   |---------|
#                                                  .                   .
#                                                  .                z     t
# Partially overlapping                (PO)        .                |-----|
#                                                  .                   .
#                                                  .             z     t
# Tangential proper part               (TPP)       .             |-----|
#                                                  .                   .
#                                                  z                   .     t
# Tangential proper part inverse       (T̅P̅P̅)       |-------------------------|
#                                                  .                   .
#                                                  .           z       .
# Non-tangential proper part           (NTPP)      .           |-----| .
#                                                  .                   .
#                                                z .                   . t
# Non-tangential proper part inverse   (N̅T̅P̅P̅)    |-----------------------|
#
############################################################################################

# Relations for RCC8
struct _Topo_DC     <: RCCRelation end; const Topo_DC     = _Topo_DC();     # Disconnected
struct _Topo_EC     <: RCCRelation end; const Topo_EC     = _Topo_EC();     # Externally connected
struct _Topo_PO     <: RCCRelation end; const Topo_PO     = _Topo_PO();     # Partially overlapping
struct _Topo_TPP    <: RCCRelation end; const Topo_TPP    = _Topo_TPP();    # Tangential proper part
struct _Topo_TPPi   <: RCCRelation end; const Topo_TPPi   = _Topo_TPPi();   # Tangential proper part inverse
struct _Topo_NTPP   <: RCCRelation end; const Topo_NTPP   = _Topo_NTPP();   # Non-tangential proper part
struct _Topo_NTPPi  <: RCCRelation end; const Topo_NTPPi  = _Topo_NTPPi();  # Non-tangential proper part inverse

syntaxstring(::Type{_Topo_DC}; kwargs...)    = "DC"
syntaxstring(::Type{_Topo_EC}; kwargs...)    = "EC"
syntaxstring(::Type{_Topo_PO}; kwargs...)    = "PO"
syntaxstring(::Type{_Topo_TPP}; kwargs...)   = "TPP"
syntaxstring(::Type{_Topo_TPPi}; kwargs...)  = "T̅P̅P̅"
syntaxstring(::Type{_Topo_NTPP}; kwargs...)  = "NTPP"
syntaxstring(::Type{_Topo_NTPPi}; kwargs...) = "N̅T̅P̅P̅"

# Properties
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

syntaxstring(::Type{_Topo_DR}; kwargs...)    = "DR"
syntaxstring(::Type{_Topo_PP}; kwargs...)    = "PP"
syntaxstring(::Type{_Topo_PPi}; kwargs...)   = "P̅P̅"

# Properties
issymmetric(r::_Topo_DR) = true
istransitive(r::_Topo_PP) = true
istransitive(r::_Topo_PPi) = true
############################################################################################

# 7 RCC8 Relations
const RCC8Relations = [Topo_DC, Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi]
RCC8Relation = Union{typeof.(RCC8Relations)...}

# 4 RCC5 Relations
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
RCC52RCC8Relations(::_Topo_DR)   = [Topo_DC,    Topo_EC]
RCC52RCC8Relations(::_Topo_PP)   = [Topo_TPP,   Topo_NTPP]
RCC52RCC8Relations(::_Topo_PPi)  = [Topo_TPPi,  Topo_NTPPi]

RCC52IARelations(::_Topo_DR)   = [IA_L,  IA_Li,  IA_A,  IA_Ai]
RCC52IARelations(::_Topo_PP)   = [IA_B,  IA_E,   IA_D]
RCC52IARelations(::_Topo_PPi)  = [IA_Bi, IA_Ei,  IA_Di]
