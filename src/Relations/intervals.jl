############################################################################################
# Abstract types
############################################################################################

abstract type Relation end

# Directional relations
abstract type DirectionalRelation <: Relation end
abstract type IntervalRelation <: DirectionalRelation end
abstract type _IA7Rel <: IntervalRelation  end
abstract type _IA3Rel <: IntervalRelation  end

# Topological relations
abstract type TopologicalRelation <: Relation end
abstract type RCCRelation <: TopologicalRelation end
abstract type _TopoRelRCC8 <: RCCRelation end
abstract type _TopoRelRCC5 <: RCCRelation end

############################################################################################
# Relations Singletons
############################################################################################

# Global relation: any world -> all worlds
struct _RelationGlob <: Relation end; const RelationGlob  = _RelationGlob();
Base.show(io::IO, ::_RelationGlob) = print(io, "G")

# Identity relation: any world -> itself
struct _RelationId <: Relation end; const RelationId   = _RelationId();
Base.show(io::IO, ::_RelationId) = print(io, "=")

############################################################################################
# Interval algebra comprehends 12 relations (plus equality, i.e. RelationId):
#  - the 6 relations After, Later, Begins, Ends, During, Overlaps
#  - their inverses
############################################################################################
# Graphical representation of R((x,y),(z,t)) for R ∈ {After, Later, Begins, Ends, During, Overlaps}:
#
#                       x                   y
#                       |-------------------|
#                       .                   .
#                       .                   z        t            y = z
# After       (A)       .                   |--------|
#                       .                   .
#                       .                   .   z         t       y < z
# Later       (L)       .                   .   |---------|
#                       .                   .
#                       z     t             .                     x = z, t < y
# Begins      (B)       |-----|             .
#                       .                   .
#                       .             z     t                     y = t, x < z
# Ends        (E)       .             |-----|
#                       .                   .
#                       .   z        t      .                     x < z, t < y
# During      (D)       .   |--------|      .
#                       .                   .
#                       .           z       .    t                x < z < y < t
# Overlaps    (O)       .           |------------|
#
############################################################################################
struct _IA_A  <: IntervalRelation end; const IA_A  = _IA_A();  # After
struct _IA_L  <: IntervalRelation end; const IA_L  = _IA_L();  # Later
struct _IA_B  <: IntervalRelation end; const IA_B  = _IA_B();  # Begins
struct _IA_E  <: IntervalRelation end; const IA_E  = _IA_E();  # Ends
struct _IA_D  <: IntervalRelation end; const IA_D  = _IA_D();  # During
struct _IA_O  <: IntervalRelation end; const IA_O  = _IA_O();  # Overlaps

struct _IA_Ai <: IntervalRelation end; const IA_Ai = _IA_Ai(); # After inverse
struct _IA_Li <: IntervalRelation end; const IA_Li = _IA_Li(); # Later inverse
struct _IA_Bi <: IntervalRelation end; const IA_Bi = _IA_Bi(); # Begins inverse
struct _IA_Ei <: IntervalRelation end; const IA_Ei = _IA_Ei(); # Ends inverse
struct _IA_Di <: IntervalRelation end; const IA_Di = _IA_Di(); # During inverse
struct _IA_Oi <: IntervalRelation end; const IA_Oi = _IA_Oi(); # Overlaps inverse

Base.show(io::IO, ::_IA_A)  = print(io, "A")
Base.show(io::IO, ::_IA_L)  = print(io, "L")
Base.show(io::IO, ::_IA_B)  = print(io, "B")
Base.show(io::IO, ::_IA_E)  = print(io, "E")
Base.show(io::IO, ::_IA_D)  = print(io, "D")
Base.show(io::IO, ::_IA_O)  = print(io, "O")
Base.show(io::IO, ::_IA_Ai) = print(io, "A̅")
Base.show(io::IO, ::_IA_Li) = print(io, "L̅")
Base.show(io::IO, ::_IA_Bi) = print(io, "B̅")
Base.show(io::IO, ::_IA_Ei) = print(io, "E̅")
Base.show(io::IO, ::_IA_Di) = print(io, "D̅")
Base.show(io::IO, ::_IA_Oi) = print(io, "O̅")

# Coarser relations: IA7
struct _IA_AorO       <: _IA7Rel end; const IA_AorO       = _IA_AorO();       # After ∪ Overlaps
struct _IA_DorBorE    <: _IA7Rel end; const IA_DorBorE    = _IA_DorBorE();    # During ∪ Begins ∪ Ends
struct _IA_AiorOi     <: _IA7Rel end; const IA_AiorOi     = _IA_AiorOi();     # (After ∪ Overlaps) inverse
struct _IA_DiorBiorEi <: _IA7Rel end; const IA_DiorBiorEi = _IA_DiorBiorEi(); # (During ∪ Begins ∪ Ends) inverse

# Even coarser relations: IA3
struct _IA_I          <: _IA3Rel end; const IA_I          = _IA_I();   # Intersecting (ABEDO ∪ ABEDO inverse)

# Split the relations contained in IA7
IA72IARelations(::_IA_AorO)       = [IA_A,  IA_O]
IA72IARelations(::_IA_AiorOi)     = [IA_Ai, IA_Oi]
IA72IARelations(::_IA_DorBorE)    = [IA_D,  IA_B,  IA_E]
IA72IARelations(::_IA_DiorBiorEi) = [IA_Di, IA_Bi, IA_Ei]
# TODO: this should be IA32IARelations(::_IA_I)
IA72IARelations(::_IA_I)          = [
    IA_A,  IA_O,  IA_D,  IA_B,  IA_E,
    IA_Ai, IA_Oi, IA_Di, IA_Bi, IA_Ei
]

Base.show(io::IO, r::_IA7Rel)       = print(io, join(IA72IARelations(r), "∨"))
Base.show(io::IO, ::_IA_I)          = print(io, "I")

# 12 IA relations
const IARelations = [IA_A,  IA_L,  IA_B,  IA_E,  IA_D,  IA_O,
                     IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi]

# 7 IA7 relations
const IA7Relations = [IA_AorO,   IA_L,  IA_DorBorE,
                      IA_AiorOi, IA_Li, IA_DiorBiorEi]

# 3 IA3 relations
const IA3Relations = [IA_I, IA_L, IA_Li]

############################################################################################
# Interval Algebra 2D relations + definitions for Interval2D
############################################################################################

# Relations from 2D interval algebra are obtained from the combination of orthogonal interval relations,
#  and are thus also referred to as rectangle algebra.
# In this implementation, we actually use the extended set as a base: IA relations + Global relation.

const _IABase = Union{IntervalRelation,_RelationId,_RelationGlob}
struct RectangleRelation{R1<:_IABase,R2<:_IABase} <: DirectionalRelation
    x :: R1
    y :: R2
end

# (12+1+1)^2-1-1 = 194 Extended 2D Interval Algebra relations
const IA_IdU  = RectangleRelation(RelationId  , RelationGlob); const IA_IdA  = RectangleRelation(RelationId   , IA_A); const IA_IdL  = RectangleRelation(RelationId   , IA_L); const IA_IdB  = RectangleRelation(RelationId   , IA_B); const IA_IdE  = RectangleRelation(RelationId   , IA_E); const IA_IdD  = RectangleRelation(RelationId   , IA_D); const IA_IdO  = RectangleRelation(RelationId   , IA_O); const IA_IdAi  = RectangleRelation(RelationId   , IA_Ai); const IA_IdLi  = RectangleRelation(RelationId   , IA_Li); const IA_IdBi  = RectangleRelation(RelationId   , IA_Bi); const IA_IdEi  = RectangleRelation(RelationId   , IA_Ei); const IA_IdDi  = RectangleRelation(RelationId   , IA_Di); const IA_IdOi  = RectangleRelation(RelationId   , IA_Oi);
const IA_UId  = RectangleRelation(RelationGlob , RelationId);                                                                const IA_UA   = RectangleRelation(RelationGlob , IA_A); const IA_UL   = RectangleRelation(RelationGlob , IA_L); const IA_UB   = RectangleRelation(RelationGlob , IA_B); const IA_UE   = RectangleRelation(RelationGlob , IA_E); const IA_UD   = RectangleRelation(RelationGlob , IA_D); const IA_UO   = RectangleRelation(RelationGlob , IA_O); const IA_UAi   = RectangleRelation(RelationGlob , IA_Ai); const IA_ULi   = RectangleRelation(RelationGlob , IA_Li); const IA_UBi   = RectangleRelation(RelationGlob , IA_Bi); const IA_UEi   = RectangleRelation(RelationGlob , IA_Ei); const IA_UDi   = RectangleRelation(RelationGlob , IA_Di); const IA_UOi   = RectangleRelation(RelationGlob , IA_Oi);
const IA_AId  = RectangleRelation(IA_A         , RelationId); const IA_AU   = RectangleRelation(IA_A        , RelationGlob); const IA_AA   = RectangleRelation(IA_A         , IA_A); const IA_AL   = RectangleRelation(IA_A         , IA_L); const IA_AB   = RectangleRelation(IA_A         , IA_B); const IA_AE   = RectangleRelation(IA_A         , IA_E); const IA_AD   = RectangleRelation(IA_A         , IA_D); const IA_AO   = RectangleRelation(IA_A         , IA_O); const IA_AAi   = RectangleRelation(IA_A         , IA_Ai); const IA_ALi   = RectangleRelation(IA_A         , IA_Li); const IA_ABi   = RectangleRelation(IA_A         , IA_Bi); const IA_AEi   = RectangleRelation(IA_A         , IA_Ei); const IA_ADi   = RectangleRelation(IA_A         , IA_Di); const IA_AOi   = RectangleRelation(IA_A         , IA_Oi);
const IA_LId  = RectangleRelation(IA_L         , RelationId); const IA_LU   = RectangleRelation(IA_L        , RelationGlob); const IA_LA   = RectangleRelation(IA_L         , IA_A); const IA_LL   = RectangleRelation(IA_L         , IA_L); const IA_LB   = RectangleRelation(IA_L         , IA_B); const IA_LE   = RectangleRelation(IA_L         , IA_E); const IA_LD   = RectangleRelation(IA_L         , IA_D); const IA_LO   = RectangleRelation(IA_L         , IA_O); const IA_LAi   = RectangleRelation(IA_L         , IA_Ai); const IA_LLi   = RectangleRelation(IA_L         , IA_Li); const IA_LBi   = RectangleRelation(IA_L         , IA_Bi); const IA_LEi   = RectangleRelation(IA_L         , IA_Ei); const IA_LDi   = RectangleRelation(IA_L         , IA_Di); const IA_LOi   = RectangleRelation(IA_L         , IA_Oi);
const IA_BId  = RectangleRelation(IA_B         , RelationId); const IA_BU   = RectangleRelation(IA_B        , RelationGlob); const IA_BA   = RectangleRelation(IA_B         , IA_A); const IA_BL   = RectangleRelation(IA_B         , IA_L); const IA_BB   = RectangleRelation(IA_B         , IA_B); const IA_BE   = RectangleRelation(IA_B         , IA_E); const IA_BD   = RectangleRelation(IA_B         , IA_D); const IA_BO   = RectangleRelation(IA_B         , IA_O); const IA_BAi   = RectangleRelation(IA_B         , IA_Ai); const IA_BLi   = RectangleRelation(IA_B         , IA_Li); const IA_BBi   = RectangleRelation(IA_B         , IA_Bi); const IA_BEi   = RectangleRelation(IA_B         , IA_Ei); const IA_BDi   = RectangleRelation(IA_B         , IA_Di); const IA_BOi   = RectangleRelation(IA_B         , IA_Oi);
const IA_EId  = RectangleRelation(IA_E         , RelationId); const IA_EU   = RectangleRelation(IA_E        , RelationGlob); const IA_EA   = RectangleRelation(IA_E         , IA_A); const IA_EL   = RectangleRelation(IA_E         , IA_L); const IA_EB   = RectangleRelation(IA_E         , IA_B); const IA_EE   = RectangleRelation(IA_E         , IA_E); const IA_ED   = RectangleRelation(IA_E         , IA_D); const IA_EO   = RectangleRelation(IA_E         , IA_O); const IA_EAi   = RectangleRelation(IA_E         , IA_Ai); const IA_ELi   = RectangleRelation(IA_E         , IA_Li); const IA_EBi   = RectangleRelation(IA_E         , IA_Bi); const IA_EEi   = RectangleRelation(IA_E         , IA_Ei); const IA_EDi   = RectangleRelation(IA_E         , IA_Di); const IA_EOi   = RectangleRelation(IA_E         , IA_Oi);
const IA_DId  = RectangleRelation(IA_D         , RelationId); const IA_DU   = RectangleRelation(IA_D        , RelationGlob); const IA_DA   = RectangleRelation(IA_D         , IA_A); const IA_DL   = RectangleRelation(IA_D         , IA_L); const IA_DB   = RectangleRelation(IA_D         , IA_B); const IA_DE   = RectangleRelation(IA_D         , IA_E); const IA_DD   = RectangleRelation(IA_D         , IA_D); const IA_DO   = RectangleRelation(IA_D         , IA_O); const IA_DAi   = RectangleRelation(IA_D         , IA_Ai); const IA_DLi   = RectangleRelation(IA_D         , IA_Li); const IA_DBi   = RectangleRelation(IA_D         , IA_Bi); const IA_DEi   = RectangleRelation(IA_D         , IA_Ei); const IA_DDi   = RectangleRelation(IA_D         , IA_Di); const IA_DOi   = RectangleRelation(IA_D         , IA_Oi);
const IA_OId  = RectangleRelation(IA_O         , RelationId); const IA_OU   = RectangleRelation(IA_O        , RelationGlob); const IA_OA   = RectangleRelation(IA_O         , IA_A); const IA_OL   = RectangleRelation(IA_O         , IA_L); const IA_OB   = RectangleRelation(IA_O         , IA_B); const IA_OE   = RectangleRelation(IA_O         , IA_E); const IA_OD   = RectangleRelation(IA_O         , IA_D); const IA_OO   = RectangleRelation(IA_O         , IA_O); const IA_OAi   = RectangleRelation(IA_O         , IA_Ai); const IA_OLi   = RectangleRelation(IA_O         , IA_Li); const IA_OBi   = RectangleRelation(IA_O         , IA_Bi); const IA_OEi   = RectangleRelation(IA_O         , IA_Ei); const IA_ODi   = RectangleRelation(IA_O         , IA_Di); const IA_OOi   = RectangleRelation(IA_O         , IA_Oi);
const IA_AiId = RectangleRelation(IA_Ai        , RelationId); const IA_AiU  = RectangleRelation(IA_Ai       , RelationGlob); const IA_AiA  = RectangleRelation(IA_Ai        , IA_A); const IA_AiL  = RectangleRelation(IA_Ai        , IA_L); const IA_AiB  = RectangleRelation(IA_Ai        , IA_B); const IA_AiE  = RectangleRelation(IA_Ai        , IA_E); const IA_AiD  = RectangleRelation(IA_Ai        , IA_D); const IA_AiO  = RectangleRelation(IA_Ai        , IA_O); const IA_AiAi  = RectangleRelation(IA_Ai        , IA_Ai); const IA_AiLi  = RectangleRelation(IA_Ai        , IA_Li); const IA_AiBi  = RectangleRelation(IA_Ai        , IA_Bi); const IA_AiEi  = RectangleRelation(IA_Ai        , IA_Ei); const IA_AiDi  = RectangleRelation(IA_Ai        , IA_Di); const IA_AiOi  = RectangleRelation(IA_Ai        , IA_Oi);
const IA_LiId = RectangleRelation(IA_Li        , RelationId); const IA_LiU  = RectangleRelation(IA_Li       , RelationGlob); const IA_LiA  = RectangleRelation(IA_Li        , IA_A); const IA_LiL  = RectangleRelation(IA_Li        , IA_L); const IA_LiB  = RectangleRelation(IA_Li        , IA_B); const IA_LiE  = RectangleRelation(IA_Li        , IA_E); const IA_LiD  = RectangleRelation(IA_Li        , IA_D); const IA_LiO  = RectangleRelation(IA_Li        , IA_O); const IA_LiAi  = RectangleRelation(IA_Li        , IA_Ai); const IA_LiLi  = RectangleRelation(IA_Li        , IA_Li); const IA_LiBi  = RectangleRelation(IA_Li        , IA_Bi); const IA_LiEi  = RectangleRelation(IA_Li        , IA_Ei); const IA_LiDi  = RectangleRelation(IA_Li        , IA_Di); const IA_LiOi  = RectangleRelation(IA_Li        , IA_Oi);
const IA_BiId = RectangleRelation(IA_Bi        , RelationId); const IA_BiU  = RectangleRelation(IA_Bi       , RelationGlob); const IA_BiA  = RectangleRelation(IA_Bi        , IA_A); const IA_BiL  = RectangleRelation(IA_Bi        , IA_L); const IA_BiB  = RectangleRelation(IA_Bi        , IA_B); const IA_BiE  = RectangleRelation(IA_Bi        , IA_E); const IA_BiD  = RectangleRelation(IA_Bi        , IA_D); const IA_BiO  = RectangleRelation(IA_Bi        , IA_O); const IA_BiAi  = RectangleRelation(IA_Bi        , IA_Ai); const IA_BiLi  = RectangleRelation(IA_Bi        , IA_Li); const IA_BiBi  = RectangleRelation(IA_Bi        , IA_Bi); const IA_BiEi  = RectangleRelation(IA_Bi        , IA_Ei); const IA_BiDi  = RectangleRelation(IA_Bi        , IA_Di); const IA_BiOi  = RectangleRelation(IA_Bi        , IA_Oi);
const IA_EiId = RectangleRelation(IA_Ei        , RelationId); const IA_EiU  = RectangleRelation(IA_Ei       , RelationGlob); const IA_EiA  = RectangleRelation(IA_Ei        , IA_A); const IA_EiL  = RectangleRelation(IA_Ei        , IA_L); const IA_EiB  = RectangleRelation(IA_Ei        , IA_B); const IA_EiE  = RectangleRelation(IA_Ei        , IA_E); const IA_EiD  = RectangleRelation(IA_Ei        , IA_D); const IA_EiO  = RectangleRelation(IA_Ei        , IA_O); const IA_EiAi  = RectangleRelation(IA_Ei        , IA_Ai); const IA_EiLi  = RectangleRelation(IA_Ei        , IA_Li); const IA_EiBi  = RectangleRelation(IA_Ei        , IA_Bi); const IA_EiEi  = RectangleRelation(IA_Ei        , IA_Ei); const IA_EiDi  = RectangleRelation(IA_Ei        , IA_Di); const IA_EiOi  = RectangleRelation(IA_Ei        , IA_Oi);
const IA_DiId = RectangleRelation(IA_Di        , RelationId); const IA_DiU  = RectangleRelation(IA_Di       , RelationGlob); const IA_DiA  = RectangleRelation(IA_Di        , IA_A); const IA_DiL  = RectangleRelation(IA_Di        , IA_L); const IA_DiB  = RectangleRelation(IA_Di        , IA_B); const IA_DiE  = RectangleRelation(IA_Di        , IA_E); const IA_DiD  = RectangleRelation(IA_Di        , IA_D); const IA_DiO  = RectangleRelation(IA_Di        , IA_O); const IA_DiAi  = RectangleRelation(IA_Di        , IA_Ai); const IA_DiLi  = RectangleRelation(IA_Di        , IA_Li); const IA_DiBi  = RectangleRelation(IA_Di        , IA_Bi); const IA_DiEi  = RectangleRelation(IA_Di        , IA_Ei); const IA_DiDi  = RectangleRelation(IA_Di        , IA_Di); const IA_DiOi  = RectangleRelation(IA_Di        , IA_Oi);
const IA_OiId = RectangleRelation(IA_Oi        , RelationId); const IA_OiU  = RectangleRelation(IA_Oi       , RelationGlob); const IA_OiA  = RectangleRelation(IA_Oi        , IA_A); const IA_OiL  = RectangleRelation(IA_Oi        , IA_L); const IA_OiB  = RectangleRelation(IA_Oi        , IA_B); const IA_OiE  = RectangleRelation(IA_Oi        , IA_E); const IA_OiD  = RectangleRelation(IA_Oi        , IA_D); const IA_OiO  = RectangleRelation(IA_Oi        , IA_O); const IA_OiAi  = RectangleRelation(IA_Oi        , IA_Ai); const IA_OiLi  = RectangleRelation(IA_Oi        , IA_Li); const IA_OiBi  = RectangleRelation(IA_Oi        , IA_Bi); const IA_OiEi  = RectangleRelation(IA_Oi        , IA_Ei); const IA_OiDi  = RectangleRelation(IA_Oi        , IA_Di); const IA_OiOi  = RectangleRelation(IA_Oi        , IA_Oi);

Base.show(io::IO, ::RectangleRelation{_XR,_YR}) where {_XR<:_IABase,_YR<:_IABase} = print(io, "$(_XR()),$(_YR())")

# (12+1)^2-1=168 2D Interval Algebra relations
const IA2DRelations = [
        IA_IdA ,IA_IdL ,IA_IdB ,IA_IdE ,IA_IdD ,IA_IdO ,IA_IdAi ,IA_IdLi ,IA_IdBi ,IA_IdEi ,IA_IdDi ,IA_IdOi,
IA_AId ,IA_AA  ,IA_AL  ,IA_AB  ,IA_AE  ,IA_AD  ,IA_AO  ,IA_AAi  ,IA_ALi  ,IA_ABi  ,IA_AEi  ,IA_ADi  ,IA_AOi,
IA_LId ,IA_LA  ,IA_LL  ,IA_LB  ,IA_LE  ,IA_LD  ,IA_LO  ,IA_LAi  ,IA_LLi  ,IA_LBi  ,IA_LEi  ,IA_LDi  ,IA_LOi,
IA_BId ,IA_BA  ,IA_BL  ,IA_BB  ,IA_BE  ,IA_BD  ,IA_BO  ,IA_BAi  ,IA_BLi  ,IA_BBi  ,IA_BEi  ,IA_BDi  ,IA_BOi,
IA_EId ,IA_EA  ,IA_EL  ,IA_EB  ,IA_EE  ,IA_ED  ,IA_EO  ,IA_EAi  ,IA_ELi  ,IA_EBi  ,IA_EEi  ,IA_EDi  ,IA_EOi,
IA_DId ,IA_DA  ,IA_DL  ,IA_DB  ,IA_DE  ,IA_DD  ,IA_DO  ,IA_DAi  ,IA_DLi  ,IA_DBi  ,IA_DEi  ,IA_DDi  ,IA_DOi,
IA_OId ,IA_OA  ,IA_OL  ,IA_OB  ,IA_OE  ,IA_OD  ,IA_OO  ,IA_OAi  ,IA_OLi  ,IA_OBi  ,IA_OEi  ,IA_ODi  ,IA_OOi,
IA_AiId,IA_AiA ,IA_AiL ,IA_AiB ,IA_AiE ,IA_AiD ,IA_AiO ,IA_AiAi ,IA_AiLi ,IA_AiBi ,IA_AiEi ,IA_AiDi ,IA_AiOi,
IA_LiId,IA_LiA ,IA_LiL ,IA_LiB ,IA_LiE ,IA_LiD ,IA_LiO ,IA_LiAi ,IA_LiLi ,IA_LiBi ,IA_LiEi ,IA_LiDi ,IA_LiOi,
IA_BiId,IA_BiA ,IA_BiL ,IA_BiB ,IA_BiE ,IA_BiD ,IA_BiO ,IA_BiAi ,IA_BiLi ,IA_BiBi ,IA_BiEi ,IA_BiDi ,IA_BiOi,
IA_EiId,IA_EiA ,IA_EiL ,IA_EiB ,IA_EiE ,IA_EiD ,IA_EiO ,IA_EiAi ,IA_EiLi ,IA_EiBi ,IA_EiEi ,IA_EiDi ,IA_EiOi,
IA_DiId,IA_DiA ,IA_DiL ,IA_DiB ,IA_DiE ,IA_DiD ,IA_DiO ,IA_DiAi ,IA_DiLi ,IA_DiBi ,IA_DiEi ,IA_DiDi ,IA_DiOi,
IA_OiId,IA_OiA ,IA_OiL ,IA_OiB ,IA_OiE ,IA_OiD ,IA_OiO ,IA_OiAi ,IA_OiLi ,IA_OiBi ,IA_OiEi ,IA_OiDi ,IA_OiOi,
]

# (1+1)*13=26 2D Interval Algebra remainder relations
const IA2D_URelations = [
    IA_UId ,IA_UA ,IA_UL ,IA_UB ,IA_UE ,IA_UD ,IA_UO ,IA_UAi ,IA_ULi ,IA_UBi ,IA_UEi ,IA_UDi ,IA_UOi,
    IA_IdU ,IA_AU ,IA_LU ,IA_BU ,IA_EU ,IA_DU ,IA_OU ,IA_AiU ,IA_LiU ,IA_BiU ,IA_EiU ,IA_DiU ,IA_OiU
]

# (12+1+1)^2-1=195 2D Interval Algebra relations extended with their combinations with universal
const IA2DRelations_extended = [
    RelationGlob,
    IA2DRelations...,
    IA2D_URelations...
]

############################################################################################
# RCC topological relations + definitions for Interval
############################################################################################

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
struct _Topo_DC     <: _TopoRelRCC8 end; const Topo_DC     = _Topo_DC();     # Disconnected
struct _Topo_EC     <: _TopoRelRCC8 end; const Topo_EC     = _Topo_EC();     # Externally connected
struct _Topo_PO     <: _TopoRelRCC8 end; const Topo_PO     = _Topo_PO();     # Partially overlapping
struct _Topo_TPP    <: _TopoRelRCC8 end; const Topo_TPP    = _Topo_TPP();    # Tangential proper part
struct _Topo_TPPi   <: _TopoRelRCC8 end; const Topo_TPPi   = _Topo_TPPi();   # Tangential proper part inverse
struct _Topo_NTPP   <: _TopoRelRCC8 end; const Topo_NTPP   = _Topo_NTPP();   # Non-tangential proper part
struct _Topo_NTPPi  <: _TopoRelRCC8 end; const Topo_NTPPi  = _Topo_NTPPi();  # Non-tangential proper part inverse

Base.show(io::IO, ::_Topo_DC)    = print(io, "DC")
Base.show(io::IO, ::_Topo_EC)    = print(io, "EC")
Base.show(io::IO, ::_Topo_PO)    = print(io, "PO")
Base.show(io::IO, ::_Topo_TPP)   = print(io, "TPP")
Base.show(io::IO, ::_Topo_TPPi)  = print(io, "T̅P̅P̅")
Base.show(io::IO, ::_Topo_NTPP)  = print(io, "NTPP")
Base.show(io::IO, ::_Topo_NTPPi) = print(io, "N̅T̅P̅P̅")

############################################################################################

# Coarser relations for RCC5
struct _Topo_DR     <: _TopoRelRCC5 end; const Topo_DR     = _Topo_DR();     # Disjointed
struct _Topo_PP     <: _TopoRelRCC5 end; const Topo_PP     = _Topo_PP();     # Proper part
struct _Topo_PPi    <: _TopoRelRCC5 end; const Topo_PPi    = _Topo_PPi();    # Proper part inverse

Base.show(io::IO, ::_Topo_DR)    = print(io, "DR")
Base.show(io::IO, ::_Topo_PP)    = print(io, "PP")
Base.show(io::IO, ::_Topo_PPi)   = print(io, "P̅P̅")

############################################################################################

# 7 RCC8 Relations
const RCC8Relations = [Topo_DC, Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi]

# 4 RCC5 Relations
const RCC5Relations = [Topo_DR, Topo_PO, Topo_PP, Topo_PPi]

############################################################################################

# It is conveniente to define RCC relations as unions of IA relations
const _TopoRelRCC8FromIA = Union{_Topo_DC,_Topo_EC,_Topo_PO,_Topo_TPP,_Topo_TPPi}

# Split the relations contained in RCC8
topo2IARelations(::_Topo_DC)     = [IA_L,  IA_Li]
topo2IARelations(::_Topo_EC)     = [IA_A,  IA_Ai]
topo2IARelations(::_Topo_PO)     = [IA_O,  IA_Oi]
topo2IARelations(::_Topo_TPP)    = [IA_B,  IA_E]
topo2IARelations(::_Topo_TPPi)   = [IA_Bi, IA_Ei]
topo2IARelations(::_Topo_NTPP)   = [IA_D]
topo2IARelations(::_Topo_NTPPi)  = [IA_Di]

# TODO: RCC5 can be better written as a combination of IA7 relations!
RCC52RCC8Relations(::_Topo_DR)   = [Topo_DC,    Topo_EC]
RCC52RCC8Relations(::_Topo_PP)   = [Topo_TPP,   Topo_NTPP]
RCC52RCC8Relations(::_Topo_PPi)  = [Topo_TPPi,  Topo_NTPPi]

RCC52IARelations(::_Topo_DR)   = [IA_L,  IA_Li,  IA_A,  IA_Ai]
RCC52IARelations(::_Topo_PP)   = [IA_B,  IA_E,   IA_D]
RCC52IARelations(::_Topo_PPi)  = [IA_Bi, IA_Ei,  IA_Di]

#=
    NOTE: all those operators may be already defined using @modaloperators macro.
    The following code has been taken from ModalDecisionTrees.jl and should be
    uniformed to SoleLogics style.


    NOTE: Ontology is the name used to describe the coupling
    of a world-type (e.g Interval) and the valid relations involving them.
=#
