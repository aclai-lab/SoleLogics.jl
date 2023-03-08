############################################################################################
# Allen's Interval Algebra 2D relations
############################################################################################

doc_rectangle_rel = """
    const _IABase = Union{IntervalRelation,IdentityRel,GlobalRel}
    struct RectangleRelation{R1<:_IABase,R2<:_IABase} <: GeometricalRelation
        x :: R1
        y :: R2
    end

Relation from 2D interval algebra, obtained from the combination of orthogonal interval relations,
 and are thus also referred to as rectangle algebra.

# Examples
```julia-repl
julia> syntaxstring.(IA2DRelations[1:20:end])
9-element Vector{String}:
 "=,A"
 "A,L̅"
 "B,L"
 "E,B̅"
 "O,B"
 "A̅,E̅"
 "B̅,E"
 "E̅,D̅"
 "O̅,D"
```

See also [`Interval`](@ref), [`Interval2D`](@ref),
[`IntervalRelation`](@ref), [`[`GeometricalRelation`](@ref).
"""

"""$(doc_rectangle_rel)"""
const _IABase = Union{IntervalRelation,IdentityRel,GlobalRel}
"""$(doc_rectangle_rel)"""
struct RectangleRelation{R1<:_IABase,R2<:_IABase} <: GeometricalRelation
    x :: R1
    y :: R2
end

arity(::Type{<:RectangleRelation}) = 2
hasconverse(::Type{<:RectangleRelation}) = true

function syntaxstring(::Type{R}; kwargs...) where {_XR<:_IABase,_YR<:_IABase,R<:RectangleRelation{_XR,_YR}}
    "$(syntaxstring(_XR; kwargs...)),$(syntaxstring(_YR; kwargs...))"
end

# Properties
converse(r::Type{RectangleRelation{R1,R2}})  where {R1<:_IABase,R2<:_IABase} = RectangleRelation{converse(R1),converse(R2)}
istransitive(r::RectangleRelation{R1,R2})  where {R1<:_IABase,R2<:_IABase} = istransitive(R1)  && istransitive(R2)
istopological(r::RectangleRelation{R1,R2}) where {R1<:_IABase,R2<:_IABase} = istopological(R1) && istopological(R2)

############################################################################################

# (12+1+1)^2-1-1 = 194 Extended 2D Interval Algebra relations
                                                                const IA_IdU  = RectangleRelation(identityrel, globalrel); const IA_IdA  = RectangleRelation(identityrel , IA_A); const IA_IdL  = RectangleRelation(identityrel , IA_L); const IA_IdB  = RectangleRelation(identityrel , IA_B); const IA_IdE  = RectangleRelation(identityrel , IA_E); const IA_IdD  = RectangleRelation(identityrel , IA_D); const IA_IdO  = RectangleRelation(identityrel , IA_O); const IA_IdAi  = RectangleRelation(identityrel , IA_Ai); const IA_IdLi  = RectangleRelation(identityrel , IA_Li); const IA_IdBi  = RectangleRelation(identityrel , IA_Bi); const IA_IdEi  = RectangleRelation(identityrel , IA_Ei); const IA_IdDi  = RectangleRelation(identityrel , IA_Di); const IA_IdOi  = RectangleRelation(identityrel , IA_Oi);
const IA_UId  = RectangleRelation(globalrel   , identityrel);                                                              const IA_UA   = RectangleRelation(globalrel   , IA_A); const IA_UL   = RectangleRelation(globalrel   , IA_L); const IA_UB   = RectangleRelation(globalrel   , IA_B); const IA_UE   = RectangleRelation(globalrel   , IA_E); const IA_UD   = RectangleRelation(globalrel   , IA_D); const IA_UO   = RectangleRelation(globalrel   , IA_O); const IA_UAi   = RectangleRelation(globalrel   , IA_Ai); const IA_ULi   = RectangleRelation(globalrel   , IA_Li); const IA_UBi   = RectangleRelation(globalrel   , IA_Bi); const IA_UEi   = RectangleRelation(globalrel   , IA_Ei); const IA_UDi   = RectangleRelation(globalrel   , IA_Di); const IA_UOi   = RectangleRelation(globalrel   , IA_Oi);
const IA_AId  = RectangleRelation(IA_A         , identityrel); const IA_AU   = RectangleRelation(IA_A        , globalrel); const IA_AA   = RectangleRelation(IA_A         , IA_A); const IA_AL   = RectangleRelation(IA_A         , IA_L); const IA_AB   = RectangleRelation(IA_A         , IA_B); const IA_AE   = RectangleRelation(IA_A         , IA_E); const IA_AD   = RectangleRelation(IA_A         , IA_D); const IA_AO   = RectangleRelation(IA_A         , IA_O); const IA_AAi   = RectangleRelation(IA_A         , IA_Ai); const IA_ALi   = RectangleRelation(IA_A         , IA_Li); const IA_ABi   = RectangleRelation(IA_A         , IA_Bi); const IA_AEi   = RectangleRelation(IA_A         , IA_Ei); const IA_ADi   = RectangleRelation(IA_A         , IA_Di); const IA_AOi   = RectangleRelation(IA_A         , IA_Oi);
const IA_LId  = RectangleRelation(IA_L         , identityrel); const IA_LU   = RectangleRelation(IA_L        , globalrel); const IA_LA   = RectangleRelation(IA_L         , IA_A); const IA_LL   = RectangleRelation(IA_L         , IA_L); const IA_LB   = RectangleRelation(IA_L         , IA_B); const IA_LE   = RectangleRelation(IA_L         , IA_E); const IA_LD   = RectangleRelation(IA_L         , IA_D); const IA_LO   = RectangleRelation(IA_L         , IA_O); const IA_LAi   = RectangleRelation(IA_L         , IA_Ai); const IA_LLi   = RectangleRelation(IA_L         , IA_Li); const IA_LBi   = RectangleRelation(IA_L         , IA_Bi); const IA_LEi   = RectangleRelation(IA_L         , IA_Ei); const IA_LDi   = RectangleRelation(IA_L         , IA_Di); const IA_LOi   = RectangleRelation(IA_L         , IA_Oi);
const IA_BId  = RectangleRelation(IA_B         , identityrel); const IA_BU   = RectangleRelation(IA_B        , globalrel); const IA_BA   = RectangleRelation(IA_B         , IA_A); const IA_BL   = RectangleRelation(IA_B         , IA_L); const IA_BB   = RectangleRelation(IA_B         , IA_B); const IA_BE   = RectangleRelation(IA_B         , IA_E); const IA_BD   = RectangleRelation(IA_B         , IA_D); const IA_BO   = RectangleRelation(IA_B         , IA_O); const IA_BAi   = RectangleRelation(IA_B         , IA_Ai); const IA_BLi   = RectangleRelation(IA_B         , IA_Li); const IA_BBi   = RectangleRelation(IA_B         , IA_Bi); const IA_BEi   = RectangleRelation(IA_B         , IA_Ei); const IA_BDi   = RectangleRelation(IA_B         , IA_Di); const IA_BOi   = RectangleRelation(IA_B         , IA_Oi);
const IA_EId  = RectangleRelation(IA_E         , identityrel); const IA_EU   = RectangleRelation(IA_E        , globalrel); const IA_EA   = RectangleRelation(IA_E         , IA_A); const IA_EL   = RectangleRelation(IA_E         , IA_L); const IA_EB   = RectangleRelation(IA_E         , IA_B); const IA_EE   = RectangleRelation(IA_E         , IA_E); const IA_ED   = RectangleRelation(IA_E         , IA_D); const IA_EO   = RectangleRelation(IA_E         , IA_O); const IA_EAi   = RectangleRelation(IA_E         , IA_Ai); const IA_ELi   = RectangleRelation(IA_E         , IA_Li); const IA_EBi   = RectangleRelation(IA_E         , IA_Bi); const IA_EEi   = RectangleRelation(IA_E         , IA_Ei); const IA_EDi   = RectangleRelation(IA_E         , IA_Di); const IA_EOi   = RectangleRelation(IA_E         , IA_Oi);
const IA_DId  = RectangleRelation(IA_D         , identityrel); const IA_DU   = RectangleRelation(IA_D        , globalrel); const IA_DA   = RectangleRelation(IA_D         , IA_A); const IA_DL   = RectangleRelation(IA_D         , IA_L); const IA_DB   = RectangleRelation(IA_D         , IA_B); const IA_DE   = RectangleRelation(IA_D         , IA_E); const IA_DD   = RectangleRelation(IA_D         , IA_D); const IA_DO   = RectangleRelation(IA_D         , IA_O); const IA_DAi   = RectangleRelation(IA_D         , IA_Ai); const IA_DLi   = RectangleRelation(IA_D         , IA_Li); const IA_DBi   = RectangleRelation(IA_D         , IA_Bi); const IA_DEi   = RectangleRelation(IA_D         , IA_Ei); const IA_DDi   = RectangleRelation(IA_D         , IA_Di); const IA_DOi   = RectangleRelation(IA_D         , IA_Oi);
const IA_OId  = RectangleRelation(IA_O         , identityrel); const IA_OU   = RectangleRelation(IA_O        , globalrel); const IA_OA   = RectangleRelation(IA_O         , IA_A); const IA_OL   = RectangleRelation(IA_O         , IA_L); const IA_OB   = RectangleRelation(IA_O         , IA_B); const IA_OE   = RectangleRelation(IA_O         , IA_E); const IA_OD   = RectangleRelation(IA_O         , IA_D); const IA_OO   = RectangleRelation(IA_O         , IA_O); const IA_OAi   = RectangleRelation(IA_O         , IA_Ai); const IA_OLi   = RectangleRelation(IA_O         , IA_Li); const IA_OBi   = RectangleRelation(IA_O         , IA_Bi); const IA_OEi   = RectangleRelation(IA_O         , IA_Ei); const IA_ODi   = RectangleRelation(IA_O         , IA_Di); const IA_OOi   = RectangleRelation(IA_O         , IA_Oi);
const IA_AiId = RectangleRelation(IA_Ai        , identityrel); const IA_AiU  = RectangleRelation(IA_Ai       , globalrel); const IA_AiA  = RectangleRelation(IA_Ai        , IA_A); const IA_AiL  = RectangleRelation(IA_Ai        , IA_L); const IA_AiB  = RectangleRelation(IA_Ai        , IA_B); const IA_AiE  = RectangleRelation(IA_Ai        , IA_E); const IA_AiD  = RectangleRelation(IA_Ai        , IA_D); const IA_AiO  = RectangleRelation(IA_Ai        , IA_O); const IA_AiAi  = RectangleRelation(IA_Ai        , IA_Ai); const IA_AiLi  = RectangleRelation(IA_Ai        , IA_Li); const IA_AiBi  = RectangleRelation(IA_Ai        , IA_Bi); const IA_AiEi  = RectangleRelation(IA_Ai        , IA_Ei); const IA_AiDi  = RectangleRelation(IA_Ai        , IA_Di); const IA_AiOi  = RectangleRelation(IA_Ai        , IA_Oi);
const IA_LiId = RectangleRelation(IA_Li        , identityrel); const IA_LiU  = RectangleRelation(IA_Li       , globalrel); const IA_LiA  = RectangleRelation(IA_Li        , IA_A); const IA_LiL  = RectangleRelation(IA_Li        , IA_L); const IA_LiB  = RectangleRelation(IA_Li        , IA_B); const IA_LiE  = RectangleRelation(IA_Li        , IA_E); const IA_LiD  = RectangleRelation(IA_Li        , IA_D); const IA_LiO  = RectangleRelation(IA_Li        , IA_O); const IA_LiAi  = RectangleRelation(IA_Li        , IA_Ai); const IA_LiLi  = RectangleRelation(IA_Li        , IA_Li); const IA_LiBi  = RectangleRelation(IA_Li        , IA_Bi); const IA_LiEi  = RectangleRelation(IA_Li        , IA_Ei); const IA_LiDi  = RectangleRelation(IA_Li        , IA_Di); const IA_LiOi  = RectangleRelation(IA_Li        , IA_Oi);
const IA_BiId = RectangleRelation(IA_Bi        , identityrel); const IA_BiU  = RectangleRelation(IA_Bi       , globalrel); const IA_BiA  = RectangleRelation(IA_Bi        , IA_A); const IA_BiL  = RectangleRelation(IA_Bi        , IA_L); const IA_BiB  = RectangleRelation(IA_Bi        , IA_B); const IA_BiE  = RectangleRelation(IA_Bi        , IA_E); const IA_BiD  = RectangleRelation(IA_Bi        , IA_D); const IA_BiO  = RectangleRelation(IA_Bi        , IA_O); const IA_BiAi  = RectangleRelation(IA_Bi        , IA_Ai); const IA_BiLi  = RectangleRelation(IA_Bi        , IA_Li); const IA_BiBi  = RectangleRelation(IA_Bi        , IA_Bi); const IA_BiEi  = RectangleRelation(IA_Bi        , IA_Ei); const IA_BiDi  = RectangleRelation(IA_Bi        , IA_Di); const IA_BiOi  = RectangleRelation(IA_Bi        , IA_Oi);
const IA_EiId = RectangleRelation(IA_Ei        , identityrel); const IA_EiU  = RectangleRelation(IA_Ei       , globalrel); const IA_EiA  = RectangleRelation(IA_Ei        , IA_A); const IA_EiL  = RectangleRelation(IA_Ei        , IA_L); const IA_EiB  = RectangleRelation(IA_Ei        , IA_B); const IA_EiE  = RectangleRelation(IA_Ei        , IA_E); const IA_EiD  = RectangleRelation(IA_Ei        , IA_D); const IA_EiO  = RectangleRelation(IA_Ei        , IA_O); const IA_EiAi  = RectangleRelation(IA_Ei        , IA_Ai); const IA_EiLi  = RectangleRelation(IA_Ei        , IA_Li); const IA_EiBi  = RectangleRelation(IA_Ei        , IA_Bi); const IA_EiEi  = RectangleRelation(IA_Ei        , IA_Ei); const IA_EiDi  = RectangleRelation(IA_Ei        , IA_Di); const IA_EiOi  = RectangleRelation(IA_Ei        , IA_Oi);
const IA_DiId = RectangleRelation(IA_Di        , identityrel); const IA_DiU  = RectangleRelation(IA_Di       , globalrel); const IA_DiA  = RectangleRelation(IA_Di        , IA_A); const IA_DiL  = RectangleRelation(IA_Di        , IA_L); const IA_DiB  = RectangleRelation(IA_Di        , IA_B); const IA_DiE  = RectangleRelation(IA_Di        , IA_E); const IA_DiD  = RectangleRelation(IA_Di        , IA_D); const IA_DiO  = RectangleRelation(IA_Di        , IA_O); const IA_DiAi  = RectangleRelation(IA_Di        , IA_Ai); const IA_DiLi  = RectangleRelation(IA_Di        , IA_Li); const IA_DiBi  = RectangleRelation(IA_Di        , IA_Bi); const IA_DiEi  = RectangleRelation(IA_Di        , IA_Ei); const IA_DiDi  = RectangleRelation(IA_Di        , IA_Di); const IA_DiOi  = RectangleRelation(IA_Di        , IA_Oi);
const IA_OiId = RectangleRelation(IA_Oi        , identityrel); const IA_OiU  = RectangleRelation(IA_Oi       , globalrel); const IA_OiA  = RectangleRelation(IA_Oi        , IA_A); const IA_OiL  = RectangleRelation(IA_Oi        , IA_L); const IA_OiB  = RectangleRelation(IA_Oi        , IA_B); const IA_OiE  = RectangleRelation(IA_Oi        , IA_E); const IA_OiD  = RectangleRelation(IA_Oi        , IA_D); const IA_OiO  = RectangleRelation(IA_Oi        , IA_O); const IA_OiAi  = RectangleRelation(IA_Oi        , IA_Ai); const IA_OiLi  = RectangleRelation(IA_Oi        , IA_Li); const IA_OiBi  = RectangleRelation(IA_Oi        , IA_Bi); const IA_OiEi  = RectangleRelation(IA_Oi        , IA_Ei); const IA_OiDi  = RectangleRelation(IA_Oi        , IA_Di); const IA_OiOi  = RectangleRelation(IA_Oi        , IA_Oi);

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
IA2DRelation = Union{typeof.(IA2DRelations)...}

# (1+1)*13=26 2D Interval Algebra remainder relations
const IA2D_URelations = [
IA_UId ,IA_UA ,IA_UL ,IA_UB ,IA_UE ,IA_UD ,IA_UO ,IA_UAi ,IA_ULi ,IA_UBi ,IA_UEi ,IA_UDi ,IA_UOi,
IA_IdU ,IA_AU ,IA_LU ,IA_BU ,IA_EU ,IA_DU ,IA_OU ,IA_AiU ,IA_LiU ,IA_BiU ,IA_EiU ,IA_DiU ,IA_OiU
]
IA2D_URelation = Union{typeof.(IA2D_URelations)...}

# (12+1+1)^2-1=195 2D Interval Algebra relations extended with their combinations with universal
const IA2DRelations_extended = [
globalrel,
IA2DRelations...,
IA2D_URelations...
]
IA2DRelation_extended = Union{typeof.(IA2DRelations_extended)...}
