module Relations

    include("interval.jl");

    export Relation,
        DirectionalRelation, IntervalRelation,
        TopologicalRelation, RCCRelation

    export RelationGlob, RelationId

    # Interval algebra relations 1D

    # IA
    export IA_A, IA_L, IA_B, IA_E, IA_D, IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi
    # IA7
    export IA_AorO, IA_DorBorE, IA_AiorOi, IA_DiorBiorEi
    # IA3
    export IA_I

    export IARelations, IA7Relations, IA3Relations
    export IA72IARelations # NOTE: read the comment about fixing IA32IARelations

    # Interval algebra relations 2D

    export RectangleRelation

    export IA_IdA ,IA_IdL ,IA_IdB ,IA_IdE ,IA_IdD ,IA_IdO ,IA_IdAi ,IA_IdLi ,IA_IdBi ,IA_IdEi ,IA_IdDi ,IA_IdOi,
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
    IA_OiId,IA_OiA ,IA_OiL ,IA_OiB ,IA_OiE ,IA_OiD ,IA_OiO ,IA_OiAi ,IA_OiLi ,IA_OiBi ,IA_OiEi ,IA_OiDi ,IA_OiOi

    export IA_UId ,IA_UA ,IA_UL ,IA_UB ,IA_UE ,IA_UD ,IA_UO ,IA_UAi ,IA_ULi ,IA_UBi ,IA_UEi ,IA_UDi ,IA_UOi,
    IA_IdU ,IA_AU ,IA_LU ,IA_BU ,IA_EU ,IA_DU ,IA_OU ,IA_AiU ,IA_LiU ,IA_BiU ,IA_EiU ,IA_DiU ,IA_OiU

    export IA2DRelations, IA2D_URelations, IA2DRelations_extended

    # RCC8 relations
    export Topo_DC, Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi
    export RCC8Relations, RCC5Relations
    export topo2IARelations, RCC52RCC8Relations, RCC52IARelations

end
