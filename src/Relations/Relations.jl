module Relations

    include("interval.jl");

    export Relation,
        DirectionalRelation, IntervalRelation,
        TopologicalRelation, RCCRelation

    export _RelationGlob, _RelationId
    export RelationGlob, RelationId

    # Interval algebra relations 1D

    # IA
    export _IA_A, _IA_L, _IA_B, _IA_E, _IA_D, _IA_O, _IA_Ai, _IA_Li, _IA_Bi, _IA_Ei, _IA_Di, _IA_Oi
    export IA_A, IA_L, IA_B, IA_E, IA_D, IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi

    # IA7
    export _IA7Rel
    export _IA_AorO, _IA_DorBorE, _IA_AiorOi, _IA_DiorBiorEi
    export IA_AorO, IA_DorBorE, IA_AiorOi, IA_DiorBiorEi

    # IA3
    export _IA3Rel
    export _IA_I
    export IA_I

    export IARelations, IA7Relations, IA3Relations
    export IA72IARelations # NOTE: read the comment about fixing IA32IARelations

    # Interval algebra relations 2D

    export _IABase
    export RectangleRelation

    export IA2DRelations..., IA2D_URelations...
    export IA2DRelations, IA2D_URelations, IA2DRelations_extended

    # RCC8 relations
    export _TopoRelRCC8
    export _Topo_DC, _Topo_EC, _Topo_PO, _Topo_TPP, _Topo_TPPi, _Topo_NTPP, _Topo_NTPPi
    export Topo_DC, Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi

    # RCC5 relations
    export _TopoRelRCC5
    export _Topo_DR, _Topo_PP, _Topo_PPi
    export Topo_DR, Topo_PP, Topo_PPi

    export RCC8Relations, RCC5Relations
    export _TopoRelRCC8FromIA
    export topo2IARelations, RCC52RCC8Relations, RCC52IARelations

end
