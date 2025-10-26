using SoleLogics: FullDimensionalFrame

################################################################################
# Linear Temporal Logic with Future and Past (LTL[F,P])
################################################################################
using SoleLogics: LTLFP_F, LTLFP_P

fr = FullDimensionalFrame((5,), Point1D{Int})
@test collect(accessibles(fr, Point(3), LTLFP_F)) == Point1D{Int64}[Point1D{Int64}((4,)), Point1D{Int64}((5,))]
@test collect(accessibles(fr, Point(3), LTLFP_P)) == Point1D{Int64}[Point1D{Int64}((1,)), Point1D{Int64}((2,))]

################################################################################
# Compass Logic (CL)
################################################################################
using SoleLogics: CL_N, CL_S, CL_E, CL_W

fr = FullDimensionalFrame((5,5), Point2D{Int})
@test collect(accessibles(fr, Point(3,3), CL_N)) == Point2D{Int64}[Point2D{Int64}((3,4)), Point2D{Int64}((3,5))]
@test collect(accessibles(fr, Point(3,3), CL_S)) == Point2D{Int64}[Point2D{Int64}((3,1)), Point2D{Int64}((3,2))]
@test collect(accessibles(fr, Point(3,3), CL_E)) == Point2D{Int64}[Point2D{Int64}((4,3)), Point2D{Int64}((5,3))]
@test collect(accessibles(fr, Point(3,3), CL_W)) == Point2D{Int64}[Point2D{Int64}((1,3)), Point2D{Int64}((2,3))]

################################################################################
# Halpern and Shoham's Modal Logic of Time Intervals (HS)
################################################################################
using SoleLogics: IA_A, IA_L, IA_B, IA_E, IA_D, IA_O
using SoleLogics: IA_Ai, IA_Li, IA_Bi, IA_Ei, IA_Di, IA_Oi

fr = FullDimensionalFrame((10,), Interval{Int})
@test collect(accessibles(fr, Interval(4,8), IA_A)) == Interval{Int64}[
    Interval{Int64}(8,9),
    Interval{Int64}(8,10),
    Interval{Int64}(8,11)
]
@test collect(accessibles(fr, Interval(4,8), IA_L)) == Interval{Int64}[
    Interval{Int64}(9,10),
    Interval{Int64}(9,11),
    Interval{Int64}(10,11)
]
@test collect(accessibles(fr, Interval(4,8), IA_B)) == Interval{Int64}[
    Interval{Int64}(4,5),
    Interval{Int64}(4,6),
    Interval{Int64}(4,7)
]
@test collect(accessibles(fr, Interval(4,8), IA_E)) == Interval{Int64}[
    Interval{Int64}(5,8),
    Interval{Int64}(6,8),
    Interval{Int64}(7,8)
]
@test collect(accessibles(fr, Interval(4,8), IA_D)) == Interval{Int64}[
    Interval{Int64}(5,6),
    Interval{Int64}(5,7),
    Interval{Int64}(6,7)
]
@test collect(accessibles(fr, Interval(4,8), IA_O)) == Interval{Int64}[
    Interval{Int64}(5,9),
    Interval{Int64}(5,10),
    Interval{Int64}(5,11),
    Interval{Int64}(6,9),
    Interval{Int64}(6,10),
    Interval{Int64}(6,11),
    Interval{Int64}(7,9),
    Interval{Int64}(7,10),
    Interval{Int64}(7,11)
]
@test collect(accessibles(fr, Interval(4,8), IA_Ai)) == Interval{Int64}[
    Interval{Int64}(1,4),
    Interval{Int64}(2,4),
    Interval{Int64}(3,4)
]
@test collect(accessibles(fr, Interval(4,8), IA_Li)) == Interval{Int64}[
    Interval{Int64}(1,2),
    Interval{Int64}(1,3),
    Interval{Int64}(2,3)
]
@test collect(accessibles(fr, Interval(4,8), IA_Bi)) == Interval{Int64}[
    Interval{Int64}(4,9),
    Interval{Int64}(4,10),
    Interval{Int64}(4,11)
]
@test collect(accessibles(fr, Interval(4,8), IA_Ei)) == Interval{Int64}[
    Interval{Int64}(1,8),
    Interval{Int64}(2,8),
    Interval{Int64}(3,8)
]
@test collect(accessibles(fr, Interval(4,8), IA_Di)) == Interval{Int64}[
    Interval{Int64}(1,9),
    Interval{Int64}(1,10),
    Interval{Int64}(1,11),
    Interval{Int64}(2,9),
    Interval{Int64}(2,10),
    Interval{Int64}(2,11),
    Interval{Int64}(3,9),
    Interval{Int64}(3,10),
    Interval{Int64}(3,11)
]
@test collect(accessibles(fr, Interval(4,8), IA_Oi)) == Interval{Int64}[
    Interval{Int64}(1,5),
    Interval{Int64}(1,6),
    Interval{Int64}(1,7),
    Interval{Int64}(2,5),
    Interval{Int64}(2,6),
    Interval{Int64}(2,7),
    Interval{Int64}(3,5),
    Interval{Int64}(3,6),
    Interval{Int64}(3,7)
]

################################################################################
# Lutz and Wolter's Modal Logic of Topological Relations with rectangular
# areas aligned with the axes (LRCC8_Rec)
################################################################################
using SoleLogics: LRCC8_Rec_DC, LRCC8_Rec_EC, LRCC8_Rec_PO
using SoleLogics: LRCC8_Rec_TPP, LRCC8_Rec_TPPi, LRCC8_Rec_NTPP, LRCC8_Rec_NTPPi

fr = FullDimensionalFrame((5,5), Interval2D{Int})
@test length(
    collect(accessibles(fr, Interval2D((3,4),(3,4)), LRCC8_Rec_DC))
) == 56
fr = FullDimensionalFrame((3,3), Interval2D{Int})
@test length(
    collect(accessibles(fr, Interval2D((2,3),(2,3)), LRCC8_Rec_EC))
) == 20
fr = FullDimensionalFrame((4,4), Interval2D{Int})
@test length(
    collect(accessibles(fr, Interval2D((2,4),(2,4)), LRCC8_Rec_PO))
) == 40
fr = FullDimensionalFrame((5,5), Interval2D{Int})
@test length(
    collect(accessibles(fr, Interval2D((2,5),(2,5)), LRCC8_Rec_TPP))
) == 34
fr = FullDimensionalFrame((6,6), Interval2D{Int})
@test length(
    collect(accessibles(fr, Interval2D((2,6),(2,6)), LRCC8_Rec_NTPP))
) == 9
fr = FullDimensionalFrame((4,4), Interval2D{Int})
@test length(
    collect(accessibles(fr, Interval2D((2,4),(2,4)), LRCC8_Rec_TPPi))
) == 14
fr = FullDimensionalFrame((5,5), Interval2D{Int})
@test length(
    collect(accessibles(fr, Interval2D((3,4),(3,4)), LRCC8_Rec_NTPPi))
) == 16
