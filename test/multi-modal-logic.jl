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
