
@test_throws ErrorException SoleLogics.Point()
@test_nowarn SoleLogics.Point(1,2)
@test_nowarn SoleLogics.Point(1,2,3)
@test_nowarn SoleLogics.Point((1,2,3),)
@test all([SoleLogics.goeswithdim.(SoleLogics.Point{N}, N) for N in 1:10])

@test_nowarn SoleLogics.Interval(1,2)
@test_nowarn SoleLogics.Interval((1,2),)
@test SoleLogics.goeswithdim(Interval(1,2), 1)
@test !SoleLogics.goeswithdim(Interval(1,2), 2)
@test SoleLogics.goeswithdim(Interval, 1)
@test !SoleLogics.goeswithdim(Interval, 2)

@test_nowarn SoleLogics.Interval2D((1,2),(3,4))
@test_nowarn SoleLogics.Interval2D(Interval(1,2),Interval(3,4))
@test !SoleLogics.goeswithdim(Interval2D, 1)
@test SoleLogics.goeswithdim(Interval2D, 2)

@test_nowarn SoleLogics.FullDimensionalFrame(1)
@test_nowarn SoleLogics.FullDimensionalFrame(1,2)
@test_nowarn SoleLogics.FullDimensionalFrame((1,2),)

# TODO test several cases of accessibles
# 

@test Base.isconcretetype(Base.return_types(accessibles, typeof.((SoleLogics.FullDimensionalFrame((5,),), [Interval(2,3),Interval(2,4)], SoleLogics.IA_L)))[1])
@test_broken Base.isconcretetype(eltype(Base.return_types(accessibles, typeof.((SoleLogics.FullDimensionalFrame((5,),), [Interval(2,3),Interval(2,4)], SoleLogics.IA_L)))[1]))
@test_broken @inferred eltype(accessibles(SoleLogics.FullDimensionalFrame((5,),), [Interval(2,3),Interval(2,4)], SoleLogics.IA_L)) == Interval{Int}
