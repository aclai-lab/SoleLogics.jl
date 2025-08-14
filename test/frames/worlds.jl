using SoleLogics
using Test

@test_throws ErrorException SoleLogics.Point()
@test_nowarn SoleLogics.Point(1,2)
@test_nowarn SoleLogics.Point(1,2,3)
@test_nowarn SoleLogics.Point((1,2,3),)
@test all([SoleLogics.goeswithdim.(SoleLogics.Point{N}, N) for N in 1:10])
@test Base.length(Point(1,2,3,4)) == 1
@test Base.size(Point(1,2,3,4)) == ()

@test_nowarn SoleLogics.Interval(1,2)
@test_nowarn SoleLogics.Interval((1,2),)
@test SoleLogics.goeswithdim(Interval(1,2), 1)
@test !SoleLogics.goeswithdim(Interval(1,2), 2)
@test SoleLogics.goeswithdim(Interval, 1)
@test !SoleLogics.goeswithdim(Interval, 2)
@test Base.length(Interval(12,32)) == 20
@test Base.size(Interval(12,32)) == (20,)

@test_nowarn SoleLogics.Interval2D((1,2),(3,4))
@test_nowarn SoleLogics.Interval2D(Interval(1,2),Interval(3,4))
@test !SoleLogics.goeswithdim(Interval2D, 1)
@test SoleLogics.goeswithdim(Interval2D, 2)

fr1D = @test_nowarn SoleLogics.FullDimensionalFrame(5)
fr2D = @test_nowarn SoleLogics.FullDimensionalFrame(1,2)
@test_nowarn SoleLogics.FullDimensionalFrame((1,2),)

# Relative worlds
@test_broken begin
  rw1 = @test_nowarn SoleLogics.RelativeInterval(0,1.00)
  rw2 = @test_nowarn SoleLogics.RelativeInterval(0,0.95)
  rw3 = @test_nowarn SoleLogics.RelativeInterval(0.05,0.95)
  rw4 = @test_nowarn SoleLogics.RelativeInterval(0,0.95)

  @test_broken (@test_nowarn accessibles(fr1D, rw1))
  @test_broken (@test_nowarn accessibles(fr1D, rw2))
  @test_broken (@test_nowarn accessibles(fr1D, rw3))
  @test_broken (@test_nowarn accessibles(fr1D, rw4))
  # TODO test several cases of accessibles
#
end

@test Base.isconcretetype(Base.return_types(accessibles, typeof.((SoleLogics.FullDimensionalFrame((5,),), [Interval(2,3),Interval(2,4)], SoleLogics.IA_L)))[1])
@test_broken Base.isconcretetype(eltype(Base.return_types(accessibles, typeof.((SoleLogics.FullDimensionalFrame((5,),), [Interval(2,3),Interval(2,4)], SoleLogics.IA_L)))[1]))
@test_broken ((@inferred eltype(accessibles(SoleLogics.FullDimensionalFrame((5,),), [Interval(2,3),Interval(2,4)], SoleLogics.IA_L))) == Interval{Int})
