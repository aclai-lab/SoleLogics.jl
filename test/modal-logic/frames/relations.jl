using Test
using FunctionWrappers
using FunctionWrappers: FunctionWrapper
using SoleLogics
using SoleLogics: FullDimensionalFrame, allworlds
using SoleLogics: FunctionalWorldFilter, IntervalLengthFilter
using SoleLogics: filterworlds, FilteredRelation
using SoleLogics: IA7Relations, IA3Relations, IARelations_extended
using BenchmarkTools

f1(i::SoleLogics.Interval{Int})::Bool = length(i) ≥ 3
funcw = FunctionWrapper{Bool,Tuple{SoleLogics.Interval{Int}}}(f1)
fr = FullDimensionalFrame(10)
myworlds = SoleLogics.allworlds(fr)

wf = FunctionalWorldFilter{SoleLogics.Interval{Int},typeof(f1)}(funcw)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter{SoleLogics.Interval{Int}}(funcw, typeof(f1))
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter(funcw, typeof(f1))
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf_lf = SoleLogics.IntervalLengthFilter(≥, 3)
@test length(collect(filterworlds(wf_lf, myworlds))) == 36
@test_nowarn collect(filterworlds(wf_lf, [2])) # Warn abouth this behavior!!

bigfr = FullDimensionalFrame(40)
collect(accessibles(bigfr, SoleLogics.Interval(1, 2), IA_L))
collect(accessibles(bigfr, SoleLogics.Interval(1, 2), FilteredRelation(IA_L, wf)))
collect(accessibles(bigfr, SoleLogics.Interval(1, 2), FilteredRelation(IA_L, wf_lf)))

@test_logs ( :warn, ) wf = FunctionalWorldFilter(funcw)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter{SoleLogics.Interval{Int},typeof(f1)}(f1)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter{SoleLogics.Interval{Int}}(f1)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter(f1, SoleLogics.Interval{Int})
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

@test_logs (
    :warn,
    "FunctionalWorldFilter initialized without specifying the worldtype.\n" *
    "Plese consider using the following syntax instead:\n" *
    "  FunctionalWorldFilter(filter, worldtype)\n" *
    "where worldtype is a subtype of AbstractWorld and filter is a Function."
) wf = FunctionalWorldFilter(f1)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

@test collect(accessibles(
    fr,
    first(allworlds(fr)),
    FilteredRelation(globalrel, wf)
)) == collect(accessibles(
    fr,
    first(allworlds(fr)),
    FilteredRelation(globalrel, wf_lf)
))

fr = FullDimensionalFrame(20)
worlds = allworlds(fr)
ops = [≤, ≥, ==]


for r in union(IARelations_extended, IA7Relations, IA3Relations)
    # @show r
    for w in worlds
        for o in ops
            for l in 1:10
                @test all(((x,y),)->x == y, zip(accessibles(
                    fr,
                    w,
                    FilteredRelation(r, FunctionalWorldFilter{SoleLogics.Interval}(i->o(i.y-i.x, l)))
                ),accessibles(
                    fr,
                    w,
                    FilteredRelation(r, SoleLogics.IntervalLengthFilter(o, l))
                )))
            end
        end
    end
end
