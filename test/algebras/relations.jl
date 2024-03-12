using Test
using FunctionWrappers
using FunctionWrappers: FunctionWrapper
using SoleLogics
using SoleLogics: FullDimensionalFrame, allworlds
using SoleLogics: FunctionalWorldFilter, IntervalLengthFilter
using SoleLogics: filterworlds, FilteredRelation
using BenchmarkTools

f1(i::Interval{Int})::Bool = length(i) ≥ 3
funcw = FunctionWrapper{Bool,Tuple{Interval{Int}}}(f1)
fr = SoleLogics.FullDimensionalFrame(10)
myworlds = SoleLogics.allworlds(fr)

wf = FunctionalWorldFilter{Interval{Int},typeof(f1)}(funcw)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter{Interval{Int}}(funcw, typeof(f1))
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter(funcw, typeof(f1))
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf_lf = IntervalLengthFilter(≥, 3)
@test length(collect(filterworlds(wf_lf, myworlds))) == 36
@test_nowarn collect(filterworlds(wf_lf, [2])) # Warn abouth this behavior!!

bigfr = SoleLogics.FullDimensionalFrame(40)
collect(accessibles(bigfr, Interval(1, 2), IA_L))
# collect(accessibles(bigfr, Interval(1, 2), FilteredRelation(IA_L, wf)))
# collect(accessibles(bigfr, Interval(1, 2), FilteredRelation(IA_L, wf_lf)))

@test_logs ( :warn, ) wf = FunctionalWorldFilter(funcw)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter{Interval{Int},typeof(f1)}(f1)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter{Interval{Int}}(f1)
@test length(collect(filterworlds(wf, myworlds))) == 36
@test_throws MethodError collect(filterworlds(wf, [2]))

wf = FunctionalWorldFilter(f1, Interval{Int})
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

@test accessibles(
    fr,
    first(allworlds(fr)),
    FilteredRelation(globalrel, wf)
) == accessibles(
    fr,
    first(allworlds(fr)),
    globalrel
)

fr = FullDimensionalFrame(50)
worlds = allworlds(fr)
operators = [≤, ≥, ==]

for w in worlds
    for r in IARelations
        for o in operators
            for l in 1:50
                @test collect(accessibles(
                    fr,
                    w,
                    FilteredRelation(r, FunctionalWorldFilter{Interval}(i->o(i.y-i.x, l)))
                )) == collect(accessibles(
                    fr,
                    w,
                    FilteredRelation(r, IntervalLengthFilter(o, l))
                ))
            end
        end
        break
    end
end
