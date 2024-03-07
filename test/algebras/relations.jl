using Test
using FunctionWrappers
using FunctionWrappers: FunctionWrapper
using SoleLogics
using SoleLogics: FunctionalWorldFilter, IntervalLengthFilter
using SoleLogics: filterworlds, FilteredRelation

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

# TODO test that the second is faster
bigfr = SoleLogics.FullDimensionalFrame(40)
collect(accessibles(bigfr, Interval(1, 2), IA_L))
collect(accessibles(bigfr, Interval(1, 2), FilteredRelation(IA_L, wf)))
collect(accessibles(bigfr, Interval(1, 2), FilteredRelation(IA_L, wf_lf)))

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

# TODO test accessibles(fr, first(worlds(fr)), FilteredRelation(globalrel, wf))
# TODO test accessibles(FullDimensionalFrame(15), Interval(2,5), FilteredRelation(IA_A, w->(w.y-w.x > 3)))
# TODO test accessibles(FullDimensionalFrame(15), Interval(2,5), FilteredRelation(IA_A, IntervalLengthFilter(5))))