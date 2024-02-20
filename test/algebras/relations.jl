using Test
using FunctionWrappers
using FunctionWrappers: FunctionWrapper
using SoleLogics
using SoleLogics: FunctionalWorldFilter
using SoleLogics: filterworlds

f1(i::Interval{Int})::Bool = true
funcw = FunctionWrapper{Bool,Tuple{Interval{Int}}}(f1)
i = Interval{Int}(1, 2)

wf = FunctionalWorldFilter{Interval{Int},typeof(f1)}(funcw)
@test length(filterworlds(wf, [i])) == 1
@test_throws MethodError filterworlds(wf, [2])

wf = FunctionalWorldFilter{Interval{Int}}(funcw, typeof(f1))
@test length(filterworlds(wf, [i])) == 1
@test_throws MethodError filterworlds(wf, [2])

wf = FunctionalWorldFilter(funcw, typeof(f1))
@test length(filterworlds(wf, [i])) == 1
@test_throws MethodError filterworlds(wf, [2])

@test_logs (
    :warn,
    "FunctionalWorldFilter initialized without specifying the functiontype.\n" *
    "Please consider using the following syntax instead:\n" *
    "  FunctionalWorldFilter(FunctionWrapper{Bool, Tuple{W}}(filter), typeof(filter))\n" *
    "where W is a subtype of AbstractWorld and filter is a Function."
) wf = FunctionalWorldFilter(funcw)
@test length(filterworlds(wf, [i])) == 1
@test_throws MethodError filterworlds(wf, [2])

wf = FunctionalWorldFilter{Interval{Int},typeof(f1)}(f1)
@test length(filterworlds(wf, [i])) == 1
@test_throws MethodError filterworlds(wf, [2])

wf = FunctionalWorldFilter{Interval{Int}}(f1)
@test length(filterworlds(wf, [i])) == 1
@test_throws MethodError filterworlds(wf, [2])

wf = FunctionalWorldFilter(f1, Interval{Int})
@test length(filterworlds(wf, [i])) == 1
@test_throws MethodError filterworlds(wf, [2])

@test_logs (
    :warn,
    "FunctionalWorldFilter initialized without specifying the worldtype.\n" *
    "Plese consider using the following syntax instead:\n" *
    "  FunctionalWorldFilter(filter, worldtype)\n" *
    "where worldtype is a subtype of AbstractWorld and filter is a Function."
) wf = FunctionalWorldFilter(f1)
@test length(filterworlds(wf, [i])) == 1
@test_throws MethodError filterworlds(wf, [2])

# TODO test accessibles(fr, first(worlds(fr)), FilteredRelation(globalrel, wf))
# TODO test accessibles(FullDimensionalFrame(15), Interval(2,5), FilteredRelation(IA_A, w->(w.y-w.x > 3)))
# TODO test accessibles(FullDimensionalFrame(15), Interval(2,5), FilteredRelation(IA_A, IntervalLengthFilter(5))))