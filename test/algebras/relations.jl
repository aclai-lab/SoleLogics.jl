using FunctionWrappers
using FunctionWrappers: FunctionWrapper
using SoleLogics
using SoleLogics: FunctionalWorldFilter

f1(i::Interval{Int})::Bool = true
fw = FunctionWrapper{Bool, Tuple{Interval{Int}}}(f1)
i = Interval{Int}(1,2)

fwf = FunctionalWorldFilter{Interval{Int}, typeof(f1)}(fw)
@test fwf.filter(i) == true
@test_throws MethodError fwf.filter(2)

fwf = FunctionalWorldFilter{Interval{Int}}(fw, typeof(f1))
@test fwf.filter(i) == true
@test_throws MethodError fwf.filter(2)

fwf = FunctionalWorldFilter(fw, typeof(f1))
@test fwf.filter(i) == true
@test_throws MethodError fwf.filter(2)

@test_logs (
    :warn,
    "FunctionalWorldFilter initialized without specifying the functiontype.\n"*
    "Please consider using the following syntax instead:\n"*
    "  FunctionalWorldFilter(FunctionWrapper{Bool, Tuple{W}}(filter), typeof(filter))\n"*
    "where W is a subtype of AbstractWorld and filter is a Function."
) fwf = FunctionalWorldFilter(fw)
@test fwf.filter(i) == true
@test_throws MethodError fwf.filter(2)

fwf = FunctionalWorldFilter{Interval{Int}, typeof(f1)}(f1)
@test fwf.filter(i) == true
@test_throws MethodError fwf.filter(2)

fwf = FunctionalWorldFilter{Interval{Int}}(f1)
@test fwf.filter(i) == true
@test_throws MethodError fwf.filter(2)

fwf = FunctionalWorldFilter(f1, Interval{Int})
@test fwf.filter(i) == true
@test_throws MethodError fwf.filter(2)

@test_logs (
    :warn,
    "FunctionalWorldFilter initialized without specifying the worldtype.\n"*
    "Plese consider using the following syntax instead:\n"*
    "  FunctionalWorldFilter(filter, worldtype)\n"*
    "where worldtype is a subtype of AbstractWorld and filter is a Function."
) fwf = FunctionalWorldFilter(f1)
@test fwf.filter(i) == true
@test_throws MethodError fwf.filter(2)
