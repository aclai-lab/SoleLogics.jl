using FunctionWrappers
using FunctionWrappers: FunctionWrapper
using SoleLogics
using SoleLogics: FunctionalWorldFilter

f(i::Interval{Int})::Bool = true
fw = FunctionWrapper{Bool, Tuple{Interval{Int}}}(f)
i = Interval{Int}(1,2)

fwf = FunctionalWorldFilter{Interval{Int}, typeof(f)}(fw)
@test fwf.filter(i) == true
@test_throws MethodError(convert, (Interval{Int64}, 2), 0x00000000000082f3) fwf.filter(2)

fwf = FunctionalWorldFilter{Interval{Int}}(fw, typeof(f))
@test fwf.filter(i) == true
@test_throws MethodError(convert, (Interval{Int64}, 2), 0x00000000000082f3) fwf.filter(2)

fwf = FunctionalWorldFilter(fw, typeof(f))
@test fwf.filter(i) == true
@test_throws MethodError(convert, (Interval{Int64}, 2), 0x00000000000082f3) fwf.filter(2)

@test_logs (
    :warn,
    "FunctionalWorldFilter initialized without specifying the functiontype.\n"*
    "Please consider using the following syntax instead:\n"*
    "  FunctionalWorldFilter(FunctionWrapper{Bool, Tuple{W}}(f), typeof(f))\n"*
    "where W is a subtype of AbstractWorld and f is a Function."
) fwf = FunctionalWorldFilter(fw)
@test fwf.filter(i) == true
@test_throws MethodError(convert, (Interval{Int64}, 2), 0x00000000000082f4) fwf.filter(2)

fwf = FunctionalWorldFilter{Interval{Int}, typeof(f)}(f)
@test fwf.filter(i) == true
@test_throws MethodError(convert, (Interval{Int64}, 2), 0x00000000000082f4) fwf.filter(2)

fwf = FunctionalWorldFilter{Interval{Int}}(f)
@test fwf.filter(i) == true
@test_throws MethodError(convert, (Interval{Int64}, 2), 0x00000000000082f4) fwf.filter(2)

fwf = FunctionalWorldFilter(f, Interval{Int})
@test fwf.filter(i) == true
@test_throws MethodError(convert, (Interval{Int64}, 2), 0x00000000000082f4) fwf.filter(2)

@test_logs (
    :warn,
    "FunctionalWorldFilter initialized without specifying the worldtype.\n"*
    "Plese consider using the following syntax instead:\n"*
    "  FunctionalWorldFilter(filter, worldtype)\n"*
    "where worldtype is a subtype of AbstractWorld and filter is a Function."
) fwf = FunctionalWorldFilter(f)
@test fwf.filter(i) == true
@test_throws MethodError(convert, (Interval{Int64}, 2), 0x00000000000082f5) fwf.filter(2)
