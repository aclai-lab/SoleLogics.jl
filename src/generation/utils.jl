# Given a function whose first argument is of type Union{Random.AbstractRNG,Integer},
# write an identical dispatch defaulting that field to Random.GLOBAL_RNG.
#
# This is useful to adhere to Base.rand methods, where the RNG is not a kwarg and it's
# placed as the first argument.
macro __rng_dispatch(ex)
    if ex.head != :function
        throw(ArgumentError("Expected a function definition"))
    end

    fsignature = ex.args[1]
    fname = fsignature.args[1]

    fargs = fsignature.args[2:end]
    # At this point, fargs is shaped similar to:
    # Any[
    #   :($(Expr(:parameters, :(kwargs...)))),
    #   :(rng::Union{Random.AbstractRNG, Integer}), :(a::AbstractAlphabet), :(args...)
    # ]

    # Extract the arguments after the first one;
    # the first one must be an Union{Random.AbstractRNG,Integer},
    # otherwise, this macro makes no sense.
    if length(fargs) > 0
        quote
            if !isa($fargs[1], Union{Random.AbstractRNG,Integer})
                throw(ArgumentError("Expected function's first argument to be of type " *
                    "Union{Random.AbstractRNG,Integer}."))
            end
        end

        # From fargs, we would like to isolate the first field (kwargs),
        # skip the second field (rng), and go on.
        newargs = Any[fargs[1], fargs[3:end]...]
    else
        throw(ArgumentError("Expected function's argument to be atleast 2, the first of " *
            "which of type Union{Random.AbstractRNG,Integer}."))
    end

    # Define both dispatches;
    # the names are the same, and the new dispatches (the one without rng)
    # also gets the docstring written just before the macro invocation.
    quote
        Core.@__doc__ function $(esc(fname))($(newargs...))
            $fname(Random.GLOBAL_RNG, $(newargs))
        end

        $(esc(ex))
    end
end
