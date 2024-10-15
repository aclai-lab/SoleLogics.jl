# Given a function whose first argument is of type Union{Random.AbstractRNG,Integer},
# write an identical dispatch defaulting that field to Random.GLOBAL_RNG.
#
# This is useful to adhere to Base.rand methods, where the RNG is not a kwarg and it's
# placed as the first argument.
#
# At the moment, this macro does not handle functions giving a return type hint.
macro __rng_dispatch(ex)
    if ex.head != :function
        throw(ArgumentError("Expected a function definition"))
    end

    fsignature = ex.args[1]
    fname = fsignature.args[1]
    fargs = fsignature.args[2:end]

    # Later, fargs is sliced from 3rd index to end
    if length(fargs) <= 2
        throw(ArgumentError("Expected function's argument to be atleast 2, the first of " *
            "which of type Union{Random.AbstractRNG,Integer}."))
    end

    # The first argument one must be an Union{Random.AbstractRNG,Integer},
    # otherwise, this macro makes no sense.
    quote
        if !isa($fargs[2], Union{Random.AbstractRNG,Integer})
            throw(ArgumentError("Expected function's first argument to be of type " *
                "Union{Random.AbstractRNG,Integer}."))
        end
    end

    # At this point, fargs is shaped similar to:
    #
    # Any[
    #   :($(Expr(:parameters, :(kwargs...)))),
    #   :(rng::Union{Random.AbstractRNG, Integer}),
    #   :(a::AbstractAlphabet), :(args...)
    # ]
    #
    # We would like to remove both kwargs... and `rng`.
    # kwargs are reinserted manually later (just writing $(newargs...) gives problems).
    newargs = Any[fargs[3:end]...]

    # Define both dispatches;
    # the names are the same, and the new dispatches (the one without rng)
    # also gets the docstring written just before the macro invocation.
    quote
        Core.@__doc__ function $(esc(fname))($(newargs...); kwargs...)
            $(esc(fname))(Random.GLOBAL_RNG, $(newargs...); kwargs...)
        end

        $(esc(ex))
    end
end
