using FunctionWrappers
import FunctionWrappers: FunctionWrapper

abstract type WorldFilter{W <: AbstractWorld} end

"""
    struct FunctionalWorldFilter{W <: AbstractWorld, F <: Function} <: WorldFilter{W}
        filter::FunctionWrapper{Bool, Tuple{W}}
    end

    FunctionalWorldFilter{W, F}(filter::FunctionWrapper{Bool, Tuple{W}}) where {W <: AbstractWorld, F <: Function}
    FunctionalWorldFilter(filter::FunctionWrapper{Bool, Tuple{W}}, functiontype::Type{F}) where {W <: AbstractWorld, F <: Function}
    FunctionalWorldFilter{W, F}(filter::F) where {W <: AbstractWorld, F <: Function}
    FunctionalWorldFilter{W}(filter::F) where {W <: AbstractWorld, F <: Function}
    FunctionalWorldFilter(filter::F, worldtype::Type{W}) where {W <: AbstractWorld, F <: Function}

Please provide a function as filter so that it takes as input an object subtype of
AbstractWorld and it gives as output a Bool.
"""
struct FunctionalWorldFilter{W <: AbstractWorld, F <: Function} <: WorldFilter{W}
    filter::FunctionWrapper{Bool, Tuple{W}}

    function FunctionalWorldFilter{W, F}(filter::FunctionWrapper{Bool, Tuple{W}}) where {W <: AbstractWorld, F <: Function}
        return new{W, F}(filter)
    end

    function FunctionalWorldFilter{W}(filter::FunctionWrapper{Bool, Tuple{W}}, functiontype::Type{F}) where {W <: AbstractWorld, F <: Function}
        return new{W, functiontype}(filter)
    end

    function FunctionalWorldFilter(filter::FunctionWrapper{Bool, Tuple{W}}, functiontype::Type{F}) where {W <: AbstractWorld, F <: Function}
        return FunctionalWorldFilter{W}(filter, functiontype)
    end

    function FunctionalWorldFilter(filter::FunctionWrapper{Bool, Tuple{W}}) where {W <: AbstractWorld}
        @warn "FunctionalWorldFilter initialized without specifying the functiontype.\n"*
              "Please consider using the following syntax instead:\n"*
              "  FunctionalWorldFilter(FunctionWrapper{Bool, Tuple{W}}(f), typeof(f))\n"*
              "where W is a subtype of AbstractWorld and f is a Function."
        return FunctionalWorldFilter(filter, Function)
    end

    function FunctionalWorldFilter{W, F}(filter::F) where {W <: AbstractWorld, F <: Function}
        return FunctionalWorldFilter{W, F}(FunctionWrapper{Bool, Tuple{W}}(filter))
    end

    function FunctionalWorldFilter{W}(filter::F) where {W <: AbstractWorld, F <: Function}
        return FunctionalWorldFilter{W, F}(filter)
    end

    function FunctionalWorldFilter(filter::F, worldtype::Type{W}) where {W <: AbstractWorld, F <: Function}
        return FunctionalWorldFilter{worldtype}(filter)
    end

    function FunctionalWorldFilter(filter::F) where {F <: Function}
        @warn "FunctionalWorldFilter initialized without specifying the worldtype.\n"*
              "Plese consider using the following syntax instead:\n"*
              "  FunctionalWorldFilter(filter, worldtype)\n"*
              "where worldtype is a subtype of AbstractWorld and filter is a Function."
        return FunctionalWorldFilter(filter, AbstractWorld)
    end
end
