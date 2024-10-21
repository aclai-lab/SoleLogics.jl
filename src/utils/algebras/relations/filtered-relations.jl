using FunctionWrappers
using FunctionWrappers: FunctionWrapper

"""
	struct FunctionalWorldFilter{W <: AbstractWorld, F <: Function} <: WorldFilter{W}
		filter::FunctionWrapper{Bool, Tuple{W}}
	end

A world filter based on a function from `AbstractWorld` to `Bool`.

# Constructors
- `FunctionalWorldFilter{W, F}(filter::FunctionWrapper{Bool, Tuple{W}}) where {W <: AbstractWorld, F <: Function}`
- `FunctionalWorldFilter(filter::FunctionWrapper{Bool, Tuple{W}}, functiontype::Type{F}) where {W <: AbstractWorld, F <: Function}`
- `FunctionalWorldFilter{W, F}(filter::F) where {W <: AbstractWorld, F <: Function}`
- `FunctionalWorldFilter{W}(filter::F) where {W <: AbstractWorld, F <: Function}`
- `FunctionalWorldFilter(filter::F, worldtype::Type{W}) where {W <: AbstractWorld, F <: Function}`

See also [`filterworlds`](@ref), [`FilteredRelation`](@ref).
"""
struct FunctionalWorldFilter{W<:AbstractWorld,F<:Function} <: WorldFilter{W}
    filter::FunctionWrapper{Bool,Tuple{W}}

    function FunctionalWorldFilter{W,F}(filter::FunctionWrapper{Bool,Tuple{W}}) where {W<:AbstractWorld,F<:Function}
        return new{W,F}(filter)
    end

    function FunctionalWorldFilter{W}(filter::FunctionWrapper{Bool,Tuple{W}}, functiontype::Type{F}) where {W<:AbstractWorld,F<:Function}
        return new{W,functiontype}(filter)
    end

    function FunctionalWorldFilter(filter::FunctionWrapper{Bool,Tuple{W}}, functiontype::Type{F}) where {W<:AbstractWorld,F<:Function}
        return FunctionalWorldFilter{W}(filter, functiontype)
    end

    function FunctionalWorldFilter(filter::FunctionWrapper{Bool,Tuple{W}}) where {W<:AbstractWorld}
        @warn "FunctionalWorldFilter initialized without specifying the functiontype.\n" *
              "Please consider using the following syntax instead:\n" *
              "  FunctionalWorldFilter(FunctionWrapper{Bool, Tuple{W}}(filter), typeof(filter))\n" *
              "where W is a subtype of AbstractWorld and filter is a Function."
        return FunctionalWorldFilter(filter, Function)
    end

    function FunctionalWorldFilter{W,F}(filter::F) where {W<:AbstractWorld,F<:Function}
        return FunctionalWorldFilter{W,F}(FunctionWrapper{Bool,Tuple{W}}(filter))
    end

    function FunctionalWorldFilter{W}(filter::F) where {W<:AbstractWorld,F<:Function}
        return FunctionalWorldFilter{W,F}(filter)
    end

    function FunctionalWorldFilter(filter::F, worldtype::Type{W}) where {W<:AbstractWorld,F<:Function}
        return FunctionalWorldFilter{worldtype}(filter)
    end

    function FunctionalWorldFilter(filter::F) where {F<:Function}
        @warn "FunctionalWorldFilter initialized without specifying the worldtype.\n" *
              "Plese consider using the following syntax instead:\n" *
              "  FunctionalWorldFilter(filter, worldtype)\n" *
              "where worldtype is a subtype of AbstractWorld and filter is a Function."
        return FunctionalWorldFilter(filter, AbstractWorld)
    end
end

function filterworlds(wf::FunctionalWorldFilter, worlds) # ::AbstractArray{W}) where {W<:AbstractWorld}
    return Iterators.filter(wf.filter, worlds)
end

"""
    struct FilteredRelation{R<:AbstractRelation,F<:WorldFilter} <: AbstractRelation
        r::R
        wf::F
    end

A (binary) accessibility relation `r`, filtered by a world filter `wf`.
"""
struct FilteredRelation{R<:AbstractRelation,F<:WorldFilter} <: AbstractRelation
    r::R
    wf::F

    function FilteredRelation{R,F}(r::R, wf::F) where {R<:AbstractRelation,F<:WorldFilter}
        return new(r, wf)
    end

    function FilteredRelation(r::R, wf::F) where {R<:AbstractRelation,F<:WorldFilter}
        return FilteredRelation{R,F}(r, wf)
    end

    # TODO constructor that accepts a Callable and wraps it into a FunctionalWorldFilter?
end

wrappedrelation(r::FilteredRelation) = r.r
worldfilter(r::FilteredRelation) = r.wf

function accessibles(
    fr::AbstractMultiModalFrame,
    w::W,
    r::FilteredRelation
) where {W <: AbstractWorld}
    return filterworlds(worldfilter(r), IterTools.imap(W, _accessibles(fr, w, r.r)))
end

function accessibles(
    fr::AbstractMultiModalFrame,
    ::W,
    r::FilteredRelation{GlobalRel,<:WorldFilter{W}}
) where {W <: AbstractWorld}
	return filterworlds(worldfilter(r), accessibles(fr, r.r))
end
