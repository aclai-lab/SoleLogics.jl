"""
    abstract type WorldFilter{W<:AbstractWorld} end

An object that selects some worlds. Can be used in filtered relations.

See also [`filterworlds`](@ref), [`FilteredRelation`](@ref).
"""
abstract type WorldFilter{W<:AbstractWorld} end

"""
    filterworlds(wf::WorldFilter, worlds)

Return an iterator to the `worlds` that satisfy the filter.
"""
function filterworlds(wf::WorldFilter, worlds) # ::AbstractArray{W}) where {W<:AbstractWorld}
    return error("Please, provide method filterworlds(::$(typeof(wf)), ::$(typeof(worlds))).")
end

function (wf::WorldFilter)(worlds) # ::AbstractArray{W}) where {W<:AbstractWorld}
    return filterworlds(wf, worlds)
end
