abstract type WorldFilter{W<:AbstractWorld} end

function filterworlds(wf::WorldFilter, worlds) # ::AbstractArray{W}) where {W<:AbstractWorld}
    return error("Please, provide method filterworlds(::$(typeof(wf)), ::$(typeof(worlds))).")
end

function (wf::WorldFilter)(worlds) # ::AbstractArray{W}) where {W<:AbstractWorld}
    return filterworlds(wf, worlds)
end
