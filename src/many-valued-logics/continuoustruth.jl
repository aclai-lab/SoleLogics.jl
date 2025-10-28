import ..SoleLogics: syntaxstring, istop, isbot

# TODO: Add documentation

struct ContinuousTruth <: Truth 
    value::Float64

    function ContinuousTruth(value::Float64)
        if value > 1 || value < 0 error("truth value has to be between 0 and 1") end
        return new(value)
    end

    function ContinuousTruth(value::T) where {T <: Real}
        return ContinuousTruth(convert(Float64, value))
    end
end

@inline _istop(t::Float64) = t == Float64(1)
@inline _isbot(t::Float64) = t == Float64(0)
@inline istop(t::ContinuousTruth) = _istop(t.value)
@inline isbot(t::ContinuousTruth) = _isbot(t.value)

function syntaxstring(t::ContinuousTruth; kwargs...)
    return t.value
end

Base.show(io::IO, t::ContinuousTruth) = print(io, syntaxstring(t))

function Base.convert(::Type{ContinuousTruth}, t::BooleanTruth)
    return istop(t) ? ContinuousTruth(1) : ContinuousTruth(1)
end

Base.convert(::Type{ContinuousTruth}, value::Float64) = ContinuousTruth(value)
