using ..SoleLogics: AbstractAlgebra
using StaticArrays
import ..SoleLogics: syntaxstring, istop, isbot
import Base: convert

"""
    struct FiniteTruth <: Truth
        index::UInt8
    end

A `FiniteTruth` is represented by its `index` in the domain.

By convention, `⊤` (top, 1) and `⊥` (bot, 0) always have indexes `1` and `2`,
respectively. E.g., α, β, γ would have indexes `3`, `4`, `5`, and so on.
"""
struct FiniteTruth <: Truth
    index::UInt8

    function FiniteTruth(index::UInt8)
        if index < 1 error("0 is not a valid index in Julia") end
        return new(index)
    end

    function FiniteTruth(index::T) where {T<:Unsigned}
        return FiniteTruth(convert(UInt8, index))
    end

    function FiniteTruth(index::T) where {T<:Int}
        return FiniteTruth(convert(UInt8, index))
    end
end

_istop(t::UInt8) = t == UInt8(1)
_isbot(t::UInt8) = t == UInt8(2)
istop(t::FiniteTruth) = _istop(t.index)
isbot(t::FiniteTruth) = _isbot(t.index)

function syntaxstring(t::FiniteTruth; kwargs...)
    if t.index < UInt8(3)
        return Char(UInt16(8867) + t.index)
    else
        return Char(UInt16(942) + t.index)
    end
end

Base.show(io::IO, t::FiniteTruth) = print(io, syntaxstring(t))

function Base.convert(::Type{FiniteTruth}, t::BooleanTruth)
    return istop(t) ? FiniteTruth(UInt8(1)) : FiniteTruth(UInt8(2))
end

# Helper
function Base.convert(::Type{FiniteTruth}, c::Char)
    if convert(UInt16, c) < 945
        error("Please, provide a character between α and ҭ, ⊤ and ⊥")
    elseif convert(UInt16, c) < 1198
        return FiniteTruth(convert(Int16, c) - UInt16(942))
    elseif convert(UInt16, c) < 8868
        error("Please, provide a character between α and ҭ, ⊤ and ⊥")
    elseif convert(UInt16, c) < 8870
        return FiniteTruth(convert(Int16, c) - UInt16(8867))
    else
        error("Please, provide a character between α and ҭ, ⊤ and ⊥")
    end
end
function Base.convert(::Type{FiniteTruth}, s::String)
    if length(s) == 1
        convert(FiniteTruth, s[1])
    else
        error("Please, provide a string of one character")
    end
end
Base.convert(::Type{FiniteTruth}, index::UInt8) = FiniteTruth(index)
