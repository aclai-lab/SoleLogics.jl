struct IntervalLengthFilter{F<:Function,T<:Real,W<:Interval} <: WorldFilter{W}
    f::F
    k::T

    function IntervalLengthFilter{F,T,W}(f::F, k::T) where {F<:Function,T<:Real,W<:Interval}
        return new{F,T,W}(f, k)
    end
    function IntervalLengthFilter{F,T}(f::F, k::T) where {F<:Function,T<:Real}
        return IntervalLengthFilter{F,T,Interval{Int}}(f, k) # Default
    end
    function IntervalLengthFilter(f::F, k::T) where {F<:Function,T<:Real}
        return IntervalLengthFilter{F,T}(f, k)
    end

end

function filterworlds(wf::IntervalLengthFilter, worlds) # ::AbstractArray{W}) where {W<:Interval}
    return Iterators.filter(w -> wf.f(Base.length(w), wf.k), worlds)
end

function accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{<:AbstractRelation,IntervalLengthFilter{F,T,W}}) where {F<:Function,T<:Real,W<:Interval{Int}}
    return IterTools.imap(W, _accessibles(fr, w, r))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{<:AbstractRelation,IntervalLengthFilter{F,T,W}}) where {F<:Function,T<:Real,W<:Interval{Int}}
    return error("Please provide a method for _accessibles(fr::$(typeof(fr)), w::$(typeof(w)), r::$(typeof(r))).")
end
