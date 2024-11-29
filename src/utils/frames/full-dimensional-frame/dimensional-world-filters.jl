# TODO docstring for IntervalLengthFilter. World filter on a thresholding of Base.length(w).
struct IntervalLengthFilter{F<:Function,T<:Real,W<:Interval} <: WorldFilter{W}
    f::F # e.g., >=, <
    k::T # e.g., 3, 10

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

function accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{<:AbstractRelation,IntervalLengthFilter{F,T,W}}) where {F<:Function,T<:Real,W<:Interval{<:Integer}}
    return IterTools.imap(W, _accessibles(fr, w, r))
end

function accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{GlobalRel,IntervalLengthFilter{F, T, W}}) where {F<:Function, T<:Real, W<:Interval{<:Integer}}
    return IterTools.imap(W, _accessibles(fr, w, r))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{<:AbstractRelation,IntervalLengthFilter{F,T,W}}) where {F<:Function,T<:Real,W<:Interval{<:Integer}}
    return error("Please provide a method for _accessibles(fr::$(typeof(fr)), w::$(typeof(w)), r::$(typeof(r))).")
end


function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{GlobalRel,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
	return Iterators.filter(((x,y),)->((x<y)&&((y-x)≤worldfilter(r).k)), Iterators.product(1:X(fr), 2:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{GlobalRel,IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
	return Iterators.filter(((x,y),)->((x<y)&&((y-x)≥worldfilter(r).k)), Iterators.product(1:X(fr), 2:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{GlobalRel,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
	return Iterators.filter(((x,y),)->((x<y)&&((y-x)==worldfilter(r).k)), Iterators.product(1:X(fr), 2:X(fr)+1))
end
