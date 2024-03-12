function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_A,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.y), w.y+1:min(w.y+r.wf.k,X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_A, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.y), w.y+r.wf.k:X(fr)+1)
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_A,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.y), w.y+r.wf.k:min(w.y+r.wf.k,X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ai,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(max(w.x-r.wf.k,1):w.x-1, Iterators.repeated(w.x))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ai, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(1:w.x-r.wf.k, Iterators.repeated(w.x))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ai,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(max(w.x-r.wf.k,1):w.x-r.wf.k, Iterators.repeated(w.x))
end
