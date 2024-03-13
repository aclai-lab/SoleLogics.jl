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

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_L,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x≤r.wf.k, Iterators.product(w.y+1:X(fr), w.y+2:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_L, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x≥r.wf.k, Iterators.product(w.y+1:X(fr), w.y+2:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_L,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x==r.wf.k, Iterators.product(w.y+1:X(fr), w.y+2:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Li,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x≤r.wf.k, Iterators.product(1:w.x-2, 2:w.x-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Li, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x≥r.wf.k, Iterators.product(1:w.x-2, 2:w.x-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Li,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x==r.wf.k, Iterators.product(1:w.x-2, 2:w.x-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_B,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.x), w.x+1:min(w.x+r.wf.k,w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_B, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.x), max(w.x+r.wf.k,w.x+1):w.y-1)
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_B,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.x), max(w.x+r.wf.k,w.x+1):min(w.x+r.wf.k,w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Bi,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.x), w.y+1:min(w.x+r.wf.k,X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Bi, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.x), max(w.x+r.wf.k,w.y+1):X(fr)+1)
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Bi,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(Iterators.repeated(w.x), max(w.x+r.wf.k,w.y+1):min(w.x+r.wf.k,X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_E,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(max(w.y-r.wf.k,w.x+1):w.y-1, Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_E, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(w.x+1:w.y-r.wf.k, Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_E,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(max(w.y-r.wf.k,w.x+1):w.y-r.wf.k, Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ei,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(max(w.y-r.wf.k,1):w.x-1, Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ei, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(1:min(w.y-r.wf.k, w.x-1), Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ei,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return zip(max(w.y-r.wf.k,1):min(w.y-r.wf.k, w.x-1), Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_D,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x≤r.wf.k, Iterators.product(w.x+1:w.y-2, w.x+2:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_D, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x≥r.wf.k, Iterators.product(w.x+1:w.y-2, w.x+2:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_D,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->x<y&&y-x==r.wf.k, Iterators.product(w.x+1:w.y-2, w.x+2:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Di,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≤r.wf.k, Iterators.product(1:w.x-1, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Di, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≥r.wf.k, Iterators.product(1:w.x-1, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Di,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x==r.wf.k, Iterators.product(1:w.x-1, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_O,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≤r.wf.k, Iterators.product(w.x+1:w.y-1, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_O, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≥r.wf.k, Iterators.product(w.x+1:w.y-1, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_O,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x==r.wf.k, Iterators.product(w.x+1:w.y-1, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Oi,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≤r.wf.k, Iterators.product(1:w.x-1, w.x+1:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Oi, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≥r.wf.k, Iterators.product(1:w.x-1, w.x+1:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Oi,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x==r.wf.k, Iterators.product(1:w.x-1, w.x+1:w.y-1))
end
