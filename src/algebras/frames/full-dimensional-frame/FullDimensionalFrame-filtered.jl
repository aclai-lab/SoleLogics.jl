function _accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A,IntervalLengthFilter{typeof(≤)}})
	return zip(Iterators.repeated(w.y), w.y+1:X(fr)+1-(r.f.k))
end

function _accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A, IntervalLengthFilter{typeof(≥)}})
	return zip(Iterators.repeated(w.y), w.y+1+(r.f.k):X(fr)+1)
end

function _accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A,IntervalLengthFilter{typeof(==)}})
	return zip(Iterators.repeated(w.y), w.y+1+(r.f.k):X(fr)+1-(r.f.k))
end
