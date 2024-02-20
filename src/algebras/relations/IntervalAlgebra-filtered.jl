# TODO IA cases with ≤, ≥ and =
# accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A,IntervalLengthFilter{≤}}) = zip(Iterators.repeated(w.y), w.y+1+(r.f.k):X(fr)+1)
# accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A,IntervalLengthFilter{≥}}) = zip(Iterators.repeated(w.y), w.y+1+(r.f.k):X(fr)+1)
# accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A,IntervalLengthFilter{=}}) = zip(Iterators.repeated(w.y), w.y+1+(r.f.k):X(fr)+1)