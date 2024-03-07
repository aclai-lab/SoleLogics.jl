# TODO IA cases with ≤, ≥ and =
# accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A,IntervalLengthFilter{typeof(≤)}}) = zip(Iterators.repeated(w.y), w.y+1+(r.f.k):X(fr)+1)
function _accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A, IntervalLengthFilter{typeof(≥)}})
  println("aoe")
  zip(Iterators.repeated(w.y), w.y+1+(r.f.k):X(fr)+1)
end

# accessibles(fr::Full1DFrame, w::Interval{Int}, r::FilteredRelation{_IA_A,IntervalLengthFilter{typeof(=)}}) = zip(Iterators.repeated(w.y), w.y+1+(r.f.k):X(fr)+1)
