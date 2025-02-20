function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_A,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(Iterators.repeated(w.y), w.y+1:min(w.y+worldfilter(r).k,X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_A, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(Iterators.repeated(w.y), w.y+worldfilter(r).k:X(fr)+1)
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_A,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return w.y+worldfilter(r).k < X(fr)+2 ? [(w.y, w.y+worldfilter(r).k)] : W[]
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ai,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(max(w.x-worldfilter(r).k,1):w.x-1, Iterators.repeated(w.x))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ai, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(1:w.x-worldfilter(r).k, Iterators.repeated(w.x))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ai,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return w.x-worldfilter(r).k > 0 ? [(w.x-worldfilter(r).k, w.x)] : W[]
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_L,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x≤worldfilter(r).k, Iterators.product(w.y+1:X(fr), w.y+2:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_L, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x≥worldfilter(r).k, Iterators.product(w.y+1:X(fr), w.y+2:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_L,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x==worldfilter(r).k, Iterators.product(w.y+1:X(fr), w.y+2:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Li,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x≤worldfilter(r).k, Iterators.product(1:w.x-2, 2:w.x-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Li, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x≥worldfilter(r).k, Iterators.product(1:w.x-2, 2:w.x-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Li,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x==worldfilter(r).k, Iterators.product(1:w.x-2, 2:w.x-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_B,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(Iterators.repeated(w.x), w.x+1:min(w.x+worldfilter(r).k,w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_B, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(Iterators.repeated(w.x), max(w.x+worldfilter(r).k,w.x+1):w.y-1)
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_B,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return w.x+worldfilter(r).k < w.y ? [(w.x, w.x+worldfilter(r).k)] : W[]
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Bi,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(Iterators.repeated(w.x), w.y+1:min(w.x+worldfilter(r).k,X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Bi, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(Iterators.repeated(w.x), max(w.x+worldfilter(r).k,w.y+1):X(fr)+1)
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Bi,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return w.x+worldfilter(r).k > w.y && w.x+worldfilter(r).k < X(fr)+2 ? [(w.x, w.x+worldfilter(r).k)] : W[]
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_E,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(max(w.y-worldfilter(r).k,w.x+1):w.y-1, Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_E, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(w.x+1:w.y-worldfilter(r).k, Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_E,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return w.y-worldfilter(r).k > w.x ? [(w.y-worldfilter(r).k, w.y)] : W[]
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ei,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(max(w.y-worldfilter(r).k,1):w.x-1, Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ei, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return zip(1:min(w.y-worldfilter(r).k, w.x-1), Iterators.repeated(w.y))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Ei,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return w.y-worldfilter(r).k < w.x && w.y-worldfilter(r).k > 0 ? [(w.y-worldfilter(r).k, w.y)] : W[]
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_D,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x≤worldfilter(r).k, Iterators.product(w.x+1:w.y-2, w.x+2:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_D, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x≥worldfilter(r).k, Iterators.product(w.x+1:w.y-2, w.x+2:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_D,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->x<y&&y-x==worldfilter(r).k, Iterators.product(w.x+1:w.y-2, w.x+2:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Di,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x≤worldfilter(r).k, ( (x,y) for x in 1:w.x-1 for y in w.y+1:X(fr)+1 ))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Di, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x≥worldfilter(r).k, ( (x,y) for x in 1:w.x-1 for y in w.y+1:X(fr)+1 ))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Di,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x==worldfilter(r).k, ( (x,y) for x in 1:w.x-1 for y in w.y+1:X(fr)+1 ))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_O,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x≤worldfilter(r).k, ( (x,y) for x in w.x+1:w.y-1 for y in w.y+1:X(fr)+1 ))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_O, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x≥worldfilter(r).k, ( (x,y) for x in w.x+1:w.y-1 for y in w.y+1:X(fr)+1 ))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_O,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x==worldfilter(r).k, ( (x,y) for x in w.x+1:w.y-1 for y in w.y+1:X(fr)+1 ))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Oi,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x≤worldfilter(r).k, ( (x,y) for x in 1:w.x-1 for y in w.x+1:w.y-1 ))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Oi, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x≥worldfilter(r).k, ( (x,y) for x in 1:w.x-1 for y in w.x+1:w.y-1 ))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_Oi,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
    return Iterators.filter(((x,y),)->y-x==worldfilter(r).k, ( (x,y) for x in 1:w.x-1 for y in w.x+1:w.y-1 ))
end

# ################################################################################
# ################################################################################
# ################################################################################

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AorO,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≤r.wf.k, Iterators.product(w.x+1:w.y, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AorO, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≥r.wf.k, Iterators.product(w.x+1:w.y, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AorO,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x==r.wf.k, Iterators.product(w.x+1:w.y, w.y+1:X(fr)+1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AiorOi,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≤r.wf.k, Iterators.product(1:w.x-1,   w.x:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AiorOi, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x≥r.wf.k, Iterators.product(1:w.x-1,   w.x:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AiorOi,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
	return Iterators.filter(((x,y),)->y-x==r.wf.k, Iterators.product(1:w.x-1,   w.x:w.y-1))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DorBorE,IntervalLengthFilter{F,T,W}}) where {F<:Function,T<:Real,W<:Interval{Int}}
	return Iterators.flatten((
		_accessibles(fr, w, FilteredRelation(IA_B,IntervalLengthFilter(r.wf.f,r.wf.k))),
		_accessibles(fr, w, FilteredRelation(IA_D,IntervalLengthFilter(r.wf.f,r.wf.k))),
		_accessibles(fr, w, FilteredRelation(IA_E,IntervalLengthFilter(r.wf.f,r.wf.k)))
	))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DiorBiorEi,IntervalLengthFilter{F,T,W}}) where {F<:Function,T<:Real,W<:Interval{Int}}
	return Iterators.flatten((
		_accessibles(fr, w, FilteredRelation(IA_Bi,IntervalLengthFilter(r.wf.f,r.wf.k))),
		_accessibles(fr, w, FilteredRelation(IA_Di,IntervalLengthFilter(r.wf.f,r.wf.k))),
		_accessibles(fr, w, FilteredRelation(IA_Ei,IntervalLengthFilter(r.wf.f,r.wf.k)))
	))
end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DorBorE,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_B,IntervalLengthFilter(≤,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_D,IntervalLengthFilter(≤,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_E,IntervalLengthFilter(≤,r.wf.k)))
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DorBorE, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_B,IntervalLengthFilter(≥,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_D,IntervalLengthFilter(≥,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_E,IntervalLengthFilter(≥,r.wf.k)))
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DorBorE,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_B,IntervalLengthFilter(==,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_D,IntervalLengthFilter(==,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_E,IntervalLengthFilter(==,r.wf.k)))
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DiorBiorEi,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_Bi,IntervalLengthFilter(≤,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_Di,IntervalLengthFilter(≤,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_Ei,IntervalLengthFilter(≤,r.wf.k)))
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DiorBiorEi, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_Bi,IntervalLengthFilter(≥,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_Di,IntervalLengthFilter(≥,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_Ei,IntervalLengthFilter(≥,r.wf.k)))
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DiorBiorEi,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_Bi,IntervalLengthFilter(==,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_Di,IntervalLengthFilter(==,r.wf.k))),
# 		_accessibles(fr, w, FilteredRelation(IA_Ei,IntervalLengthFilter(==,r.wf.k)))
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_I,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	Iterators.flatten((
# 		Iterators.filter(((x,y),)->y-x≤r.wf.k, Iterators.product(1:w.y, w.y+1:X(fr)+1)), # Di+A+O+Bi
# 		Iterators.filter(((x,y),)->y-x≤r.wf.k, Iterators.product(1:w.x-1, w.x:w.y)),     # Ai+Oi+Ei
# 		_accessibles(fr, w, FilteredRelation(IA_B,IntervalLengthFilter(≤,r.wf.k))), 	 # B
# 		_accessibles(fr, w, FilteredRelation(IA_E,IntervalLengthFilter(≤,r.wf.k))), 	 # E
# 		_accessibles(fr, w, FilteredRelation(IA_D,IntervalLengthFilter(≤,r.wf.k)))  	 # D
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_I,IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	Iterators.flatten((
# 		Iterators.filter(((x,y),)->y-x≥r.wf.k, Iterators.product(1:w.y, w.y+1:X(fr)+1)), # Di+A+O+Bi
# 		Iterators.filter(((x,y),)->y-x≥r.wf.k, Iterators.product(1:w.x-1, w.x:w.y)),     # Ai+Oi+Ei
# 		_accessibles(fr, w, FilteredRelation(IA_B,IntervalLengthFilter(≥,r.wf.k))), 	 # B
# 		_accessibles(fr, w, FilteredRelation(IA_E,IntervalLengthFilter(≥,r.wf.k))), 	 # E
# 		_accessibles(fr, w, FilteredRelation(IA_D,IntervalLengthFilter(≥,r.wf.k)))  	 # D
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_I,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	Iterators.flatten((
# 		Iterators.filter(((x,y),)->y-x==r.wf.k, Iterators.product(1:w.y, w.y+1:X(fr)+1)), # Di+A+O+Bi
# 		Iterators.filter(((x,y),)->y-x==r.wf.k, Iterators.product(1:w.x-1, w.x:w.y)),     # Ai+Oi+Ei
# 		_accessibles(fr, w, FilteredRelation(IA_B,IntervalLengthFilter(==,r.wf.k))), 	  # B
# 		_accessibles(fr, w, FilteredRelation(IA_E,IntervalLengthFilter(==,r.wf.k))), 	  # E
# 		_accessibles(fr, w, FilteredRelation(IA_D,IntervalLengthFilter(==,r.wf.k)))  	  # D
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{GlobalRel,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	return Iterators.filter(((x,y),)->((x<y)&&((y-x)≤r.wf.k)), Iterators.product(1:X(fr), 2:X(fr)+1))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{GlobalRel,IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{Int}}
# 	return Iterators.filter(((x,y),)->((x<y)&&((y-x)≥r.wf.k)), Iterators.product(1:X(fr), 2:X(fr)+1))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{GlobalRel,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{Int}}

# 	return Iterators.filter(((x,y),)->((x<y)&&((y-x)==r.wf.k)), Iterators.product(1:X(fr), 2:X(fr)+1))
# end

# function _accessibles(
#     fr::Full1DFrame,
#     w::W,
#     r::FilteredRelation{<:IA7Relation, IntervalLengthFilter{<:Function, <:Real, W}}
# ) where {W <: Interval{<:Integer}}
#     return Iterators.flatten((_accessibles(fr, w, FilteredRelation(wr, worldfilter(r))) for wr in IA72IARelations(wrappedrelation(r))))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AorO,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
#     return Iterators.filter(((x,y),)->y-x≤worldfilter(r).k, Iterators.product(w.x+1:w.y, w.y+1:X(fr)+1))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AorO, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
#     return Iterators.filter(((x,y),)->y-x≥worldfilter(r).k, Iterators.product(w.x+1:w.y, w.y+1:X(fr)+1))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AorO,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
#     return Iterators.filter(((x,y),)->y-x==worldfilter(r).k, Iterators.product(w.x+1:w.y, w.y+1:X(fr)+1))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AiorOi,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
#     return Iterators.filter(((x,y),)->y-x≤worldfilter(r).k, Iterators.product(1:w.x-1,   w.x:w.y-1))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AiorOi, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
#     return Iterators.filter(((x,y),)->y-x≥worldfilter(r).k, Iterators.product(1:w.x-1,   w.x:w.y-1))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_AiorOi,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
#     return Iterators.filter(((x,y),)->y-x==worldfilter(r).k, Iterators.product(1:w.x-1,   w.x:w.y-1))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DorBorE,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
#     return Iterators.flatten((
#         _accessibles(fr, w, FilteredRelation(IA_B,worldfilter(r))),
#         _accessibles(fr, w, FilteredRelation(IA_D,worldfilter(r))),
#         _accessibles(fr, w, FilteredRelation(IA_E,worldfilter(r)))
#     ))
#     # return Iterators.filter(((x,y),)->x<y&&y-x==worldfilter(r).k, Iterators.product(w.x:w.y-1, w.x+1:w.y))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DiorBiorEi,IntervalLengthFilter{typeof(≤),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_Bi,worldfilter(r))),
# 		_accessibles(fr, w, FilteredRelation(IA_Di,worldfilter(r))),
# 		_accessibles(fr, w, FilteredRelation(IA_Ei,worldfilter(r)))
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DiorBiorEi, IntervalLengthFilter{typeof(≥),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_Bi,worldfilter(r))),
# 		_accessibles(fr, w, FilteredRelation(IA_Di,worldfilter(r))),
# 		_accessibles(fr, w, FilteredRelation(IA_Ei,worldfilter(r)))
# 	))
# end

# function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_DiorBiorEi,IntervalLengthFilter{typeof(==),T,W}}) where {T<:Real,W<:Interval{<:Integer}}
# 	return Iterators.flatten((
# 		_accessibles(fr, w, FilteredRelation(IA_Bi,worldfilter(r))),
# 		_accessibles(fr, w, FilteredRelation(IA_Di,worldfilter(r))),
# 		_accessibles(fr, w, FilteredRelation(IA_Ei,worldfilter(r)))
# 	))
# end

# ################################################################################
# ################################################################################
# ################################################################################

function _accessibles(
    fr::Full1DFrame,
    w::W,
    r::FilteredRelation{<:IA3Relation, IntervalLengthFilter{<:Function, <:Real, W}}
) where {W <: Interval{<:Integer}}
    return Iterators.flatten((_accessibles(fr, w, FilteredRelation(wr, worldfilter(r))) for wr in IA32IARelations(wrappedrelation(r))))
end

function _accessibles(fr::Full1DFrame, w::W, r::FilteredRelation{_IA_I,IntervalLengthFilter{F,T,W}}) where {F<:Function,T<:Real,W<:Interval{<:Integer}}
    wf = worldfilter(r)
    Iterators.flatten((
        Iterators.filter(((x,y),)->wf.f(y-x, wf.k), Iterators.product(1:w.y, w.y+1:X(fr)+1)), # Di+A+O+Bi
        Iterators.filter(((x,y),)->wf.f(y-x, wf.k), Iterators.product(1:w.x-1, w.x:w.y)),     # Ai+Oi+Ei
        _accessibles(fr, w, FilteredRelation(IA_B,wf)), 	 # B
        _accessibles(fr, w, FilteredRelation(IA_E,wf)), 	 # E
        _accessibles(fr, w, FilteredRelation(IA_D,wf))  	 # D
    ))
end
