    
goeswith(::Type{<:Full1DFrame}, ::IntervalRelation) = true

_accessibles(fr::Full1DFrame, w::Interval, ::_IA_A) = zip(Iterators.repeated(w.y), w.y+1:X(fr)+1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_Ai) = zip(1:w.x-1, Iterators.repeated(w.x))
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_L) = _intervals_in(w.y+1, X(fr)+1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_Li) = _intervals_in(1, w.x-1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_B) = zip(Iterators.repeated(w.x), w.x+1:w.y-1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_Bi) = zip(Iterators.repeated(w.x), w.y+1:X(fr)+1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_E) = zip(w.x+1:w.y-1, Iterators.repeated(w.y))
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_Ei) = zip(1:w.x-1, Iterators.repeated(w.y))
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_D) = _intervals_in(w.x+1, w.y-1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_Di) = Iterators.product(1:w.x-1, w.y+1:X(fr)+1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_O) = Iterators.product(w.x+1:w.y-1, w.y+1:X(fr)+1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_Oi) = Iterators.product(1:w.x-1, w.x+1:w.y-1)

_accessibles(fr::Full1DFrame, w::Interval, ::_IA_AorO) = Iterators.product(w.x+1:w.y, w.y+1:X(fr)+1)
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_AiorOi) = Iterators.product(1:w.x-1,   w.x:w.y-1)

_accessibles(fr::Full1DFrame, w::Interval, ::_IA_DorBorE) = Iterators.flatten((_accessibles(fr, w, IA_B), _accessibles(fr, w, IA_D), _accessibles(fr, w, IA_E)))
_accessibles(fr::Full1DFrame, w::Interval, ::_IA_DiorBiorEi) = Iterators.flatten((_accessibles(fr, w, IA_Bi), _accessibles(fr, w, IA_Di), _accessibles(fr, w, IA_Ei)))

_accessibles(fr::Full1DFrame, w::Interval, ::_IA_I) = Iterators.flatten((
    # Iterators.product(1:w.x-1, w.y+1:X(fr)+1),   # Di
    # Iterators.product(w.x:w.y, w.y+1:X(fr)+1),   # A+O+Bi
    Iterators.product(1:w.y, w.y+1:X(fr)+1),       # Di+A+O+Bi
    Iterators.product(1:w.x-1, w.x:w.y),       # Ai+Oi+Ei
    zip(Iterators.repeated(w.x), w.x+1:w.y-1), # B
    zip(w.x+1:w.y-1, Iterators.repeated(w.y)), # E
    _intervals_in(w.x+1, w.y-1),               # D
))


# More efficient implementations for edge cases (Later)
accessibles(fr::Full1DFrame, S::AbstractWorldSet{Interval}, ::_IA_L) =
    accessibles(fr, nth(S, argmin(map((w)->w.y, S))), IA_L)
accessibles(fr::Full1DFrame, S::AbstractWorldSet{Interval}, ::_IA_Li) =
    accessibles(fr, nth(S, argmax(map((w)->w.x, S))), IA_Li)

# More efficient implementations for edge cases (After)
accessibles(fr::Full1DFrame, S::AbstractWorldSet{Interval}, ::_IA_A) =
    IterTools.imap(Interval,
        Iterators.flatten(
            IterTools.imap((y)->zip(Iterators.repeated(y), y+1:X(fr)+1),
                IterTools.distinct(map((w)->w.y, S))
            )
        )
    )
accessibles(fr::Full1DFrame, S::AbstractWorldSet{Interval}, ::_IA_Ai) =
    IterTools.imap(Interval,
        Iterators.flatten(
            IterTools.imap((x)->zip(1:x-1, Iterators.repeated(x)),
                IterTools.distinct(map((w)->w.x, S))
            )
        )
    )
