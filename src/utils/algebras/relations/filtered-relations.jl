"""
    struct FilteredRelation{R<:AbstractRelation,F<:WorldFilter} <: AbstractRelation
        r::R
        wf::F
    end

A (binary) accessibility relation `r`, filtered by a world filter `wf`.
"""
struct FilteredRelation{R<:AbstractRelation,F<:WorldFilter} <: AbstractRelation
    r::R
    wf::F

    function FilteredRelation{R,F}(r::R, wf::F) where {R<:AbstractRelation,F<:WorldFilter}
        return new(r, wf)
    end

    function FilteredRelation(r::R, wf::F) where {R<:AbstractRelation,F<:WorldFilter}
        return FilteredRelation{R,F}(r, wf)
    end

    # TODO constructor that accepts a Callable and wraps it into a FunctionalWorldFilter?
end

wrappedrelation(r::FilteredRelation) = r.r
worldfilter(r::FilteredRelation) = r.wf

function accessibles(
    fr::AbstractMultiModalFrame,
    w::W,
    r::FilteredRelation
) where {W <: AbstractWorld}
    return filterworlds(worldfilter(r), IterTools.imap(W, _accessibles(fr, w, r.r)))
end

function accessibles(
    fr::AbstractMultiModalFrame,
    ::W,
    r::FilteredRelation{GlobalRel,<:WorldFilter{W}}
) where {W <: AbstractWorld}
	return filterworlds(worldfilter(r), accessibles(fr, r.r))
end
