
"""
    struct FilteredFrame{F<:AbstractMultiModalFrame,WF<:WorldFilter} <: AbstractMultiModalFrame
        innerframe::F
        wf::WF
    end

A (binary) accessibility relation `r`, filtered by a world filter `wf`.
"""
struct FilteredFrame{F<:AbstractMultiModalFrame,WF<:WorldFilter} <: AbstractMultiModalFrame
    r::F
    wf::WF

    function FilteredFrame{F,WF}(r::F, wf::F) where {F<:AbstractMultiModalFrame,WF<:WorldFilter}
        return new(r, wf)
    end

    function FilteredFrame(r::F, wf::F) where {F<:AbstractMultiModalFrame,WF<:WorldFilter}
        return FilteredFrame{F,WF}(r, wf)
    end

    # TODO constructor that accepts a Callable and wraps it into a FunctionalWorldFilter?
end

innerframe(r::FilteredFrame) = r.innerframe
worldfilter(r::FilteredFrame) = r.wf

function accessibles(fr::FilteredFrame, w::W, args...) where {W <: AbstractWorld}
    return filterworlds(worldfilter(fr), IterTools.imap(W, _accessibles(innerframe(fr), w, args...)))
end
