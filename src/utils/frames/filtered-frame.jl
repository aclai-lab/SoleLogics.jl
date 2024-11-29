
"""
    struct FilteredFrame{W<:AbstractWorld,F<:AbstractMultiModalFrame{W},WF<:WorldFilter{W}} <: AbstractMultiModalFrame{W}
        innerframe::F
        wf::WF
    end

A (binary) accessibility relation `r`, filtered by a world filter `wf`.
"""
struct FilteredFrame{W<:AbstractWorld,F<:AbstractMultiModalFrame{W},WF<:WorldFilter{W}} <: AbstractMultiModalFrame{W}
    r::F
    wf::WF

    function FilteredFrame{W,F,WF}(r::F, wf::WF) where {W<:AbstractWorld,F<:AbstractMultiModalFrame{W},WF<:WorldFilter{W}}
        return new{W,F,WF}(r, wf)
    end

    function FilteredFrame(r::F, wf::WF) where {W<:AbstractWorld,F<:AbstractMultiModalFrame{W},WF<:WorldFilter{W}}
        return FilteredFrame{W,F,WF}(r, wf)
    end

    # TODO constructor that accepts a Callable and wraps it into a FunctionalWorldFilter?
end

innerframe(r::FilteredFrame) = r.innerframe
worldfilter(r::FilteredFrame) = r.wf

function accessibles(fr::FilteredFrame, w::W, args...) where {W<:AbstractWorld}
    return filterworlds(worldfilter(fr), IterTools.imap(W, _accessibles(innerframe(fr), w, args...)))
end
