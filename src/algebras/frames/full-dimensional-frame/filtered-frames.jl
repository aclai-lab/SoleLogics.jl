function _accessibles(
    fr::AbstractMultiModalFrame,
    w::AbstractWorld,
    r::FilteredRelation{R, F}
) where {
    R <: AbstractRelation,
    F <: FunctionalWorldFilter
}
    filter(r.f, _accessibles(fr, w, r.r))
end
