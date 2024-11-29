# It is convenient to define methods for `accessibles` that take a world set instead of a
#  single world. Generally, this falls back to calling `_accessibles` on each world in
#  the set, and returning a constructor of wolds from the union; however, one may provide
#  improved implementations for special cases (e.g. ⟨L⟩ of a world set in interval algebra).
function accessibles(
    fr::AbstractMultiModalFrame{W},
    S::AbstractWorlds,
    r::AbstractRelation,
) where {W<:AbstractWorld}
    IterTools.imap(W,
        IterTools.distinct(
            Iterators.flatten(
                (_accessibles(fr, w, r) for w in S)
            )
        )
    )
end

############################################################################################

"""
    emptyworld(fr::AbstractMultiModalFrame)

Return an empty world (e.g., `Interval(-1,0)`).

See also [`AbstractMultiModalFrame`](@ref).
"""
function emptyworld(fr::AbstractMultiModalFrame)
    return error("Please, provide method emptyworld(::$(typeof(fr))).")
end

"""
    centralworld(fr::AbstractMultiModalFrame)

Return the world at the *center* of the frame (whenever there exists a definition of
"center" that makes sense).

See also [`AbstractDimensionalFrame`](@ref), [`AbstractMultiModalFrame`](@ref).
"""
function centralworld(fr::AbstractMultiModalFrame)
    return error("Please, provide method centralworld(::$(typeof(fr))).")
end

############################################################################################

include("dimensional-frame.jl")
