"""
    function precedeq(
        l::L,
        t1::T1,
        t2::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        L<:FiniteAlgebra{T,D},
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` ≤ `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`succeedes`](@ref), [`succeedeq`](@ref).
"""
function precedeq(
    l::L,
    t1::T1,
    t2::T2
) where {
    T<:Truth,
    D<:AbstractVector{T},
    L<:FiniteAlgebra{T,D},
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, T) t1 = convert(T, t1)::T end
    if !isa(t2, T) t2 = convert(T, t2)::T end
    !islattice(l) && error("Cannot convert object of type $(typeof(l)) to an object of " *
        "type FiniteLattice.")
    if l.meet(t1, t2) == t1
        return true
    else
        return false
    end
end

"""
    function precedes(
        l::L,
        t1::T1,
        t2::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        L<:FiniteAlgebra{T,D},
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` < `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedeq`](@ref), [`succeedes`](@ref), [`succeedeq`](@ref).
"""
function precedes(
    l::L,
    t1::T1,
    t2::T2
) where {
    T<:Truth,
    D<:AbstractVector{T},
    L<:FiniteAlgebra{T,D},
    T1<:Truth,
    T2<:Truth
}
    return t1 != t2 && precedeq(l, t1, t2)
end

"""
    function succeedeq(
        l::L,
        t1::T1,
        t2::T2
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        L<:FiniteAlgebra{T,D},
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` ≥ `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`precedeq`](@ref), [`succeedes`](@ref).
"""
function succeedeq(
    l::L,
    t1::T1,
    t2::T2
) where {
    T<:Truth,
    D<:AbstractVector{T},
    L<:FiniteAlgebra{T,D},
    T1<:Truth,
    T2<:Truth
}
    return precedeq(l, t2, t1)
end

"""
    function succeedeq(
        l::L,
        t1::T,
        t2::T
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        L<:FiniteAlgebra{T,D}
    }

Return true if `t1` > `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`precedeq`](@ref), [`succeedeq`](@ref).
"""
function succeedes(
    l::L,
    t1::T1,
    t2::T2
) where {
    T<:Truth,
    D<:AbstractVector{T},
    L<:FiniteAlgebra{T,D},
    T1<:Truth,
    T2<:Truth
}
    return precedes(l, t2, t1)
end

"""
    function lesservalues(
        l::L,
        t::T1
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        L<:FiniteAlgebra{T,D},
        T1<:Truth
    }
        if !isa(t, T) t = convert(T, t)::T end
        return filter(ti->precedes(l, ti, t), getdomain(l))
    end

Return all members of l below (or equal to) t.

See also [`precedes`](@ref), [`precedeq`](@ref).
"""
function lesservalues(
    l::L,
    t::T1
) where {
    T<:Truth,
    D<:AbstractVector{T},
    L<:FiniteAlgebra{T,D},
    T1<:Truth
}
    if !isa(t, T) t = convert(T, t)::T end
    return filter(ti->precedeq(l, ti, t), getdomain(l))
end

"""
    function maximalmembers(
        l::L,
        t::T1
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        L<:FiniteAlgebra{T,D},
        T1<:Truth
    }

Return all maximal members of l not above (or equal to) t.

See also [`succeedes`](@ref), [`succeedeq`](@ref), [`minimalmembers`](@ref).
"""
function maximalmembers(
    l::L,
    t::T1
) where {
    T<:Truth,
    D<:AbstractVector{T},
    L<:FiniteAlgebra{T,D},
    T1<:Truth
}
    if !isa(t, T) t = convert(T, t)::T end
    candidates = filter(ti->!succeedeq(l, ti, t), getdomain(l))
    mm = D()
    for c in candidates
        if isempty(filter(ti->succeedes(l, ti, c), candidates)) push!(mm, c) end
    end
    return mm
end

"""
    function minimalmembers(
        l::L,
        t::T1
    ) where {
        T<:Truth,
        D<:AbstractVector{T},
        L<:FiniteAlgebra{T,D},
        T1<:Truth
    }

Return all minimal members of l not below (or equal to) t.

See also [`precedes`](@ref), [`precedeq`](@ref), [`maximalmembers`](@ref).
"""
function minimalmembers(
    l::L,
    t::T1
) where {
    T<:Truth,
    D<:AbstractVector{T},
    L<:FiniteAlgebra{T,D},
    T1<:Truth
}
    if !isa(t, T) t = convert(T, t)::T end
    candidates = filter(ti->!precedeq(l, ti, t), getdomain(l))
    mm = D()
    for c in candidates
        if isempty(filter(ti->precedes(l, ti, c), candidates)) push!(mm, c) end
    end
    return mm
end
