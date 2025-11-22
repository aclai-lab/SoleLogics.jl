"""
    function precedeq(
        l::L,
        t1::T1,
        t2::T2
    ) where {
        N,
        L<:FiniteAlgebra{N},
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` ≤ `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`succeedes`](@ref), [`succeedeq`](@ref).
"""
@inline function precedeq(
    l::L,
    t1::T1,
    t2::T2
) where {
    N,
    L<:FiniteAlgebra{N},
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, FiniteTruth) t1 = convert(FiniteTruth, t1)::FiniteTruth end
    if !isa(t2, FiniteTruth) t2 = convert(FiniteTruth, t2)::FiniteTruth end
    !islattice(l) && error("Cannot convert object of type $(typeof(l)) to an object of " *
        "type FiniteLattice.")
    if l.meet(t1, t2) == t1
        return true
    else
        return false
    end
end

"""
    function precedeq(
        l::FuzzyLogic,
        t1::T1,
        t2::T2
    ) where {
        T1<:Truth,
        T2<:Truth
    }
Return true if `t1` < `t2` in fuzzy logic `l`. For continuous truth values, 
this is the standard strict less-than ordering on real numbers in [0,1].

See also [`precedes`](@ref), [`succeedes`](@ref), [`succeedeq`](@ref).
"""
@inline function precedeq(
    l::FuzzyLogic,
    t1::T1,
    t2::T2
) where {
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, ContinuousTruth) t1 = convert(ContinuousTruth, t1)::ContinuousTruth end
    if !isa(t2, ContinuousTruth) t2 = convert(ContinuousTruth, t2)::ContinuousTruth end
    return t1.value <= t2.value ? true : false
end

"""
    function precedes(
        l::L,
        t1::T1,
        t2::T2
    ) where {
        N,
        L<:FiniteAlgebra{N},
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` < `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedeq`](@ref), [`succeedes`](@ref), [`succeedeq`](@ref).
"""
@inline function precedes(
    l::L,
    t1::T1,
    t2::T2
) where {
    N,
    L<:FiniteAlgebra{N},
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, FiniteTruth) t1 = convert(FiniteTruth, t1)::FiniteTruth end
    if !isa(t2, FiniteTruth) t2 = convert(FiniteTruth, t2)::FiniteTruth end
    !islattice(l) && error("Cannot convert object of type $(typeof(l)) to an object of " *
        "type FiniteLattice.")
    return t1 != t2 && precedeq(l, t1, t2)
end

"""
    function precedes(
        l::FuzzyLogic,
        t1::T1,
        t2::T2
    ) where {
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` < `t2` in fuzzy logic `l`. For continuous truth values, 
this is the standard strict less-than ordering on real numbers in [0,1].

See also [`precedeq`](@ref), [`succeedes`](@ref), [`succeedeq`](@ref).
"""
@inline function precedes(
    l::FuzzyLogic,
    t1::T1,
    t2::T2
) where {
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, ContinuousTruth) t1 = convert(ContinuousTruth, t1)::ContinuousTruth end
    if !isa(t2, ContinuousTruth) t2 = convert(ContinuousTruth, t2)::ContinuousTruth end
    return t1.value < t2.value ? true : false
end

"""
    function succeedeq(
        l::L,
        t1::T1,
        t2::T2
    ) where {
        N,
        L<:FiniteAlgebra{N},
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` ≥ `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`precedeq`](@ref), [`succeedes`](@ref).
"""
@inline function succeedeq(
    l::L,
    t1::T1,
    t2::T2
) where {
    N,
    L<:FiniteAlgebra{N},
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, FiniteTruth) t1 = convert(FiniteTruth, t1)::FiniteTruth end
    if !isa(t2, FiniteTruth) t2 = convert(FiniteTruth, t2)::FiniteTruth end
    !islattice(l) && error("Cannot convert object of type $(typeof(l)) to an object of " *
        "type FiniteLattice.")
    return precedeq(l, t2, t1)
end

"""
    function succeedeq(
        l::FuzzyLogic,
        t1::T1,
        t2::T2
    ) where {
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` ≥ `t2` in fuzzy logic `l`. For continuous truth values, 
this is the standard greater-than-or-equal ordering on real numbers in [0,1].

See also [`precedes`](@ref), [`precedeq`](@ref), [`succeedes`](@ref).
"""
@inline function succeedeq(
    l::FuzzyLogic,
    t1::T1,
    t2::T2
) where {
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, ContinuousTruth) t1 = convert(ContinuousTruth, t1)::ContinuousTruth end
    if !isa(t2, ContinuousTruth) t2 = convert(ContinuousTruth, t2)::ContinuousTruth end
    return t1.value >= t2.value ? true : false
end

"""
    function succeedes(
        l::L,
        t1::T1,
        t2::T2
    ) where {
        N,
        L<:FiniteAlgebra{N},
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` > `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`precedeq`](@ref), [`succeedeq`](@ref).
"""
@inline function succeedes(
    l::L,
    t1::T1,
    t2::T2
) where {
    N,
    L<:FiniteAlgebra{N},
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, FiniteTruth) t1 = convert(FiniteTruth, t1)::FiniteTruth end
    if !isa(t2, FiniteTruth) t2 = convert(FiniteTruth, t2)::FiniteTruth end
    !islattice(l) && error("Cannot convert object of type $(typeof(l)) to an object of " *
        "type FiniteLattice.")
    return precedes(l, t2, t1)
end

"""
    function succeedes(
        l::FuzzyLogic,
        t1::T1,
        t2::T2
    ) where {
        T1<:Truth,
        T2<:Truth
    }

Return true if `t1` > `t2` in fuzzy logic `l`. For continuous truth values, 
this is the standard strict greater-than ordering on real numbers in [0,1].

See also [`precedes`](@ref), [`precedeq`](@ref), [`succeedeq`](@ref).
"""
@inline function succeedes(
    l::FuzzyLogic,
    t1::T1,
    t2::T2
) where {
    T1<:Truth,
    T2<:Truth
}
    if !isa(t1, ContinuousTruth) t1 = convert(ContinuousTruth, t1)::ContinuousTruth end
    if !isa(t2, ContinuousTruth) t2 = convert(ContinuousTruth, t2)::ContinuousTruth end
    return t1.value > t2.value ? true : false
end

"""
    function lesservalues(
        l::L,
        t::T1
    ) where {
        N,
        L<:FiniteAlgebra{N},
        T1<:Truth
    }

Return all members of l below (or equal to) t.

See also [`precedes`](@ref), [`precedeq`](@ref).
"""
@inline function lesservalues(
    l::L,
    t::T
) where {
    N,
    L<:FiniteAlgebra{N},
    T<:Truth
}
    if !isa(t, FiniteTruth) t = convert(FiniteTruth, t)::FiniteTruth end
    return filter(ti->precedes(l, ti, t), getdomain(l))
end

"""
    function maximalmembers(
        l::L,
        t::T
    ) where {
        N,
        L<:FiniteAlgebra{N},
        T<:Truth
    }

Return all maximal members of l not above (or equal to) t.

See also [`succeedes`](@ref), [`succeedeq`](@ref), [`minimalmembers`](@ref).
"""
@inline function maximalmembers(
    l::L,
    t::T
) where {
    N,
    L<:FiniteAlgebra{N},
    T<:Truth
}
    if !isa(t, FiniteTruth) t = convert(FiniteTruth, t)::FiniteTruth end
    candidates = filter(ti->!succeedeq(l, ti, t), getdomain(l))
    mm = Vector{FiniteTruth}()
    for c in candidates
        if isempty(filter(ti->succeedes(l, ti, c), candidates)) push!(mm, c) end
    end
    return mm
end

"""
    function minimalmembers(
        l::L,
        t::T
    ) where {
        N,
        L<:FiniteAlgebra{N},
        T<:Truth
    }

Return all minimal members of l not below (or equal to) t.

See also [`precedes`](@ref), [`precedeq`](@ref), [`maximalmembers`](@ref).
"""
@inline function minimalmembers(
    l::L,
    t::T
) where {
    N,
    L<:FiniteAlgebra{N},
    T<:Truth
}
    if !isa(t, FiniteTruth) t = convert(FiniteTruth, t)::FiniteTruth end
    candidates = filter(ti->!precedeq(l, ti, t), getdomain(l))
    mm = Vector{FiniteTruth}()
    for c in candidates
        if isempty(filter(ti->precedes(l, ti, c), candidates)) push!(mm, c) end
    end
    return mm
end
