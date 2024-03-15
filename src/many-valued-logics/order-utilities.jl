"""
    function precedeq(
        l::L,
        t1::T,
        t2::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T},
        L<:FiniteAlgebra{T,D}
    }

Return true if `t1` ≤ `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`succeedes`](@ref), [`succeedeq`](@ref).
"""
function precedeq(
    l::L,
    t1::T,
    t2::T
) where {
    T<:Truth,
    D<:AbstractSet{T},
    L<:FiniteAlgebra{T,D}
}
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
        t1::T,
        t2::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T},
        L<:FiniteAlgebra{T,D}
    }

Return true if `t1` < `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedeq`](@ref), [`succeedes`](@ref), [`succeedeq`](@ref).
"""
function precedes(
    l::L,
    t1::T,
    t2::T
) where {
    T<:Truth,
    D<:AbstractSet{T},
    L<:FiniteAlgebra{T,D}
}
    t1 != t2 && precedeq(l, t1, t2)
end

"""
    function succeedeq(
        l::L,
        t1::T,
        t2::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T},
        L<:FiniteAlgebra{T,D}
    }

Return true if `t1` ≥ `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`precedeq`](@ref), [`succeedes`](@ref).
"""
function succeedeq(
    l::L,
    t1::T,
    t2::T
) where {
    T<:Truth,
    D<:AbstractSet{T},
    L<:FiniteAlgebra{T,D}
}
    precedeq(l, t2, t1)
end

"""
    function succeedeq(
        l::L,
        t1::T,
        t2::T
    ) where {
        T<:Truth,
        D<:AbstractSet{T},
        L<:FiniteAlgebra{T,D}
    }

Return true if `t1` > `t2` in `l`. Given an algebraically defined lattice (L, ∨, ∧), one can
define a partial order ≤ on L by setting a ≤ b if a = a ∧ b.

See also [`precedes`](@ref), [`precedeq`](@ref), [`succeedeq`](@ref).
"""
function succeedes(
    l::L,
    t1::T,
    t2::T
) where {
    T<:Truth,
    D<:AbstractSet{T},
    L<:FiniteAlgebra{T,D}
}
    precedes(l, t2, t1)
end
