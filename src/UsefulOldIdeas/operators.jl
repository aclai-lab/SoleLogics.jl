# Macro to collect all modaloperators (e.g @modaloperators HSRELATIONS 1)
"""
    modaloperators(R, d::Int)
Collect all the valid modal operators -both existential and universal- from a collection
of strings or symbols.

# Example
```jldoctest
julia> @modaloperators HSRELATIONS 1
⟨L⟩
⟨A⟩
⟨O⟩
⟨E⟩
⋮
[E̅]
[D̅]
[B̅]
julia> @modaloperators HS₃RELATIONS 2
⟨L,L⟩
⟨L̅,L⟩
⟨I,L⟩
⟨L,L̅⟩
⋮
[L,I]
[L̅,I]
[I,I]
```
"""
macro modaloperators(R, d::Int)
    quote
        rels = vec(collect(Iterators.product([$(R) for _ = 1:$(d)]...)))
        if "=" in $(R)
            rels = rels[1:end-1]
        end
        exrels = [EXMODOP(r) for r in rels]
        univrels = [UNIVMODOP(r) for r in rels]
        Operators(vcat(exrels, univrels))
    end
end
