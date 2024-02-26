using ..SoleLogics: AbstractAlgebra
import ..SoleLogics: syntaxstring

struct FLewTruth <: Truth
    label::String
end

syntaxstring(t::FLewTruth; kwargs...) = t.label

struct FLewAlgebra <: AbstractAlgebra{FLewTruth}
    elements::Set{FLewTruth}
    jointable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
    meettable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
    monoidtable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}

    function FLewAlgebra(
        elements::Set{FLewTruth},
        jointable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth},
        meettable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth},
        monoidtable::Dict{Tuple{FLewTruth, FLewTruth}, FLewTruth}
    )
        for i ∈ elements
            for j ∈ elements
                @assert (i,j) in keys(jointable) "jointable($i, $j) not defined."
                @assert (i,j) in keys(meettable) "meettable($i, $j) not defined."
                @assert (i,j) in keys(monoidtable) "monoidtable($i, $j) not defined."
                @assert jointable(i,j) == jointable(j,i) "jointable is not commutative: jointable($i,$j) ≠ jointable($j,$i)"
                @assert meettable(i,j) == meettable(j,i) "meettable is not commutative: meettable($i,$j) ≠ meettable($j,$i)"
                @assert monoidtable(i,j) == monoidtable(j,i) "monoidtable is not commutative: monoidtable($i,$j) ≠ monoidtable($j,$i)"
                for k ∈ elements
                    @assert jointable(jointable(i,j), k) == jointable(i, jointable(j, k)) "jointable is not transitive: jointable(jointable(i,j),k) ≠ jointable(i,jointable(j,k))"
                    @assert meettable(meettable(i,j), k) == meettable(i, meettable(j, k)) "meettable is not transitive: meettable(meettable(i,j),k) ≠ meettable(i,meettable(j,k))"
                    @assert monoidtable(monoidtable(i,j), k) == monoidtable(i, monoidtable(j, k)) "monoidtable is not transitive: monoidtable(monoidtable(i,j),k) ≠ monoidtable(i,monoidtable(j,k))"
                end
            end
        end
        @assert length(jointable) == length(elements)^2 "Found jointable(i,j) where i or j ∉ elements."
        @assert length(meettable) == length(elements)^2 "Found meettable(i,j) where i or j ∉ elements."
        @assert length(monoidtable) == length(elements)^2 "Found monoidtable(i,j) where i or j ∉ elements."
        return new(elements, meettable, jointable, monoidtable)
    end
end

join(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = get(a.jointable, (t1,t2))
meet(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = get(a.meettable, (t1,t2))
monoid(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = get(a.monoidtable, (t1,t2))
