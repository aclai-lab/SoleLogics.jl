using ..SoleLogics: AbstractAlgebra
import ..SoleLogics: syntaxstring

struct FLewTruth <: Truth
    label::String

    function FLewTruth(label::String)
        return new(label)
    end

    function FLewTruth(t::BooleanTruth)
        if istop(t)
            return FLewTruth("⊤")
        else
            return FLewTruth("⊥")
        end
    end
end

syntaxstring(t::FLewTruth; kwargs...) = t.label
convert(::Type{FLewTruth}, t::BooleanTruth) = FLewTruth(t)

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
        @assert FLewTruth(⊥) ∈ elements "⊥ not found in elements."
        @assert FLewTruth(⊤) ∈ elements "⊤ not found in elements."
        for i ∈ elements
            for j ∈ elements
                @assert (i, j) ∈ keys(jointable) "jointable[($i, $j)] is not defined."
                @assert (i, j) ∈ keys(meettable) "meettable[($i, $j)] is not defined."
                @assert (i, j) ∈ keys(monoidtable) "monoidtable[($i, $j)] is not defined."
            end
        end
        @assert length(jointable) == length(elements)^2 "Found jointable[(i, j)] where i or j ∉ elements."
        @assert length(meettable) == length(elements)^2 "Found meettable[(i, j)] where i or j ∉ elements."
        @assert length(monoidtable) == length(elements)^2 "Found monoidtable[(i, j)] where i or j ∉ elements."
        for i ∈ elements
            @assert jointable[(i, FLewTruth(⊥))] == i "Defined join don't satisfy the neutral element rule: jointable[($i, ⊥) ≠ $i]"
            @assert meettable[(i, FLewTruth(⊤))] == i "Defined meet don't satisfy the neutral element rule: meettable[($i, ⊤) ≠ $i]"
            @assert monoidtable[(i, FLewTruth(⊤))] == i "Defined monoid don't satisfy the neutral element rule: monoidtable[($i, ⊤) ≠ $i]"
            for j ∈ elements
                @assert jointable[(i, j)] == jointable[(j, i)] "Defined join is not commutative: jointable[($i,$j)] ≠ jointable[($j,$i)]"
                @assert meettable[(i, j)] == meettable[(j, i)] "Defined meet is not commutative: meettable[($i,$j)] ≠ meettable[($j,$i)]"
                @assert monoidtable[(i, j)] == monoidtable[(j, i)] "Defined monoid is not commutative: monoidtable[($i,$j)] ≠ monoidtable[($j,$i)]"
                for k ∈ elements
                    @assert jointable[(jointable[(i, j)], k)] == jointable[(i, jointable[(j, k)])] "Defined join is not associative: jointable[(jointable[(i, j)], k)] ≠ jointable[(i, jointable[(j, k)])]"
                    @assert meettable[(meettable[(i, j)], k)] == meettable[(i, meettable[(j, k)])] "Defined meet is not associative: meettable[(meettable[(i, j)], k)] ≠ meettable[(i, meettable[(j, k)])]"
                    @assert monoidtable[(monoidtable[(i,j)], k)] == monoidtable[(i, monoidtable[(j, k)])] "Defined monoid is not associative: monoidtable[(monoidtable[(i,j)], k)] ≠ monoidtable[(i, monoidtable[(j, k)])]"
                end
                @assert jointable[(i, meettable[(i, j)])] == i "Defined join and meet don't satisfy the asborption law: jointable[($i, meettable[($i, $j)])] ≠ $i"
                @assert meettable[(i, jointable[(i, j)])] == i "Defined join and meet don't satisfy the asborption law: meettable[($i, jointable[($i, $j)])] ≠ $i"
            end
        end
        return new(elements, meettable, jointable, monoidtable)
    end
end

join(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = a.jointable[(t1, t2)]
meet(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = a.meettable[(t1, t2)]
monoid(a::FLewAlgebra, t1::FLewTruth, t2::FLewTruth) = a.monoidtable[(t1, t2)]
