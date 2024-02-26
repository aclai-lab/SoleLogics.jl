module ManyValuedLogics

using ..SoleLogics
using ..SoleLogics: AbstractAlgebra

export HeytingTruth, HeytingAlgebra
export precedes, succeedes, precedeq, succeedeq # TODO move above.
export @heytingtruths, @heytingalgebra

include("algebras/heyting-algebra.jl")

end
