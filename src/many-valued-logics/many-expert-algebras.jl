import ..SoleLogics: AbstractAlgebra, top, bot, iscrisp
using StaticArrays

"""
    struct ManyExpertAlgebra{N, A <: SVector{N, FuzzyLogic}} <: AbstractAlgebra{ContinuousTruth}
        experts::A    
    end
A Many Expert Algebra is a combination of N continuous fuzzy logics, each potentially
based on a different t-norm. This algebraic structure allows modeling scenarios where
multiple "experts" evaluate truth values independently, each using their own fuzzy logic.

The algebra operates on tuples of [`ContinuousTruth`](@ref) values, where each component
corresponds to one expert's evaluation.
"""
struct ManyExpertAlgebra{N, A <: SVector{N, FuzzyLogic}} <: AbstractAlgebra{ContinuousTruth}
    experts::A

    function ManyExpertAlgebra{N}(experts::A) where {N, A <: SVector{N, FuzzyLogic}}
        return new{N, A}(experts)
    end

    function ManyExpertAlgebra{N}(experts::AbstractVector{FuzzyLogic}) where {N}
        return ManyExpertAlgebra{N}(SVector{N, FuzzyLogic}(experts))
    end
end

function Base.show(io::IO, a::ManyExpertAlgebra)
    println(typeof(a))
    for expert in a.experts print(expert) end
end

iscrisp(::ManyExpertAlgebra) = false

top(::ManyExpertAlgebra{N}) where {N} = ntuple(_ -> ContinuousTruth(1.0), N)
bot(::ManyExpertAlgebra{N}) where {N}= ntuple(_ -> ContinuousTruth(0.0), N)