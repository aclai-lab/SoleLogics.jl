import ..SoleLogics: AbstractAlgebra, top, bot, iscrisp
using StaticArrays

"""
    struct ManyExpertAlgebra <: AbstractAlgebra{ContinuousTruth}
        experts::A    
    end
A Many Expert Algebra is a combination of N continuous fuzzy logics, each potentially
based on a different t-norm. This algebraic structure allows modeling scenarios where
multiple "experts" evaluate truth values independently, each using their own fuzzy logic.

The algebra operates on tuples of [`ContinuousTruth`](@ref) values, where each component
corresponds to one expert's evaluation.
"""
struct ManyExpertAlgebra <: AbstractAlgebra{ContinuousTruth}
    experts::Vector{FuzzyLogic}

    function ManyExpertAlgebra(experts::FuzzyLogic...)
        return new(FuzzyLogic[experts...])
    end
end

function Base.show(io::IO, a::ManyExpertAlgebra)
    n = length(a.experts)
    print(io, "ManyExpertAlgebra with ", n, " expert", n == 1 ? "" : "s", ":")
    for (i, expert) in enumerate(a.experts)
        print(io, "\n  [", i, "] ")
        show(io, expert)
    end
end

function addexperts!(a::ManyExpertAlgebra, experts::FuzzyLogic...)
    append!(a.experts, FuzzyLogic[experts...])
end

iscrisp(::ManyExpertAlgebra) = false

top(a::ManyExpertAlgebra) = ntuple(_ -> ContinuousTruth(1.0), length(a.experts))
bot(a::ManyExpertAlgebra) = ntuple(_ -> ContinuousTruth(0.0), length(a.experts))