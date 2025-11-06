import ..SoleLogics: AbstractAlgebra, top, bot, iscrisp
using StaticArrays

"""
    struct FuzzyLogic <: AbstractAlgebra{ContinuousTruth}
        tnorm::ContinuousBinaryOperation
    end
A fuzzy logic is a type of many-valued logic in which the truth
value of variables can be any real value in the range [0,1]. Its only field 
is the t-norm the logic is based upon.
"""
struct FuzzyLogic <: AbstractAlgebra{ContinuousTruth}
    tnorm::ContinuousBinaryOperation
end

function Base.show(io::IO, a::FuzzyLogic)
    println(string(typeof(a)))
    println(string(tnorm))
end

iscrisp(::FuzzyLogic) = false

top(::FuzzyLogic) = ContinuousTruth(1.0)
bot(::FuzzyLogic) = ContinuousTruth(0.0)

# Shortcuts for most used fuzzy logics
const GodelLogic = FuzzyLogic(GodelTNorm)

const LukasiewiczLogic = FuzzyLogic(LukasiewiczTNorm)

const ProductLogic = FuzzyLogic(ProductTNorm)

"""
    struct ManyExpertAlgebra{N, A <: SArray{N, FuzzyLogic}} <: AbstractAlgebra{ContinuousTruth}
        fuzzylogics::A    
    end
TODO: write a proper doc
"""
struct ManyExpertAlgebra{N, A <: SArray{N, FuzzyLogic}} <: AbstractAlgebra{ContinuousTruth}
    fuzzylogics::A

    function ManyExpertAlgebra{N}(fuzzylogics::A) where {N, A <: SArray{N, FuzzyLogic}}
        return new{N, A}(fuzzylogics)
    end
end

iscrisp(::ManyExpertAlgebra) = false

top(::ManyExpertAlgebra) = ContinuousTruth(1.0)
bot(::ManyExpertAlgebra) = ContinuousTruth(0.0)

