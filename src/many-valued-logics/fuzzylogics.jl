import ..SoleLogics: AbstractAlgebra, top, bot, iscrisp
using StaticArrays

#TODO: Write docs
struct FuzzyLogic <: AbstractAlgebra{ContinuousTruth}
    tnorm::ContinuousBinaryOperation
end

iscrisp(::FuzzyLogic) = false

top(::FuzzyLogic) = ContinuousTruth(1.0)
bot(::FuzzyLogic) = ContinuousTruth(0.0)

# Shortcuts for most used fuzzy logics
const GodelLogic = FuzzyLogic(GodelTNorm)

const LukasiewiczLogic = FuzzyLogic(LukasiewiczTNorm)

const ProductLogic = FuzzyLogic(ProductTNorm)

#TODO: Write docs
struct ManyExpertAlgebra{N, A <: SArray{N, FuzzyLogic}} <: AbstractAlgebra{ContinuousTruth}
    fuzzylogics::A

    function ManyExpertAlgebra{N}(fuzzylogics::A) where {N, A <: SArray{N, FuzzyLogic}}
        return new{N, A}(fuzzylogics)
    end
end

iscrisp(::ManyExpertAlgebra) = false

top(::ManyExpertAlgebra) = ContinuousTruth(1.0)
bot(::ManyExpertAlgebra) = ContinuousTruth(0.0)

