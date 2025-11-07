import ..SoleLogics: AbstractAlgebra, top, bot, iscrisp

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
    println(string(a.tnorm.func))
end

iscrisp(::FuzzyLogic) = false

top(::FuzzyLogic) = ContinuousTruth(1.0)
bot(::FuzzyLogic) = ContinuousTruth(0.0)

"""
    const GodelLogic

Gödel fuzzy logic based on the minimum t-norm: `a ∧ b = min(a, b)`.

See also [`FuzzyLogic`](@ref), [`GodelTNorm`](@ref), [`LukasiewiczLogic`](@ref).
"""
const GodelLogic = FuzzyLogic(GodelTNorm)

"""
    const LukasiewiczLogic

Łukasiewicz fuzzy logic based on the Łukasiewicz t-norm: `a ∧ b = max(0, a + b - 1)`.

See also [`FuzzyLogic`](@ref), [`LukasiewiczTNorm`](@ref), [`ProductLogic`](@ref).
"""
const LukasiewiczLogic = FuzzyLogic(LukasiewiczTNorm)

"""
    const ProductLogic

Product fuzzy logic based on multiplication: `a ∧ b = a × b`.

See also [`FuzzyLogic`](@ref), [`ProductTNorm`](@ref), [`GodelLogic`](@ref).
"""
const ProductLogic = FuzzyLogic(ProductTNorm)



