# Scrathpad to sketch SoleLogics code

#### Letter ################################################################################

abstract type Letter{A} end
ariety(::Letter) = error("Ariety not defined")

# This letter simply represent a generic fact, no real data is taken into account
struct AtomicLetter{A} <: Letter{A} end
AtomicLetter(a::AbstractString) = AtomicLetter{Symbol(a)}()
AtomicLetter(a::Symbol) = AtomicLetter{a}()

# When real data is involved (see SoleData, SoleModels etc...)
# then a letter is said to be a DimensionalLetter (a1 ⋈ f(A) ⋈ a2).
struct DimensionalLetter{A} <: Letter{A}
    # Here the following knowledge is required:
    # - what a threshold is
    # - what an operator is
    # - what a feature is
end

#### Alphabet ##############################################################################

abstract type AbstractAlphabet{A} <: AbstractVector{Letter{A}} end
is_complete(::AbstractAlphabet{A}) where {A} = error("Property not defined")

# Methods to implement for each concrete alphabet
# https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-array

struct ExplicitAlphabet{A} <: AbstractAlphabet{A}
    alphabet::Vector{<:A} # TODO: choose correct type here
end
size(EA::ExplicitAlphabet{A}) where {A} = size(EA.alphabet)

is_complete(::ExplicitAlphabet) = true

# This alphabet leverages generators to dynamically manipulate letters
struct InfiniteAlphabet{A} <: AbstractAlphabet{A}
    # ...
end
is_complete(::InfiniteAlphabet) = false

# This alphabet is not complete and its dispatches requires more arguments to work
struct NonCompleteDimensionalAlphabet{A} <: AbstractAlphabet{A}
    #
end
is_complete(::NonCompleteDimensionalAlphabet) = false

#### Operators #############################################################################
abstract type AbstractOperator end
ariety(::AbstractOperator) = error("Ariety not defined")
is_constant(op::AbstractOperator) = ariety(op) == 0
is_unary(op::AbstractOperator) = ariety(op) == 1
is_binary(op::AbstractOperator) = ariety(op) == 2

struct NamedOperator{T} <: AbstractOperator end

top = NamedOperator{:⊤}
ariety(top) = 0

bottom = NamedOperator{:⊥}
ariety(bottom) = 0

conjunction = NamedOperator{:∧}
ariety(conjunction) = 2

disjunction = NamedOperator{:∨}

ariety(disjunction) = 2

implication = NamedOperator{:⟹}
ariety(implication) = 2

BaseOperators = Union{top, bottom, conjunction, disjunction, implication}
