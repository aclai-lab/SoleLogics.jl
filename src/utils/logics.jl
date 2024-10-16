# implementations.jl

import SoleBase: initrng

############################################################################################
#### AbstractAlphabet Implementations ######################################################
############################################################################################

Base.eltype(::Type{<:AbstractAlphabet{V}}) where {V} = Atom{V}
atomstype(V::Type{<:AbstractAlphabet}) = eltype(V)
atomstype(a::AbstractAlphabet) = atomstype(typeof(a))
valuetype(a::Type{<:AbstractAlphabet}) = valuetype(atomstype(a))
valuetype(a::AbstractAlphabet) = valuetype(atomstype(a))

(::AbstractAlphabet{V})(a) where {V} = Atom{V}(a)

Base.isfinite(::Type{<:AbstractAlphabet}) = true
Base.isfinite(a::AbstractAlphabet) = Base.isfinite(typeof(a))

function randatom(a::AbstractAlphabet, args...; kwargs...)
    randatom(Random.GLOBAL_RNG, a, args...; kwargs...)
end

function randatom(rng::Union{Random.AbstractRNG, Integer}, a::AbstractAlphabet, args...; kwargs...)
    if isfinite(a)
        return Base.rand(rng, atoms(a), args...; kwargs...)
    else
        error("Please provide method randatom(rng::$(typeof(rng)), " *
            "alphabet::$(typeof(a)), args...; kwargs...)")
    end
end

function Base.length(a::AbstractAlphabet)
    @warn "Please use `natoms` instead of `Base.length` with alphabets."
    return natoms(a)
end

function Base.iterate(a::AbstractAlphabet)
    if isfinite(a)
        return Base.iterate(atoms(a))
    else
        return error("Cannot iterate (infinite) alphabet of type $(typeof(a)).")
    end
end

function Base.iterate(a::AbstractAlphabet, state)
    if isfinite(a)
        return Base.iterate(atoms(a), state)
    else
        return error("Cannot iterate (infinite) alphabet of type $(typeof(a)).")
    end
end

function Base.IteratorSize(::Type{V}) where {V<:AbstractAlphabet}
    return Base.isfinite(V) ? Base.HasLength() : Base.IsInfinite()
end

############################################################################################
#### ExplicitAlphabet ######################################################################
############################################################################################

struct ExplicitAlphabet{V} <: AbstractAlphabet{V}
    atoms::Vector{Atom{V}}

    function ExplicitAlphabet{V}(atoms) where {V}
        return new{V}(collect(atoms))
    end

    function ExplicitAlphabet(atoms::AbstractVector{Atom{V}}) where {V}
        return ExplicitAlphabet{V}(collect(atoms))
    end

    function ExplicitAlphabet(atoms::AbstractVector{V}) where {V}
        return ExplicitAlphabet{V}(Atom.(collect(atoms)))
    end
end

atoms(a::ExplicitAlphabet) = a.atoms
natoms(a::ExplicitAlphabet) = length(atoms(a))

Base.convert(::Type{AbstractAlphabet}, alphabet::Vector{<:Atom}) =
    ExplicitAlphabet(alphabet)

############################################################################################
#### AlphabetOfAny #########################################################################
############################################################################################

struct AlphabetOfAny{V} <: AbstractAlphabet{V} end
Base.isfinite(::Type{<:AlphabetOfAny}) = false
Base.in(::Atom{PV}, ::AlphabetOfAny{VV}) where {PV,VV} = (PV <: VV)

############################################################################################
#### UnionAlphabet #########################################################################
############################################################################################

struct UnionAlphabet{C,A<:AbstractAlphabet{C}} <: AbstractAlphabet{C}
    subalphabets::Vector{A}
end

subalphabets(a::UnionAlphabet) = a.subalphabets
nsubalphabets(a::UnionAlphabet) = length(subalphabets(a))

function Base.show(io::IO, a::UnionAlphabet)
    println(io, "$(typeof(a)):")
    for sa in subalphabets(a)
        Base.show(io, sa)
    end
end

function atoms(a::UnionAlphabet)
    return Iterators.flatten(Iterators.map(atoms, subalphabets(a)))
end

natoms(a::UnionAlphabet) = sum(natoms, subalphabets(a))

function Base.in(p::Atom, a::UnionAlphabet)
    return any(sa -> Base.in(p, sa), subalphabets(a))
end

function randatom(
    rng::Union{Integer,AbstractRNG},
    a::UnionAlphabet;
    atompicking_mode::Symbol=:uniform,
    subalphabets_weights::Union{AbstractWeights,AbstractVector{<:Real},Nothing} = nothing
)::Atom
    # Implementation
end

############################################################################################
#### AbstractGrammar Implementations #######################################################
############################################################################################

operatorstype(::AbstractGrammar{V,O}) where {V,O} = O
alphabettype(::AbstractGrammar{V,O}) where {V,O} = V

atomstype(g::AbstractGrammar) = eltype(alphabet(g))
tokenstype(g::AbstractGrammar) = Union{operatorstype(g),atomstype(g)}

function Base.in(φ::Formula, g::AbstractGrammar)::Bool
    return Base.in(tree(φ), g)
end

Base.in(a::Atom, g::AbstractGrammar) = Base.in(a, alphabet(g))
Base.in(op::Truth, g::AbstractGrammar) = (op <: operatorstype(g))
Base.in(op::Connective, g::AbstractGrammar) = (op <: operatorstype(g))

function Base.isequal(a::AbstractGrammar, b::AbstractGrammar)
    Base.isequal(alphabet(a), alphabet(b)) &&
    Base.isequal(operatorstype(a), operatorstype(b))
end
Base.hash(a::AbstractGrammar) = Base.hash(alphabet(a), Base.hash(operatorstype(a)))

############################################################################################
#### CompleteFlatGrammar ###################################################################
############################################################################################

struct CompleteFlatGrammar{V<:AbstractAlphabet,O<:Operator} <: AbstractGrammar{V,O}
    alphabet::V
    operators::Vector{<:O}

    function CompleteFlatGrammar{V,O}(
        alphabet::V,
        operators::Vector{<:O},
    ) where {V<:AbstractAlphabet,O<:Operator}
        return new{V,O}(alphabet, operators)
    end

    function CompleteFlatGrammar{V}(
        alphabet::V,
        operators::Vector{<:Operator},
    ) where {V<:AbstractAlphabet}
        return new{V,Union{typeof.(operators)...}}(
            alphabet,
            Vector{Union{typeof.(operators)...}}(operators)
        )
    end

    function CompleteFlatGrammar(
        alphabet::V,
        operators::Vector{<:Operator},
    ) where {V<:AbstractAlphabet}
        return new{V,Union{typeof.(operators)...}}(
            alphabet,
            Vector{Union{typeof.(operators)...}}(operators)
        )
    end
end

alphabet(g::CompleteFlatGrammar) = g.alphabet
operators(g::CompleteFlatGrammar) = g.operators

function connectives(g::AbstractGrammar)::AbstractVector{Connective}
    return filter(!isnullary, operators(g))
end

function leaves(g::AbstractGrammar)
    return [atoms(alphabet(g))..., filter(isnullary, operators(g))...]
end

function Base.in(φ::SyntaxTree, g::CompleteFlatGrammar)::Bool
    # Implementation
end

function formulas(
    g::CompleteFlatGrammar{V,O} where {V,O};
    maxdepth::Integer,
    nformulas::Union{Nothing,Integer} = nothing,
)::Vector{SyntaxTree}
    # Implementation
end

Base.in(p::Atom, g::CompleteFlatGrammar) = Base.in(p, alphabet(g))
Base.in(op::Truth, g::CompleteFlatGrammar) = (op <: operatorstype(g))

############################################################################################
#### AbstractAlgebra Implementations #######################################################
############################################################################################

truthtype(::Type{<:AbstractAlgebra{T}}) where {T<:Truth} = T
truthtype(a::AbstractAlgebra) = truthtype(typeof(a))

iscrisp(a::AbstractAlgebra) = (length(domain(a)) == 2)

############################################################################################
#### AbstractLogic Implementations #########################################################
############################################################################################

operatorstype(l::AbstractLogic) = operatorstype(grammar(l))
alphabettype(l::AbstractLogic) = alphabettype(grammar(l))
operators(l::AbstractLogic) = operators(grammar(l))
alphabet(l::AbstractLogic) = alphabet(grammar(l))
atomstype(l::AbstractLogic) = atomstype(alphabet(l))
tokenstype(l::AbstractLogic) = tokenstype(grammar(l))
formulas(l::AbstractLogic, args...; kwargs...) = formulas(grammar(l), args...; kwargs...)

Base.in(op::Operator, l::AbstractLogic) = Base.in(op, grammar(l))
Base.in(φ::SyntaxBranch, l::AbstractLogic) = Base.in(φ, grammar(l))
Base.in(p::Atom, l::AbstractLogic) = Base.in(p, alphabet(l))

truthtype(l::AbstractLogic) = truthtype(algebra(l))
top(l::AbstractLogic) = top(algebra(l))
bot(l::AbstractLogic) = bot(algebra(l))
iscrisp(l::AbstractLogic) = iscrisp(algebra(l))

function Base.isequal(a::AbstractLogic, b::AbstractLogic)
    Base.isequal(grammar(a), grammar(b)) &&
    Base.isequal(algebra(a), algebra(b))
end
Base.hash(a::AbstractLogic) = Base.hash(grammar(a), Base.hash(algebra(a)))