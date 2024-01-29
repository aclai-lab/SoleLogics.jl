const BASE_PROPOSITIONAL_CONNECTIVES = BASE_CONNECTIVES
const BasePropositionalConnectives = Union{typeof.(BASE_PROPOSITIONAL_CONNECTIVES)...}

# A propositional logic based on the base propositional operators
const BasePropositionalLogic = AbstractLogic{G,A} where {
        ALP,
        G<:AbstractGrammar{ALP,<:BasePropositionalConnectives},
        A<:AbstractAlgebra
    }

"""
    propositionallogic(;
        alphabet = AlphabetOfAny{String}(),
        operators = $(BASE_PROPOSITIONAL_CONNECTIVES),
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), $(BASE_PROPOSITIONAL_CONNECTIVES)),
        algebra = BooleanAlgebra()
    )

Instantiate a [propositional logic](https://simple.wikipedia.org/wiki/Propositional_logic)
given a grammar and an algebra. Alternatively, an alphabet and a set of operators
can be specified instead of the grammar.

# Examples
```julia-repl
julia> (¬) isa operatorstype(propositionallogic())
true

julia> (¬) isa operatorstype(propositionallogic(; operators = [∨]))
false

julia> propositionallogic(; alphabet = ["p", "q"]);

julia> propositionallogic(; alphabet = ExplicitAlphabet([Atom("p"), Atom("q")]));

```

See also [`modallogic`](@ref), [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref).
"""
function propositionallogic(;
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:Operator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing
)
    _baselogic(
        alphabet = alphabet,
        operators = operators,
        grammar = grammar,
        algebra = algebra;
        default_operators = BASE_PROPOSITIONAL_CONNECTIVES,
        logictypename = "propositional logic",
    )
end

############################################################################################

"""
    abstract type AbstractAssignment <: AbstractInterpretation end

Abstract type for assigments, that is, interpretations of propositional logic,
encoding mappings from `Atom`s to `Truth` values.

See also [`AbstractInterpretation`](@ref).
"""
abstract type AbstractAssignment <: AbstractInterpretation end

"""
    Base.haskey(i::AbstractAssignment, ::Atom)::Bool

Return whether an assigment has a truth value for a given atom.

See also [`AbstractInterpretation`](@ref).
"""
function Base.haskey(i::AbstractAssignment, ::Atom)::Bool
    return error("Please, provide method Base.haskey(::$(typeof(i)), ::Atom)::Bool.")
end

# Helper
function Base.haskey(i::AbstractAssignment, v)::Bool
    Base.haskey(i, Atom(v))
end

function inlinedisplay(i::AbstractAssignment)
    return error("Please, provide method inlinedisplay(::$(typeof(i)))::String.")
end

# When interpreting a single atom, if the lookup fails, then return the atom itself
function interpret(a::Atom, i::AbstractAssignment, args...; kwargs...)::SyntaxLeaf
    if Base.haskey(i, a)
        Base.getindex(i, a, args...; kwargs...)
    else
        a
    end
end

function interpret(φ::SyntaxBranch, i::AbstractAssignment, args...; kwargs...)::Formula
    connective = token(φ)
    return simplify(connective, Tuple(
        [interpret(ch, i, args...; kwargs...) for ch in children(φ)]
    ))
end

############################################################################################
#### Implementations #######################################################################
############################################################################################

"""
    struct TruthDict{D<:AbstractDict{A where A<:Atom,T where T<:Truth}} <: AbstractAssignment
        truth::D
    end

A logical interpretation instantiated as a dictionary,
explicitly assigning truth values to a *finite* set of atoms.

# Examples
```julia-repl
julia> TruthDict(1:4)
TruthDict with values:
┌────────┬────────┬────────┬────────┐
│      4 │      2 │      3 │      1 │
│  Int64 │  Int64 │  Int64 │  Int64 │
├────────┼────────┼────────┼────────┤
│      ⊤ │      ⊤ │      ⊤ │      ⊤ │
└────────┴────────┴────────┴────────┘


julia> t1 = TruthDict(1:4, false); t1[5] = true; t1
TruthDict with values:
┌───────┬───────┬───────┬───────┬───────┐
│     5 │     4 │     2 │     3 │     1 │
│ Int64 │ Int64 │ Int64 │ Int64 │ Int64 │
├───────┼───────┼───────┼───────┼───────┤
│     ⊤ │     ⊥ │     ⊥ │     ⊥ │     ⊥ │
└───────┴───────┴───────┴───────┴───────┘

julia> t2 = TruthDict(["a" => true, "b" => false, "c" => true])
TruthDict with values:
┌────────┬────────┬────────┐
│      c │      b │      a │
│ String │ String │ String │
├────────┼────────┼────────┤
│      ⊤ │      ⊥ │      ⊤ │
└────────┴────────┴────────┘

julia> check(parseformula("a ∨ b"), t2)
true

```

!!! note
    If prompted for the value of an unknown atom, this throws an error.
    If boolean, integer, or float values are specified, they are converted to
    `Truth` values.
    If the structure is initialized as empty, `BooleanTruth` values are assumed.

See also
[`DefaultedTruthDict`](@ref),
[`AbstractAssignment`](@ref), [`AbstractInterpretation`](@ref).
"""
struct TruthDict{D<:AbstractDict} <: AbstractAssignment

    truth::D

    function TruthDict{D}(d::D) where {A<:Atom,T<:Truth,D<:AbstractDict{A,T}}
        return new{typeof(d)}(d)
    end
    function TruthDict(d::AbstractDict{A,T}) where {A<:Atom,T<:Truth}
        return TruthDict{typeof(d)}(d)
    end
    function TruthDict(d::AbstractDict{V,T}) where {V,T<:Truth}
        return TruthDict(Dict([(Atom{V}(a),v) for (a,v) in d]))
    end
    function TruthDict(d::AbstractDict)
        d = Dict([(a, convert(Truth, v)) for (a,v) in d])
        return TruthDict(d)
    end
    function TruthDict(v::AbstractVector, truth_value = ⊤)
        if length(v) == 0
            return TruthDict()
        else
            return TruthDict(Dict([k => truth_value for k in v]))
        end
    end
    function TruthDict(v::AbstractVector{<:Union{Tuple,Pair}})
        if length(v) == 0
            return TruthDict()
        else
            return TruthDict(Dict(v))
        end
    end
    function TruthDict(p::Pair)
        return TruthDict([p])
    end
    function TruthDict(t::Tuple)
        return TruthDict(Pair(t...))
    end

    # Empty dict
    function TruthDict{D}() where {A<:Atom,T<:Truth,D<:AbstractDict{A,T}}
        return TruthDict{D}(D())
    end
    function TruthDict()
        T = BooleanTruth
        d = Dict{Atom,T}([])
        return TruthDict{typeof(d)}(d)
    end
end

Base.haskey(i::TruthDict, a::Atom) = Base.haskey(i.truth, a)

function interpret(a::Atom, i::TruthDict, args...; kwargs...)
    return Base.haskey(i, a) ? Base.getindex(i.truth, a) : a
end

function inlinedisplay(i::TruthDict)
    "TruthDict([$(join(["$(syntaxstring(a)) => $t" for (a,t) in i.truth], ", "))])"
end

# Utility function to represent pretty tables horizontally
function _hpretty_table(io::IO, keys::Any, values::Any)
    # Prepare columns names
    _keys = map(x -> x isa Atom ? value(x) : x, collect(keys))
    header = (_keys, string.(nameof.(typeof.(_keys))))

    try
        # Try to draw a complete table
        data = hcat([x for x in values]...)
        pretty_table(io, data; header=header)
    catch e
        if e isa DimensionMismatch
            # If it is not possible to draw a complete table, throw a custom error.
            @error "Some syntax structures are not resolved with all the interpretations "
        else
            throw(e)
        end
    end
end

function Base.show(
    io::IO,
    i::TruthDict,
)
    if isempty(i.truth)
        print(io, "Empty TruthDict")
    else
        println(io, "TruthDict with values:")
        _hpretty_table(
            io,
            i.truth |> keys,
            i.truth |> values
        )
    end
end

# Helpers
@forward TruthDict.truth (
    Base.length, Base.setindex!, Base.iterate,
    Base.IteratorSize, Base.IteratorEltype,
    Base.firstindex, Base.lastindex,
    Base.keys, Base.values,
)

############################################################################################

"""
    struct DefaultedTruthDict{
        D<:AbstractDict{A where A<:Atom,T where T<:Truth},
        T<:Truth
    } <: AbstractAssignment
        truth::D
        default_truth::T
    end

A truth table instantiated as a dictionary, plus a default value.
This structure assigns truth values to a set of atoms and,
when prompted for the value of an atom that is not in the dictionary,
it returns `default_truth`.

# Examples
```julia-repl
julia> t1 = DefaultedTruthDict(string.(1:4), false); t1["5"] = false; t1
DefaultedTruthDict with default truth `⊥` and values:
┌────────┬────────┬────────┬────────┬────────┐
│      4 │      1 │      5 │      2 │      3 │
│ String │ String │ String │ String │ String │
├────────┼────────┼────────┼────────┼────────┤
│      ⊤ │      ⊤ │      ⊥ │      ⊤ │      ⊤ │
└────────┴────────┴────────┴────────┴────────┘

julia> check(parseformula("1 ∨ 2"), t1)
true

julia> check(parseformula("1 ∧ 5"), t1)
false

```

See also
[`TruthDict`](@ref),
[`AbstractAssignment`](@ref), [`AbstractInterpretation`](@ref).
"""
struct DefaultedTruthDict{
    D<:AbstractDict,
    T<:Truth
} <: AbstractAssignment

    truth::D

    default_truth::T

    function DefaultedTruthDict{D,T}(
        d::D,
        default_truth::T,
    ) where {D<:AbstractDict{<:Atom,<:Truth},T<:Truth}
        return new{D,T}(d, default_truth)
    end

    function DefaultedTruthDict(
        d::TruthDict{D},
        default_truth::T = BOT,
    ) where {D<:AbstractDict{<:Atom,<:Truth},T<:Union{Any,Truth}}
        default_truth = convert(Truth, default_truth)
        return DefaultedTruthDict{D,T}(d.truth, default_truth)
    end

    function DefaultedTruthDict(
        d::Union{
            AbstractDict{<:Atom,<:Union{Any,Truth}},
            AbstractDict{<:Any,<:Union{Any,Truth}},
            AbstractVector{<:Union{Tuple,Pair}},
            AbstractVector,
            Pair,
            Tuple,
        },
        default_truth::T = BOT,
    ) where {T<:Union{Any,Truth}}
        default_truth = convert(Truth, default_truth)

        if length(d) == 0
            return DefaultedTruthDict(default_truth)
        else
            d = TruthDict(d)
            return DefaultedTruthDict(d, default_truth)
        end
    end

    function DefaultedTruthDict(default_truth = BOT)
        default_truth = convert(Truth, default_truth)
        T = typeof(default_truth)
        d = Dict{Atom,T}([])
        return DefaultedTruthDict{typeof(d),T}(d, default_truth)
    end
end

Base.haskey(i::DefaultedTruthDict, a::Atom) = true

function interpret(a::Atom, i::DefaultedTruthDict, args...; kwargs...)
    return Base.haskey(i.truth, a) ? Base.getindex(i.truth, a) : i.default_truth
end

function inlinedisplay(i::DefaultedTruthDict)
    "DefaultedTruthDict([$(join(["$(syntaxstring(a)) => $t" for (a,t) in i.truth], ", "))], $(i.default_truth))"
end

function Base.show(
    io::IO,
    i::DefaultedTruthDict,
)
    println(io, "DefaultedTruthDict with default truth `$(i.default_truth)` and values:")
    _hpretty_table(io, i.truth |> keys, i.truth |> values)
end

# Helpers
@forward DefaultedTruthDict.truth (
    Base.setindex!, Base.iterate,
    Base.firstindex, Base.lastindex,
    Base.keys,
    Base.values,
)


############################################################################################

"""
    struct TruthTable{A,T<:Truth}

Dictionary which associates an [`AbstractAssignment`](@ref)s to the truth value of the
assignment itself on a [`AbstractSyntaxStructure`](@ref).

See also [`AbstractAssignment`](@ref), [`AbstractSyntaxStructure`](@ref),
[`Truth`](@ref).
"""
struct TruthTable{
    A,
    T<:Truth
} <: Formula # TODO is this correct? Remove?
    truth::Dict{<:AbstractAssignment,Vector{Pair{AbstractSyntaxStructure,T}}}
end

############################################################################################

# Helpers:
#  we let any AbstractDict and AbstractVector be used as an interpretation when model checking.

function check(
    φ::Formula,
    i::Union{AbstractDict,AbstractVector},
    args...
)
    check(φ, convert(AbstractInterpretation, i), args...)
end

# A dictionary is interpreted as the map from atoms to `Truth` values
convert(::Type{AbstractInterpretation}, i::AbstractDict) = TruthDict(i)
# Base.getindex(i::AbstractDict, a::Atom) = i[value(a)]
Base.haskey(a::Atom, i::AbstractDict) = (value(a) in keys(i))
check(a::Atom, i::AbstractDict) = Base.getindex(i, a)

# A vector is interpreted as the set of true atoms
convert(::Type{AbstractInterpretation}, i::AbstractVector) = DefaultedTruthDict(i, ⊥)
# Base.getindex(i::AbstractVector, a::Atom) = (value(a) in i)
# Base.in(a::Atom, i::AbstractVector) = true
check(a::Atom, i::AbstractVector) = (a in i)
