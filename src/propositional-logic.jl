const BASE_PROPOSITIONAL_OPERATORS = BASE_OPERATORS
const BasePropositionalOperators = Union{typeof.(BASE_PROPOSITIONAL_OPERATORS)...}

# A propositional logic based on the base propositional operators
const BasePropositionalLogic = AbstractLogic{G,A} where {ALP,G<:AbstractGrammar{ALP,<:BasePropositionalOperators},A<:AbstractAlgebra}

"""
    propositionallogic(;
        alphabet = AlphabetOfAny{String}(),
        operators = [⊤, ⊥, ¬, ∧, ∨, →],
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), [⊤, ⊥, ¬, ∧, ∨, →]),
        algebra = BooleanAlgebra(),
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
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
)
    _baselogic(
        alphabet = alphabet,
        operators = operators,
        grammar = grammar,
        algebra = algebra;
        default_operators = BASE_PROPOSITIONAL_OPERATORS,
        logictypename = "propositional logic",
    )
end

############################################################################################

"""
    abstract type AbstractAssignment{A,T<:Truth} <: AbstractInterpretation{A,T} end

A propositional assigment (or, simply, an *assigment*) is a propositional interpretation,
encoding a mapping from `Atom`s of value type `A`
to truth values of type `T`.

See also [`AbstractInterpretation`](@ref).
"""
abstract type AbstractAssignment{A,T<:Truth} <: AbstractInterpretation{A,T} end

"""
    Base.getindex(i::AbstractAssignment{AA,T}, a::Atom, args...)::T where {AA,T<:Truth}

Return the truth value of an atom, given an assignment.

See also [`AbstractInterpretation`](@ref).
"""
function Base.getindex(
    i::AbstractAssignment{AA,T},
    ::Atom,
    args...
)::T where {AA,T<:Truth}
    return error("Please, provide method " *
                 "Base.getindex(::$(typeof(i)), " *
                 "::Atom, " *
                 "args...::$(typeof(args))::$(truthtype(i)).")
end

"""
    Base.haskey(::Atom{A}, i::AbstractAssignment{A})::Bool where {A}

Return whether an assigment has a truth value for a given atom.

See also [`AbstractInterpretation`](@ref).
"""
function Base.haskey(i::AbstractAssignment, ::Atom)::Bool
    return error("Please, provide method " *
                 "Base.haskey(::$(typeof(i)), " *
                 "::Atom)::Bool.")
end

# Helpers
function Base.getindex(
    i::AbstractAssignment{AA,T},
    a,
    args...
)::T where {AA,T<:Truth}
    # if !(a isa Atom)
        Base.getindex(i, Atom(a))
    # else
    #     return error("Please, provide method" *
    #                  " Base.getindex(::$(typeof(i))," *
    #                  " a," *
    #                  " args...::$(typeof(args))::$(truthtype(i)).")
    # end
end
function Base.haskey(i::AbstractAssignment, a)::Bool
    # if !(a isa Atom)
        Base.haskey(i, Atom(a))
    # else
    #     return error("Please, provide method" *
    #                  " Base.haskey(::$(typeof(i))," *
    #                  " a)::Bool.")
    # end
end


function inlinedisplay(i::AbstractAssignment)
    return error("Please, provide method inlinedisplay(::$(typeof(i)))::String.")
end

# TODO: get inspiration from PAndQ package and write interpret function.
# TODO: change collatetruth name (concepts are "unite and simplify")
function interpret(
    tree::SyntaxBranch,
    i::AbstractAssignment,
    args...;
    kwargs...
)
    return collatetruth(token(tree), Tuple(
        [interpret(ch, i, args...; kwargs...) for ch in children(tree)]
    ))
end

# When interpreting a single atom, if the lookup fails then return the atom itself
function interpret(
    a::Atom,
    i::AbstractAssignment,
    args...;
    kwargs...
)
    try
        Base.getindex(i, a, args...)
    catch e
        if e isa BoundsError
            a
        else
            rethrow(e)
        end
    end
end

interpret(t::Truth, args...; kwargs...) = t

# Different ways to call interpret
# i[a] -> (a itself, or a single Truth value!)
# This has to be user-defined when creating a custom AbstractAssignment concrete type.
# Otherwise, an error is thrown noticing the user (see our most general dispatch).
# Note by Gio: these are written for AbstractAssignment, but
#  isn't this true for any AbstractInterpretation? That is, also at the non-propositional level?
#  Probably. Therefore, these should be moved to core and AbstractAssignment->AbstractInterpretation.
# By Mauro: Done, see core.jl after AbstractInterpretation definition

############################################################################################
#################################### IMPLEMENTATIONS #######################################
############################################################################################

"""
    struct TruthDict{
        A,
        T<:Truth,
        D<:AbstractDict{<:Atom{<:A},T}
    } <: AbstractAssignment{A,T}
        truth::D
    end

A logical interpretation instantiated as a dictionary,
explicitly assigning truth values to a *finite* set of atoms.
If prompted for the value of an unknown atom, it throws an error.

# Examples
```julia-repl
julia> TruthDict(1:4)
TruthDict with values:
┌───────┬───────┬───────┬───────┐
│     4 │     2 │     3 │     1 │
│ Int64 │ Int64 │ Int64 │ Int64 │
├───────┼───────┼───────┼───────┤
│  true │  true │  true │  true │
└───────┴───────┴───────┴───────┘


julia> t1 = TruthDict(1:4, false); t1[5] = true; t1
TruthDict with values:
┌───────┬───────┬───────┬───────┬───────┐
│     5 │     4 │     2 │     3 │     1 │
│ Int64 │ Int64 │ Int64 │ Int64 │ Int64 │
├───────┼───────┼───────┼───────┼───────┤
│  true │ false │ false │ false │ false │
└───────┴───────┴───────┴───────┴───────┘


julia> t2 = TruthDict(["a" => true, "b" => false, "c" => true])
TruthDict with values:
┌────────┬────────┬────────┐
│      c │      b │      a │
│ String │ String │ String │
├────────┼────────┼────────┤
│   true │  false │   true │
└────────┴────────┴────────┘

julia> check(parsebaseformula("a ∨ b"), t2)
true

```

See also
[`DefaultedTruthDict`](@ref),
[`AbstractAssignment`](@ref), [`AbstractInterpretation`](@ref).
"""
struct TruthDict{
    A,
    T<:Truth,
    D<:AbstractDict{<:Atom{<:A},T}
} <: AbstractAssignment{A,T}

    truth::D

    function TruthDict{A,T,D}(
        d::D,
    ) where {
        A,
        T<:Truth,
        D<:AbstractDict{<:Atom{<:A},T},
    }
        truthtype = supertype(T)
        d = Dict{Atom{A},truthtype}(d)
        return new{A,truthtype,typeof(d)}(d)
    end
    function TruthDict{A,T}(d::AbstractDict{<:Atom,T}) where {A,T<:Truth}
        return TruthDict{A,T,typeof(d)}(d)
    end
    function TruthDict{A}(d::AbstractDict{<:Atom,T}) where {A,T<:Truth}
        return TruthDict{A,T,typeof(d)}(d)
    end
    function TruthDict(d::AbstractDict{<:Atom,T}) where {T<:Truth}
        # DEBUG:
        # A = Union{valuetype.(keys(d))...}
        # P = Union{[Atom{_A} for _A in valuetype.(keys(d))]...}
        # println(A)
        # println(d)
        A = typejoin(valuetype.(keys(d))...)
        return TruthDict{A,T,typeof(d)}(d)
    end
    function TruthDict(d::AbstractDict{A,T}) where {A,T<:Truth}
        return TruthDict(Dict([(Atom{A}(a),v) for (a,v) in d]))
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

    function TruthDict{A,T,D}() where {
        A,
        T<:Truth,
        D<:AbstractDict{<:Atom{<:A},T},
    }
        return TruthDict{A,T,D}(D())
    end
    function TruthDict{A,T}() where {
        A,
        T<:Truth,
    }
        d = Dict{Atom{A},T}()
        return TruthDict{A,T,typeof(d)}(d)
    end
    function TruthDict{A}() where {
        A,
    }
        return TruthDict{A,BooleanTruth}()
    end
    function TruthDict()
        A = Any
        T = Truth
        d = Dict{Atom{A},T}([])
        return TruthDict{A,T,typeof(d)}(d)
    end
end

function interpret(a::Atom, i::TruthDict, args...; kwargs...)
    return Base.haskey(i, a) ? Base.getindex(i.truth, a) : a
end

Base.haskey(i::TruthDict, a::Atom) = Base.haskey(i.truth, a)

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
    i::TruthDict{A,T,D},
) where {A,T<:Truth,D<:AbstractDict{<:Atom{<:A},T}}
    if isempty(i.truth)
        print(io, "Empty TruthDict")
        return
    else
        println(io, "TruthDict with values:")
    end

    _hpretty_table(
        io,
        i.truth |> keys,  # Iterators.flatten([i.truth |> keys]),
        i.truth |> values # Iterators.flatten([i.truth |> values])
    )
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
        A,
        T<:Truth,
        D<:AbstractDict{<:Atom{<:A},T}
    } <: AbstractAssignment{A,T}
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
DefaultedTruthDict with default truth `false` and values:
┌────────┬────────┬────────┬────────┬────────┐
│      4 │      1 │      5 │      2 │      3 │
│ String │ String │ String │ String │ String │
├────────┼────────┼────────┼────────┼────────┤
│   true │   true │  false │   true │   true │
└────────┴────────┴────────┴────────┴────────┘


julia> check(parsebaseformula("1 ∨ 2"), t1)
true

julia> check(parsebaseformula("1 ∧ 5"), t1)
false

```

See also
[`TruthDict`](@ref),
[`AbstractAssignment`](@ref), [`AbstractInterpretation`](@ref).
"""
struct DefaultedTruthDict{
    A,
    T<:Truth,
    D<:AbstractDict{<:Atom{<:A},T}
} <: AbstractAssignment{A,T}

    truth::D

    default_truth::T

    function DefaultedTruthDict{A,T,D}(
        d::D,
        default_truth::T = false,
    ) where {
        A,
        T<:Truth,
        D<:AbstractDict{<:Atom{<:A},T},
    }
        return new{A,T,D}(d, default_truth)
    end

    function DefaultedTruthDict(
        d::TruthDict{A,T,D},
        default_truth::T = false,
    ) where {
        A,
        T<:Truth,
        D<:AbstractDict{<:Atom{<:A},T}
    }
        return DefaultedTruthDict{A,T,D}(d.truth, default_truth)
    end

    function DefaultedTruthDict(
        a::Union{
            AbstractDict{<:Atom,T},
            AbstractDict{A,T},
            AbstractVector{<:Union{Tuple,Pair}},
            AbstractVector,
            Pair,
            Tuple,
        },
        default_truth::T = false,
    ) where {A,T<:Truth}
        if length(a) == 0
            return DefaultedTruthDict(default_truth)
        else
            return DefaultedTruthDict(TruthDict(a), default_truth)
        end
    end

    function DefaultedTruthDict(
        default_truth::T = false,
    ) where {T<:Truth}
        d = Dict{Atom{Any},T}([])
        return DefaultedTruthDict{Any,T,typeof(d)}(d, default_truth)
    end
end

function interpret(a::Atom, i::DefaultedTruthDict, args...; kwargs...)
    return Base.haskey(i.truth, a) ? Base.getindex(i.truth, a) : i.default_truth
end
Base.haskey(i::DefaultedTruthDict, a::Atom) = true

function inlinedisplay(i::DefaultedTruthDict)
    "DefaultedTruthDict([$(join(["$(syntaxstring(a)) => $t" for (a,t) in i.truth], ", "))], $(i.default_truth))"
end

function Base.show(
    io::IO,
    i::DefaultedTruthDict{A,T,D},
) where {A,T<:Truth,D<:AbstractDict{<:Atom{<:A},T}}
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

# NOTE: the following has to be discussed @mauro-milella
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
    truth::Dict{<:AbstractAssignment{A,T},Vector{Pair{AbstractSyntaxStructure,T}}}
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

# A dictionary is interpreted as the map from atoms to truth values
convert(::Type{AbstractInterpretation}, i::AbstractDict) = TruthDict(i)
# Base.getindex(i::AbstractDict, a::Atom) = i[value(a)]
Base.haskey(a::Atom, i::AbstractDict) = (value(a) in keys(i))
check(a::Atom, i::AbstractDict) = Base.getindex(i, a)

# A vector is interpreted as the set of true atoms
convert(::Type{AbstractInterpretation}, i::AbstractVector) = DefaultedTruthDict(i, ⊥)
# Base.getindex(i::AbstractVector, a::Atom) = (value(a) in i)
# Base.in(a::Atom, i::AbstractVector) = true
check(a::Atom, i::AbstractVector) = (a in i)
