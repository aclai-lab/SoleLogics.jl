using Dictionaries

############################################################################################
####################################### Utilities ##########################################
############################################################################################

"""
    show_assignment_pretty_table(io::IO, keys::Any, matrix::Matrix)

Recreate horizontal pretty table formatting. 
The keys represent the header of the table and the values the first row of the table.
"""
function show_assignment_pretty_table(io::IO, keys::Any, matrix::AbstractMatrix; kwargs...)
    # Prepare columns names
    _keys = map(x -> x isa AbstractAtom ? value(x) : x, collect(keys))
    # Try to draw a complete table
    try
        column_labels = [
            _keys,
            string.(nameof.(typeof.(_keys)))
        ]
        pretty_table(io, matrix; column_labels, kwargs...)
    catch e
        # TODO PrettyTables version < 3
        pretty_table(io, matrix; header = (_keys, string.(nameof.(typeof.(_keys)))), kwargs...)
    end
end

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

See also [`modallogic`](@ref), 
[`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref), [`AlphabetOfAny`](@ref),
[`CompleteFlatGrammar`], [`BooleanAlgebra`](@ref), [`BASE_PROPOSITIONAL_CONNECTIVES`](@ref).
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
####################################### TruthDict ##########################################
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
    If the structure is initialized as empty, [`BooleanTruth`](@ref) values are assumed.

See also [`AbstractAssignment`](@ref), 
[`Interpretation`](@ref),
[`DefaultedTruthDict`](@ref),
[`BooleanTruth`](@ref).
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

function Base.show(io::IO, i::TruthDict)
    if isempty(i.truth)
        print(io, "Empty TruthDict")
    else
        println(io, "TruthDict with values:")
        values = [i.truth[k] for k in keys(i.truth)]
        show_assignment_pretty_table(
            io,
            keys(i.truth),
            reshape(values, 1, length(values))
        )
    end
end

function Base.show(io::IO, is::Vector{<:TruthDict})
    if isempty(is)
        println(io, "$(eltype(is))[]")
    else
        println(io, "$(length(is))-element Vector of $(eltype(is)) with values:")
        ks = unique(Iterators.flatten([keys(i.truth) for i in is]))
        matrix = [i.truth[k] for i in is, k in ks]
        show_assignment_pretty_table(
            io,
            ks,
            matrix
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
################################## DefaultedTruthDict ######################################
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

# Implementation

If you use [`interpret`](@ref) function and you pass a [`DefaultedTruthDict`](@ref) as [`AbstractAssignment`](@ref) 
and the [`Atom`](@ref) is not present in the dictionary, then the default dictionary value will be 
returned and not the [`Atom`](@ref) itself.

Here is an example of this.
```julia-repl
julia> interpret(Atom(5), DefaultedTruthDict(string.(1:4), false))
⊥
```

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

See also [`AbstractAssignment`](@ref), [`Interpretation`](@ref),
[`interpret`](@ref), [`Atom`](@ref),
[`TruthDict`](@ref), [`DefaultedTruthDict`](@ref).
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

function inlinedisplay(i::DefaultedTruthDict)::String
    "DefaultedTruthDict([$(join(["$(syntaxstring(a)) => $t" for (a,t) in i.truth], ", "))], $(i.default_truth))"
end

function Base.show(io::IO, i::DefaultedTruthDict)
    println(io, "DefaultedTruthDict with default truth `$(i.default_truth)` and values:")
    show_assignment_pretty_table(io, keys(i.truth), [i.truth[k] for k in keys(i.truth)])
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
    check(φ::Formula, i::Union{AbstractDict, AbstractVector}, args...)

Takes a [`Formula`](@ref) as input and returns its truth value in relation to the dictionary 
or vector passed. We let any `AbstractDict` and `AbstractVector` be used as an interpretation 
when model checking.

See also [`Formula`](@ref).
"""
function check(algo::CheckAlgorithm, φ::Formula, i::Union{AbstractDict,AbstractVector}, args...)
    check(algo, φ, convert(Interpretation, i), args...)
end

function interpret(φ::Formula, i::Union{AbstractDict,AbstractVector}, args...)
    interpret(φ, convert(Interpretation, i), args...)
end

#############################################################################################
###################################### AbstractDict #########################################
#############################################################################################

"""
    convert(::Type{Interpretation}, i::AbstractDict)

Convert a dictionary (with keys and values) in a [`TruthDict`](@ref).
In this case, a dictionary is interpreted as the map from atoms to `Truth` values.

# Examples

```julia-repl
julia> convert(Interpretation, Dict([1 => ⊤, 2 => ⊥]))
TruthDict with values:
┌───────┬───────┐
│     2 │     1 │
│ Int64 │ Int64 │
├───────┼───────┤
│     ⊥ │     ⊤ │
└───────┴───────┘
```

!!! warning
    For a proper functioning, the values contained in the dictionary and 
    associated with the keys must be Boolean values. If this were not the 
    case, this method could not be used.

See also [`Interpretation`](@ref), [`TruthDict`](@ref).
"""
convert(::Type{Interpretation}, i::AbstractDict) = TruthDict(i)

"""
    Base.haskey(a::Atom, i::AbstractDict)::Bool

Checks whether an atom is contained in any dictionary.

# Examples

```julia-repl
julia> haskey(Atom(1), Dict([1 => ⊤, 2 => ⊥]))
true

julia> haskey(Atom(3), Dict([1 => ⊤, 2 => ⊥]))
false
```

See also [`TruthDict`](@ref).
"""
Base.haskey(a::Atom, i::AbstractDict)::Bool = (value(a) in keys(i))

"""
    check(a::Atom, i::AbstractDict)

Returns the Boolean value corresponding to the atom passed as parameter.

# Examples

```julia-repl
julia> check(Atom(1), Dict([1 => ⊤, 2 => ⊥]))
true

julia> check(Atom(3), Dict([1 => ⊤, 2 => ⊥]))
false
```

See also [`Atom`](@ref).
"""
check(::CheckAlgorithm, a::Atom, i::AbstractDict) = haskey(a,i) ? Base.getindex(i, value(a)) : nothing

#############################################################################################
##################################### AbstractVector ########################################
#############################################################################################

"""
    convert(::Type{Interpretation}, i::AbstractVector)

Converts any vector to a dictionary with all ⊤ and ⊥ default value.
In this case, a vector is interpreted as the set of true atoms.

# Examples

```julia-repl
julia> convert(Interpretation, [1,2,3])
DefaultedTruthDict with default truth `⊥` and values:
┌───────┬───────┬───────┐
│     2 │     3 │     1 │
│ Int64 │ Int64 │ Int64 │
├───────┼───────┼───────┤
│     ⊤ │     ⊤ │     ⊤ │
└───────┴───────┴───────┘

julia> convert(Interpretation, ["a","b"])
DefaultedTruthDict with default truth `⊥` and values:
┌────────┬────────┐
│      b │      a │
│ String │ String │
├────────┼────────┤
│      ⊤ │      ⊤ │
└────────┴────────┘
```

See also [`Interpretation`](@ref), [`DefaultedTruthDict`](@ref), [`TruthDict`](@ref).
"""
convert(::Type{Interpretation}, i::AbstractVector) = DefaultedTruthDict(i, ⊥)
#Base.in(a::Atom, i::AbstractVector) = true

"""
    check(a::Atom, i::AbstractVector)

Returns a truth value indicating whether or not that [`Atom`](@ref) 
is contained in the passed vector.

# Examples

```julia-repl
julia> check(Atom(1), [1,2,4])
true

julia> check(Atom(5), [2,3,4])
false
```

See also [`Atom`](@ref).
"""
check(::CheckAlgorithm, a::Atom, i::AbstractVector) = (value(a) in i)
