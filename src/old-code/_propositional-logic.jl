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
    operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
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
    abstract type AbstractAssignment{A,T<:TruthValue} <: AbstractInterpretation{A,T} end

A propositional assigment (or, simply, an *assigment*) is a propositional interpretation,
encoding a mapping from `Atom`s of value type `A`
to truth values of type `T`.

See also [`AbstractInterpretation`](@ref).
"""
abstract type AbstractAssignment{A,T<:TruthValue} <: AbstractInterpretation{A,T} end

"""
    Base.getindex(i::AbstractAssignment{AA,T}, p::Atom, args...)::T where {AA,T<:TruthValue}

Return the truth value of an atom, given an assignment.

See also [`AbstractInterpretation`](@ref).
"""
function Base.getindex(
    i::AbstractAssignment{AA,T},
    ::Atom,
    args...
)::T where {AA,T<:TruthValue}
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
function Base.haskey(i::AbstractAssignment{AA}, ::Atom)::Bool where {AA}
    return error("Please, provide method " *
                 "Base.haskey(::$(typeof(i)), " *
                 "::Atom)::Bool.")
end

# Helpers
function Base.getindex(
    i::AbstractAssignment{AA,T},
    a,
    args...
)::T where {AA,T<:TruthValue}
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

# # Implementation

# With propositional logic, the fallback method extracts the formula's syntax tree and checks it using the logic's
# algebra.

#     check(
#         a::AbstractAlgebra,
#         tree::SyntaxTree,
#         i::AbstractAssignment{A,T},
#         args...
#     )::T where {A,T<:TruthValue}
"""
    check(
        f::AbstractFormula,
        i::AbstractAssignment::{A,T},
        args...
    )::T where {A,T<:TruthValue}

Check a logical formula on an assigment, returning a truth value.
The (finite) [model checking](https://en.wikipedia.org/wiki/Model_checking) algorithm depends
on the given logic.

See also
[`TruthDict`](@ref),
[`SyntaxTree`](@ref), [`AbstractFormula`](@ref),
[`AbstractAlgebra`](@ref), [`AbstractInterpretation`](@ref).
"""
check(f::Formula, i::AbstractAssignment, args...) = check(algebra(f), tree(f), i, args...)

function check(
    a::AbstractAlgebra,
    tree::SyntaxTree,
    i::AbstractAssignment{A,T},
    args...
)::T where {A,T<:TruthValue}
    if token(tree) isa Atom
        return Base.getindex(i, token(tree), args...)
    elseif token(tree) isa AbstractOperator
        ts = Tuple([check(a, childtree, i, args...) for childtree in children(tree)])
        return collatetruth(a, token(tree), ts)
    else
        return error("Unknown token type encountered when checking formula " *
                     "on interpretation of type $(typeof(i)): $(typeof(token(tree))).")
    end
end

# Helper: an atom can be checked on an interpretation; a simple lookup is performed.
check(p::Atom, i::AbstractAssignment{AA}, args...) where {AA} = Base.getindex(i, p, args...)

############################################################################################

"""
    struct TruthDict{
        A,
        T<:TruthValue,
        D<:AbstractDict{<:Atom{<:A},T}
    } <: AbstractAssignment{A,T}
        truth::D
    end

A truth table instantiated as a dictionary,
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
    T<:TruthValue,
    D<:AbstractDict{<:Atom{<:A},T}
} <: AbstractAssignment{A,T}

    truth::D
    synstructs::Dict{AbstractSyntaxStructure,T} # nothing if not computed

    function TruthDict{A,T,D}(
        d::D,
    ) where {
        A,
        T<:TruthValue,
        D<:AbstractDict{<:Atom{<:A},T},
    }
        return new{A,T,D}(d,Dict{AbstractSyntaxStructure,T}())
    end
    function TruthDict{A,T}(d::AbstractDict{<:Atom,T}) where {A,T<:TruthValue}
        return TruthDict{A,T,typeof(d)}(d)
    end
    function TruthDict{A}(d::AbstractDict{<:Atom,T}) where {A,T<:TruthValue}
        return TruthDict{A,T,typeof(d)}(d)
    end
    function TruthDict(d::AbstractDict{<:Atom,T}) where {T<:TruthValue}
        # A = Union{valuetype.(keys(d))...}
        # P = Union{[Atom{_A} for _A in valuetype.(keys(d))]...}
        # println(A)
        # println(d)
        A = typejoin(valuetype.(keys(d))...)
        d = Dict{Atom{A},T}(d)
        return TruthDict{A,T,typeof(d)}(d)
    end
    function TruthDict(d::AbstractDict{A,T}) where {A,T<:TruthValue}
        return TruthDict(Dict{Atom{A},T}([(Atom{A}(a),v) for (a,v) in d]))
    end
    function TruthDict(v::AbstractVector, truth_value = true)
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
    function TruthDict{A,T,D}() where {
        A,
        T<:TruthValue,
        D<:AbstractDict{<:Atom{<:A},T},
    }
        return TruthDict{A,T,D}(Dict{Atom{A},T}())
    end
    function TruthDict()
        d = Dict{Atom{Any},TruthValue}([])
        return TruthDict{Any,TruthValue,typeof(d)}(d)
    end
end

Base.getindex(i::TruthDict{AA}, p::Atom) where {AA} = Base.getindex(i.truth, p)
Base.haskey(i::TruthDict{AA}, p::Atom) where {AA} = Base.haskey(i.truth, p)

function inlinedisplay(i::TruthDict)
    "TruthDict([$(join(["$(syntaxstring(p)) => $t" for (p,t) in i.truth], ", "))])"
end

# Utility function to represent pretty tables horizontally
function _hpretty_table(
    io::IO,
    keys::Any,
    values::Any,
    nvalues::Int; # This is required since "values" might be a generator
    defaulttruth::Union{Nothing,TruthValue} = nothing
)
    # Prepare columns names
    _keys = map(x -> x isa Atom ? value(x) : x, collect(keys))
    header = (_keys, string.(nameof.(typeof.(_keys))))

    try
        # Try to draw a complete table
        data = hcat([x for x in values]...)
        pretty_table(io, data; header=header)
    catch e
        if e isa DimensionMismatch
            # If it is not possible to draw a complete table, then fill the missing
            # values with `defaulttruth` (e.g., there are 3 interpretations but only
            # one is evaluated on a certain formula)
            @warn "Some syntax structures are not resolved with all the interpretations " *
            "(which are $nvalues)\n" *
            "Missing truth values are replaced with default value $defaulttruth."

            data = hcat([
                length(x) == nvalues ? x : [x..., fill(nothing, nvalues - length(x))...]
                for x in values]...
            )

            pretty_table(io, data; header=header)
        end
    end
end

function Base.show(
    io::IO,
    i::TruthDict{A,T,D},
) where {A,T<:TruthValue,D<:AbstractDict{<:Atom{<:A},T}}
    if isempty(i.truth)
        print(io, "Empty TruthDict")
        return
    else
        println(io, "TruthDict with values:")
    end

    _hpretty_table(
        io,
        Iterators.flatten((i.truth |> keys, i.synstructs |> keys)),
        Iterators.flatten((i.truth |> values, i.synstructs |> values)),
        i.truth |> values |> first |>length
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
        T<:TruthValue,
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
    T<:TruthValue,
    D<:AbstractDict{<:Atom{<:A},T}
} <: AbstractAssignment{A,T}

    truth::D

    default_truth::T

    function DefaultedTruthDict{A,T,D}(
        d::D,
        default_truth::T = false,
    ) where {
        A,
        T<:TruthValue,
        D<:AbstractDict{<:Atom{<:A},T},
    }
        return new{A,T,D}(d, default_truth)
    end

    function DefaultedTruthDict(
        d::TruthDict{A,T,D},
        default_truth::T = false,
    ) where {
        A,
        T<:TruthValue,
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
    ) where {A,T<:TruthValue}
        if length(a) == 0
            return DefaultedTruthDict(default_truth)
        else
            return DefaultedTruthDict(TruthDict(a), default_truth)
        end
    end

    function DefaultedTruthDict(
        default_truth::T = false,
    ) where {T<:TruthValue}
        d = Dict{Atom{Any},T}([])
        return DefaultedTruthDict{Any,T,typeof(d)}(d, default_truth)
    end
end

function Base.getindex(i::DefaultedTruthDict{AA}, p::Atom) where {AA}
    return Base.haskey(i.truth, p) ? Base.getindex(i.truth, p) : i.default_truth
end
Base.haskey(i::DefaultedTruthDict{AA}, p::Atom) where {AA} = true

function inlinedisplay(i::DefaultedTruthDict)
    "DefaultedTruthDict([$(join(["$(syntaxstring(p)) => $t" for (p,t) in i.truth], ", "))], $(i.default_truth))"
end

function Base.show(
    io::IO,
    i::DefaultedTruthDict{A,T,D},
) where {A,T<:TruthValue,D<:AbstractDict{<:Atom{<:A},T}}
    println(io, "DefaultedTruthDict with default truth `$(i.default_truth)` and values:")
    _hpretty_table(io, i.truth |> keys, i.truth |> values, i.truth |> values |> length)
end

# Helpers
@forward DefaultedTruthDict.truth (
    Base.setindex!, Base.iterate,
    Base.firstindex, Base.lastindex,
    Base.keys,
    Base.values,
)

############################################################################################

# Helpers:
#  we let any AbstractDict and AbstractVector be used as an interpretation when model checking.

check(f::Formula, i::Union{AbstractDict,AbstractVector}, args...) = check(algebra(f), tree(f), i, args...)
function check(
    a::AbstractAlgebra,
    tree::SyntaxTree,
    i::Union{AbstractDict,AbstractVector},
    args...
)
    check(a, tree, convert(AbstractInterpretation, i), args...)
end

# A dictionary is interpreted as the map from atoms to truth values
convert(::Type{AbstractInterpretation}, i::AbstractDict) = TruthDict(i)
# Base.getindex(i::AbstractDict, p::Atom) = i[value(p)]
Base.haskey(p::Atom, i::AbstractDict) = (value(p) in keys(i))
check(p::Atom, i::AbstractDict) = Base.getindex(i, p)

# A vector is interpreted as the set of true atoms
convert(::Type{AbstractInterpretation}, i::AbstractVector) = DefaultedTruthDict(i, false)
# Base.getindex(i::AbstractVector, p::Atom) = (value(p) in i)
# Base.in(p::Atom, i::AbstractVector) = true
check(p::Atom, i::AbstractVector) = (p in i)

"""
    function feedtruth!(
        td::TruthDict{A,T,D},
        entry::T
    ) where {A,T<:AbstractVector,D<:AbstractDict{<:Atom{<:A},T}}

Push a new interpretation `entry` in a `TruthDict`.

# Examples
```julia-repl
julia> p, q = Atom.(["p", "q"])

julia> td = TruthDict([p => [true], q => [true]])
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│   true │   true │
└────────┴────────┘

julia> SoleLogics.feedtruth!(td, [true, false])
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│   true │   true │
│   true │  false │
└────────┴────────┘
```

See also [`TruthDict`](@ref), [`TruthValue`](@ref).
"""
function feedtruth!(
    td::TruthDict{A,T,D},
    entry::T
) where {A,T<:AbstractVector,D<:AbstractDict{<:Atom{<:A},T}}
    # NOTE: this function could be useful if avoids duplicate entries.
    # In order to efficiently implement duplicates recognition, a Set could be used to
    # see the TruthDict keys from a different perspective.
    [td[k] = vcat(v, e) for (k,v,e) in zip(td.truth|>keys, td.truth|>values, entry)]
    return td
end


"""
    function truth_table(
        st::AbstractSyntaxStructure;
        truthvals::T=[true, false]
    ) where {T <: Vector{<:TruthValue}}

Return a [`TruthDict`](@ref) containing the complete truth table of a generic syntax
structure.

# Arguments
- `st::AbstractSyntaxStructure`: principal structure of the truth table;
- `truthvals::T where {T <: Vector{<:TruthValue}}`: vector of legal truth values; every
    combination of those values is considered when computing the truth table.

# Examples
```julia-repl
julia> st = CONJUNCTION(Atom("p"), Atom("q"))
p ∧ q

julia> truth_table(st, truthvals=[true, false])
TruthDict with values:
┌────────┬────────┬────────────┐
│      q │      p │      p ∧ q │
│ String │ String │ SyntaxTree │
├────────┼────────┼────────────┤
│   true │   true │       true │
│   true │  false │      false │
│  false │   true │      false │
│  false │  false │      false │
└────────┴────────┴────────────┘
```

See also [`TruthDict`](@ref), [`TruthValue`](@ref), [`check`](@ref).
"""
function truth_table(
    st::AbstractSyntaxStructure;
    truthvals::T=[true, false]
) where {T <: Vector{<:TruthValue}}
    props = atoms(st)
    proptypes = typejoin(valuetype.(atoms(st))...)
    # Interpretations generator
    intergen = Iterators.product([truthvals for _ in 1:length(props)]...)

    td = TruthDict{proptypes, T, Dict{Atom{proptypes},T} }(
        Dict([
            props[p] => vec([
                i[p]
                for i in intergen
            ])
            for p in 1:length(props)
        ])
    )

    function _addentry(
        i::T
    ) where {T <: Vector{<:TruthValue}}
        checkans = check(st, TruthDict([prop => truth for (prop, truth) in zip(props, i)]))

        try
            push!(td.synstructs[st], checkans)
        catch e
            if e isa KeyError
                td.synstructs[st] = [checkans]
            end
        end
    end

    map(i -> _addentry([i...]), intergen)
    return td
end

"""
    eagercheck(phi::SoleLogics.AbstractSyntaxStructure)

Return a generator that yields applications of `check` algorithm over `phi`, considering
every possible formula interpretation. Each yielded value is a pair `(i,c)` where `i` is an
interpretation and c corresponds to `check(phi, i)`.

# Examples
```julia-repl
julia> for (interpretation, checkans) in SoleLogics.eagercheck(parseformula("¬(p ∧ q)"))
        println(interpretation)
        print("Checking ¬(p ∧ q) using the above interpretation: ")
        println(checkans)
    end
TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│   true │   true │
└────────┴────────┘

Checking result using the above interpretation: false

TruthDict with values:
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│   true │  false │
└────────┴────────┘

Checking result using the above interpretation: true
...
```

See also [`AbstractAssignment`](@ref), [`check`](@ref).
"""
function eagercheck(
    phi::SoleLogics.AbstractSyntaxStructure;
    truthvals::Vector{T}=[true, false]
) where {T <: TruthValue}
    props = atoms(phi)
    typejoin(valuetype.(atoms(st))...)

    return (
        (
            TruthDict([prop => truth for (prop, truth) in zip(props, interpretation)]),
            check(
                phi,
                TruthDict([prop => truth for (prop, truth) in zip(props, interpretation)])
            )
        )
        for interpretation in Iterators.product([truthvals for _ in 1:length(props)]...)
    )
end
