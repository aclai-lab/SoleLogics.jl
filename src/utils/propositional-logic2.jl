export TruthDict2
############################################################################################
####################################### TruthDict2 ##########################################
############################################################################################

"""
    struct TruthDict2{D<:AbstractDict{A where A<:Atom,T where T<:Truth}} <: AbstractAssignment
        truth::D
    end

A logical interpretation instantiated as a dictionary,
explicitly assigning truth values to a *finite* set of atoms.

# Examples
```julia-repl
julia> TruthDict2(1:4)
TruthDict2 with values:
┌────────┬────────┬────────┬────────┐
│      4 │      2 │      3 │      1 │
│  Int64 │  Int64 │  Int64 │  Int64 │
├────────┼────────┼────────┼────────┤
│      ⊤ │      ⊤ │      ⊤ │      ⊤ │
└────────┴────────┴────────┴────────┘


julia> t1 = TruthDict2(1:4, false); t1[5] = true; t1
TruthDict2 with values:
┌───────┬───────┬───────┬───────┬───────┐
│     5 │     4 │     2 │     3 │     1 │
│ Int64 │ Int64 │ Int64 │ Int64 │ Int64 │
├───────┼───────┼───────┼───────┼───────┤
│     ⊤ │     ⊥ │     ⊥ │     ⊥ │     ⊥ │
└───────┴───────┴───────┴───────┴───────┘

julia> t2 = TruthDict2(["a" => true, "b" => false, "c" => true])
TruthDict2 with values:
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
[`DefaultedTruthDict2`](@ref),
[`BooleanTruth`](@ref).
"""
struct TruthDict2{D} <: AbstractAssignment
    truth::D
end

function TruthDict2(d::AbstractDictionary)
    d = dictionary([(convert(Atom, a), convert(Truth, v)) for (a,v) in pairs(d)])
    return TruthDict2{typeof(d)}(d)
end

function TruthDict2(d::AbstractDict{<:Atom,<:Truth})
    # @assert eltype(d) isa Truth "Failed assertion: $(eltype(d)) <: Truth"
    # @assert keytype(d) isa Atom "Failed assertion: $(keytype(d)) <: Atom"
    return TruthDict2{typeof(d)}(d)
end

# Convenience methods
function TruthDict2(d::Dict{V,T}) where {V,T<:Truth}
    # keys -> Atom
    return TruthDict2(dictionary([(Atom{V}(a),v) for (a,v) in d]))
end
function TruthDict2(d::Dict)
    # values -> Truth
    d = dictionary([(a, convert(Truth, v)) for (a,v) in d])
    return TruthDict2(d)
end

function TruthDict2(v::AbstractVector, truth_value = ⊤)
    if length(v) == 0
        return TruthDict2()
    else
        return TruthDict2(dictionary([k => truth_value for k in v]))
    end
end
function TruthDict2(v::AbstractVector{<:Union{Tuple,Pair}})
    if length(v) == 0
        return TruthDict2()
    else
        return TruthDict2(dictionary(v))
    end
end
function TruthDict2(p::Pair)
    return TruthDict2([p])
end
function TruthDict2(t::Tuple)
    return TruthDict2(Pair(t...))
end

# Empty dict
function TruthDict2{D}() where {A<:Atom,T<:Truth,D<:AbstractDict{A,T}}
    return TruthDict2{D}(D())
end
function TruthDict2(T::Type = BooleanTruth)
    d = dictionary(Pair{Atom,T}[])
    return TruthDict2{typeof(d)}(d)
end

Base.haskey(i::TruthDict2, a::Atom) = Base.haskey(i.truth, a)

function interpret(a::Atom, i::TruthDict2, args...; kwargs...)
    return Base.haskey(i, a) ? Base.getindex(i.truth, a) : a
end

function inlinedisplay(i::TruthDict2)
    "TruthDict2([$(join(["$(syntaxstring(a)) => $t" for (a,t) in i.truth], ", "))])"
end

function Base.show(io::IO, i::TruthDict2)
    if isempty(i.truth)
        print(io, "Empty TruthDict2")
    else
        println(io, "TruthDict2 with values:")
        values = [i.truth[k] for k in keys(i.truth)]
        show_assignment_pretty_table(
            io,
            keys(i.truth),
            reshape(values, 1, length(values))
        )
    end
end

function Base.show(io::IO, is::Vector{<:TruthDict2})
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
@forward TruthDict2.truth (
    Base.length, Base.setindex!, Base.iterate,
    Base.IteratorSize, Base.IteratorEltype,
    Base.firstindex, Base.lastindex,
    Base.keys, Base.values,
)
