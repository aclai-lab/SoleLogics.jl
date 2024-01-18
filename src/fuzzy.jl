using Graphs

# Author: alberto-paparella

############################################################################################
#### HeytingTruth ##########################################################################
############################################################################################

"""
    struct HeytingTruth <: Truth
        label::String
        index::Int
    end

A truth value of a Heyting algebra.
Heyting truth values are represented by a label, and an index corresponding to its
position in the domain vector of the associated algebra.
Values `⊤` and `⊥` always exist with index 1 and 2, respectively.
New values can be easily constructed via the [`@heytingtruths`](@ref) macro.

See also [`@heytingtruths`](@ref), [`HeytingAlgebra`](@ref), [`Truth`](@ref)
"""
struct HeytingTruth <: Truth
    label::String
    index::Int  # the index of the node in the domain vector: no order is implied!

    function HeytingTruth(label::String, index::Int)
        return new(label, index)
    end

    # Helpers
    function HeytingTruth(booleantruth::BooleanTruth)
        return convert(HeytingTruth, booleantruth)
    end
end

"""
Return the label of a [`HeytingTruth`](@ref).
"""
label(t::HeytingTruth)::String = t.label

"""
Return the index of a [`HeytingTruth`](@ref).
"""
index(t::HeytingTruth)::Int = t.index

istop(t::HeytingTruth) = index(t) == 1
isbot(t::HeytingTruth) = index(t) == 2

syntaxstring(t::HeytingTruth; kwargs...) = label(t)

convert(::Type{HeytingTruth}, t::HeytingTruth) = t

function convert(::Type{HeytingTruth}, booleantruth::BooleanTruth)
    return istop(booleantruth) ? HeytingTruth("⊤", 1) : HeytingTruth("⊥", 2)
end

# Convert an object of type HeytingTruth to an object of type `BooleanTruth` (if possible).
function convert(::Type{BooleanTruth}, t::HeytingTruth)
    if istop(t)
        return TOP
    elseif isbot(t)
        return BOT
    else
        error("Cannot convert HeytingTruth \"" * syntaxstring(t) * "\" to BooleanTruth. " *
              "Only ⊤ and ⊥ can be converted to BooleanTruth.")
    end
end

# Helper
function Base.show(io::IO, v::Vector{HeytingTruth})
    print(io, displaysyntaxvector(v; quotes = false))
end

############################################################################################
#### HeytingAlgebra ########################################################################
############################################################################################

# TODO verify: these may be useful:
# - https://github.com/scheinerman/SimplePosets.jl
# - https://github.com/simonschoelly/SimpleValueGraphs.jl
"""
    struct HeytingAlgebra{D<:AbstractVector{HeytingTruth},G<:Graphs.SimpleGraphs.SimpleDiGraph}
        domain::D
        graph::G
    end

A Heyting algebra, represented explicitly
as a domain of truth values, and a graph over them encoding a partial order with
specific constraints (see [here](https://en.m.wikipedia.org/wiki/Heyting_algebra)).
⊤ and ⊥ are always the first and the second element of each algebra, respectively.

See also [`@heytingalgebra`](@ref), [`HeytingTruth`](@ref)
"""
struct HeytingAlgebra{
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph,
}
    domain::D
    graph::G # directed graph where (α, β) represents α ≺ β

    function HeytingAlgebra(
        domain::D,
        graph::G,
    ) where {
        D<:AbstractVector{HeytingTruth},
        G<:Graphs.SimpleGraphs.SimpleDiGraph,
    }
        @assert length(domain) >= 2 "Cannot instantiate `HeytingAlgebra` with domain " *
            "of length $(length(domain)). Need to specify at least a top and a bottom " *
            "element (to be placed at positions 1 and 2, respectively)."
        @assert isbounded(domain, graph) "Tried to define an HeytingAlgebra with a graph" *
            "which is not a bounded lattice."
        @assert iscomplete(domain, graph) "Tried to define an HeytingAlgebra" *
            "with a graph which is not a complete lattice."
        return new{D,G}(domain, graph)
    end

    function HeytingAlgebra(domain::Vector{HeytingTruth}, relations::Vector{Edge{Int64}})
        return HeytingAlgebra(domain, SimpleDiGraph(relations))
    end
end

domain(h::HeytingAlgebra) = h.domain
top(h::HeytingAlgebra) = h.domain[1]
bot(h::HeytingAlgebra) = h.domain[2]
graph(h::HeytingAlgebra) = h.graph

cardinality(h::HeytingAlgebra) = length(domain(h))
isboolean(h::HeytingAlgebra) = (cardinality(h) == 2)

Graphs.has_path(graph::AbstractGraph, α::HeytingTruth, β::HeytingTruth) = has_path(graph, index(α), index(β))

function isbounded(domain::Vector{HeytingTruth}, graph::Graphs.SimpleGraphs.SimpleDiGraph)
    for α ∈ domain
        if !has_path(graph, HeytingTruth(⊥), α) || !has_path(graph, α, HeytingTruth(⊤))
            return false
        end
    end
    return true
end

function Graphs.inneighbors(d::Vector{HeytingTruth}, g::Graphs.SimpleGraphs.SimpleDiGraph, t::HeytingTruth)::Vector{HeytingTruth}
    return d[inneighbors(g, index(t))]
end

function Graphs.outneighbors(d::Vector{HeytingTruth}, g::Graphs.SimpleGraphs.SimpleDiGraph, t::HeytingTruth)::Vector{HeytingTruth}
    return d[outneighbors(g, index(t))]
end

precedes(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, β::HeytingTruth) = β ∈ outneighbors(d, ctg, α)

precedeq(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, β::HeytingTruth) = α == β ||  precedes(d, ctg, α, β)

succeedes(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, β::HeytingTruth) = precedes(d, ctg, β, α)

succeedeq(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, β::HeytingTruth) = α == β ||  succeedes(d, ctg, α, β)

greatervalues(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth) = union(HeytingTruth[α], outneighbors(d, ctg, α))

function upperbounds(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, β::HeytingTruth)
    return greatervalues(d, ctg, α)[in.(greatervalues(d, ctg, α), Ref(greatervalues(d, ctg, β)))]
end

function isleastupperbound(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, ubs::Vector{HeytingTruth})
    for ub ∈ ubs
        if !precedeq(d, ctg, α, ub)
            return false
        end
    end
    return true
end

function leastupperbound(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, β::HeytingTruth)
    ubs = upperbounds(d, ctg, α, β)
    for ub ∈ ubs
        if isleastupperbound(d, ctg, ub, ubs)
            return ub
        end
    end
    return nothing
end

lesservalues(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth) = union(HeytingTruth[α], inneighbors(d, ctg, α))

function lowerbounds(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, β::HeytingTruth)
    return lesservalues(d, ctg, α)[in.(lesservalues(d, ctg, α), Ref(lesservalues(d, ctg, β)))]
end

function isgreatestlowerbound(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, lbs::Vector{HeytingTruth})
    for lb ∈ lbs
        if !succeedeq(d, ctg, α, lb)
            return false
        end
    end
    return true
end

function greatestlowerbound(d::Vector{HeytingTruth}, ctg::Graphs.SimpleGraphs.SimpleDiGraph, α::HeytingTruth, β::HeytingTruth)
    lbs = lowerbounds(d, ctg, α, β)
    for lb ∈ lbs
        if isgreatestlowerbound(d, ctg, lb, lbs)
            return lb
        end
    end
    return nothing
end

function iscomplete(d::Vector{HeytingTruth}, g::Graphs.SimpleGraphs.SimpleDiGraph)
    ctg = transitiveclosure(g)
    for α ∈ d
        for β ∈ d
            if α != β && (isnothing(leastupperbound(d, ctg, α, β)) || isnothing(greatestlowerbound(d, ctg, α, β)))
                return false
            end
        end
    end
    return true
end

"""
    @heytingtruths(labels...)

Instantiate a collection of [`HeytingTruth`](@ref)s and return them as a vector.
⊤ and ⊥ already exist as `const`s of type `BooleanTruth` and they are
treated as `HeytingTruth`s with index 1 and 2, respectively.

!!! info
    `HeytingTruth`s instantiated with this macro are defined in the global scope as constants.

# Examples
```julia-repl
julia> SoleLogics.@heytingtruths α β
2-element Vector{HeytingTruth}:
 HeytingTruth: α
 HeytingTruth: β

julia> α
HeytingTruth: α

See also [`HeytingTruth`](@ref), [`@heytingalgebra`](@ref)
"""
macro heytingtruths(labels...)
    quote
        $(map(t -> begin
            if !(t[2] in [Symbol(:⊤), Symbol(:⊥)])
                :(const $(t[2]) = $(HeytingTruth(string(t[2]), t[1]+2)))
            else
                return error("Invalid heyting truth provided: $(t[2]). " *
                    "Symbols `⊤` and `⊥` are reserved for the top and bottom of the "
                    * "algebra, and they do not need to be specified.")
            end
        end, enumerate(labels))...)
        HeytingTruth[$(labels...)]
    end |> esc
end

"""
    @heytingalgebra(values, relations...)

Construct a [`HeytingAlgebra`](@ref)
with domain containing `values` and graph represented by the tuples in `relations`, with each
tuple (α, β) representing a direct edge in the graph asserting α ≺ β.

# Examples
```julia-repl

julia> myalgebra = SoleLogics.@heytingalgebra (α, β) (⊥, α) (⊥, β) (α, ⊤) (β, ⊤)
HeytingAlgebra(HeytingTruth[⊤, ⊥, α, β], SimpleDiGraph{Int64}(4, [Int64[], [3, 4], [1], [1]], [[3, 4], Int64[], [2], [2]]))

See also [`HeytingTruth`](@ref), [`@heytingalgebra`](@ref)
"""
macro heytingalgebra(values, relations...)
    quote
        labels = @heytingtruths $(values.args...)
        domain = HeytingTruth[convert(HeytingTruth, ⊤), convert(HeytingTruth, ⊥), labels...]
        edges = Vector{SoleLogics.Graphs.Edge{Int64}}()
        map(e -> push!(edges, SoleLogics.Graphs.Edge(eval(e))), $relations)
        HeytingAlgebra(domain, edges)
    end |> esc
end

Graphs.Edge(t::Tuple{HeytingTruth, HeytingTruth}) = Edge(index(t[1]), index(t[2]))
Graphs.Edge(t::Tuple{HeytingTruth, BooleanTruth}) = Edge((t[1], convert(HeytingTruth, t[2])))
Graphs.Edge(t::Tuple{BooleanTruth, HeytingTruth}) = Edge((convert(HeytingTruth, t[1]), t[2]))
Graphs.Edge(t::Tuple{BooleanTruth, BooleanTruth}) = Edge((convert(HeytingTruth, t[1]), convert(HeytingTruth, t[2])))

function Graphs.inneighbors(heytingalgebra::HeytingAlgebra, t::HeytingTruth)::Vector{HeytingTruth}
    return domain(heytingalgebra)[inneighbors(graph(heytingalgebra), index(t))]
end

function Graphs.outneighbors(heytingalgebra::HeytingAlgebra, t::HeytingTruth)::Vector{HeytingTruth}
    return domain(heytingalgebra)[outneighbors(graph(heytingalgebra), index(t))]
end

# α ≺ β
function precedes(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth)
    if α ∈ inneighbors(h, β)
        return true
    else
        for γ ∈ outneighbors(h, α)
            if precedes(h, γ, β)
                return true
            end
        end
        return false
    end
end
precedes(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth) = precedes(h, α, convert(HeytingTruth, β))
precedes(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth) = precedes(h, convert(HeytingTruth, α), β)
precedes(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth) = precedes(h, convert(HeytingTruth, α), convert(HeytingTruth, β))

# β ≺ α
succeedes(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = precedes(h, β, α)
succeedes(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth) = succeedes(h, α, convert(HeytingTruth, β))
succeedes(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth) = succeedes(h, convert(HeytingTruth, α), β)
succeedes(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth) = succeedes(h, convert(HeytingTruth, α), convert(HeytingTruth, β))

# α ⪯ β
precedeq(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = α == β ||  precedes(h, α, β)
precedeq(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth) = precedeq(h, α, convert(HeytingTruth, β))
precedeq(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth) = precedeq(h, convert(HeytingTruth, α), β)
precedeq(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth) = precedeq(h, convert(HeytingTruth, α), convert(HeytingTruth, β))

# β ⪯ α
succeedeq(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = α == β ||  succeedes(h, α, β)
succeedeq(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth) = succeedeq(h, α, convert(HeytingTruth, β))
succeedeq(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth) = succeedeq(h, convert(HeytingTruth, α), β)
succeedeq(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth) = succeedeq(h, convert(HeytingTruth, α), convert(HeytingTruth, β))

# Meet (greatest lower bound) between values α and β
function collatetruth(::typeof(∧), (α, β)::NTuple{N, T where T<:HeytingTruth}, h::HeytingAlgebra) where {N}
    if precedeq(h, α, β)
        return α
    elseif succeedes(h, α, β)
        return β
    else
        for γ ∈ inneighbors(h, α)
            if γ ∈ inneighbors(h, β)
                return γ
            else
                collatetruth(∧, (α, β), h)
            end
        end
    end
end

# Join (least upper bound) between values α and β
function collatetruth(::typeof(∨), (α, β)::NTuple{N, T where T<:HeytingTruth}, h::HeytingAlgebra) where {N}
    if succeedeq(h, α, β)
        return α
    elseif precedes(h, α, β)
        return β
    else
        for γ ∈ outneighbors(h, α)
            if γ ∈ outneighbors(h, β)
                return γ
            else
                return collatetruth(∨, (α, β), h)
            end
        end
    end
end

# Implication/pseudo-complement α → β = join(γ | meet(α, γ) = β)
function collatetruth(::typeof(→), (α, β)::NTuple{N, T where T<:HeytingTruth}, h::HeytingAlgebra) where {N}
    η = bot(h)
    for γ ∈ domain(h)
        if precedeq(h, collatetruth(∧, (α, γ), h), β)
            η = collatetruth(∨, (η, γ), h)
        else
            continue
        end
    end
    return η
end

collatetruth(c::Connective, (α, β)::Tuple{HeytingTruth, BooleanTruth}, h::HeytingAlgebra) = collatetruth(c, (α, convert(HeytingTruth, β)), h)
collatetruth(c::Connective, (α, β)::Tuple{BooleanTruth, HeytingTruth}, h::HeytingAlgebra) = collatetruth(c, (convert(HeytingTruth, α), β), h)
collatetruth(c::Connective, (α, β)::Tuple{BooleanTruth, BooleanTruth}, h::HeytingAlgebra) = collatetruth(c, (convert(HeytingTruth, α), convert(HeytingTruth, β)), h)

simplify(c::Connective, (α, β)::Tuple{HeytingTruth,HeytingTruth}, h::HeytingAlgebra) = collatetruth(c, (α, β), h)
simplify(c::Connective, (α, β)::Tuple{HeytingTruth,BooleanTruth}, h::HeytingAlgebra) = collatetruth(c, (α, convert(HeytingTruth, β)), h)
simplify(c::Connective, (α, β)::Tuple{BooleanTruth,HeytingTruth}, h::HeytingAlgebra) = collatetruth(c, (convert(HeytingTruth, α), β), h)
simplify(c::Connective, (α, β)::Tuple{BooleanTruth,BooleanTruth}, h::HeytingAlgebra) = collatetruth(c, (convert(HeytingTruth, α), convert(HeytingTruth, β)), h)

# Note: output type can both be BooleanTruth or HeytingTruth, i.e., the following check can be used effectively
# convert(HeytingTruth, interpret(φ, td8)) == convert(HeytingTruth,interpret(φ, td8, booleanalgebra)))
function interpret(φ::SyntaxBranch, i::AbstractAssignment, h::HeytingAlgebra, args...; kwargs...)::Formula
    return simplify(token(φ), Tuple(
        [interpret(ch, i, h, args...; kwargs...) for ch in children(φ)]
    ), h)
end

function collatetruth(::typeof(¬), (α,)::Tuple{HeytingTruth}, h::HeytingAlgebra)
    if isboolean(h)
        if istop(α)
            return ⊥
        else
            return ⊤
        end
    else
        return error("¬ operation isn't defined outside of BooleanAlgebra")
    end
end
collatetruth(c::Connective, (α,)::Tuple{BooleanTruth}, h::HeytingAlgebra) = collatetruth(c, convert(HeytingTruth, α), h)

simplify(c::Connective, (α,)::Tuple{HeytingTruth}, h::HeytingAlgebra) = collatetruth(c, (α,), h)
simplify(c::Connective, (α,)::Tuple{BooleanTruth}, h::HeytingAlgebra) = simplify(c, (convert(HeytingTruth, α),), h)
