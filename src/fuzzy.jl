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

    # Helper
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

function convert(::Type{HeytingTruth}, t::BooleanTruth)
    return istop(t) ? HeytingTruth("⊤", 1) : HeytingTruth("⊥", 2)
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
    print(io, SoleLogics.displaysyntaxvector(v; quotes = false))
end

############################################################################################
#### HeytingAlgebra ########################################################################
############################################################################################

# TODO verify: these may be useful:
# - https://github.com/scheinerman/SimplePosets.jl
# - https://github.com/simonschoelly/SimpleValueGraphs.jl
"""
    struct HeytingAlgebra{D<:AbstractVector{HeytingTruth},
                          G<:Graphs.SimpleGraphs.SimpleDiGraph}
        domain::D
        graph::G
        transitiveclosure::G
    end

A Heyting algebra, represented explicitly
as a domain of truth values, and a graph over them encoding a partial order with
specific constraints (see [here](https://en.m.wikipedia.org/wiki/Heyting_algebra)).
⊤ and ⊥ are always the first and the second element of each algebra, respectively.
A copy of the graph under transitive closure is also stored for optimization purposes.

See also [`@heytingalgebra`](@ref), [`HeytingTruth`](@ref)
"""
struct HeytingAlgebra{
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
} <: AbstractAlgebra{HeytingTruth}
    domain::D
    graph::G # directed graph where (α, β) represents α ≺ β
    transitiveclosure::G # transitive closure of the graph (useful for some optimization)
    isevaluated::Bool
    meet::Vector{Vector{HeytingTruth}}
    join::Vector{Vector{HeytingTruth}}
    implication::Vector{Vector{HeytingTruth}}
    maxmembers::Vector{Vector{HeytingTruth}}
    minmembers::Vector{Vector{HeytingTruth}}

    function HeytingAlgebra(
        domain::D,
        graph::G;
        evaluate::Bool
    ) where {
        D<:AbstractVector{HeytingTruth},
        G<:Graphs.SimpleGraphs.SimpleDiGraph
    }
        @assert length(domain) >= 2 "Cannot instantiate `HeytingAlgebra` with domain " *
            "of length $(length(domain)). Need to specify at least a top and a bottom " *
            "element (to be placed at positions 1 and 2, respectively)."
        @assert isbounded(domain, graph) "Tried to define an HeytingAlgebra with a graph " *
            "which is not a bounded lattice."
        @assert iscomplete(domain, graph) "Tried to define an HeytingAlgebra " *
            "with a graph which is not a complete lattice."
        if evaluate
            tc = transitiveclosure(graph)
            meet = [Vector{HeytingTruth}(undef, length(domain)) for _ in 1:length(domain)]
            join = [Vector{HeytingTruth}(undef, length(domain)) for _ in 1:length(domain)]
            implication = [Vector{HeytingTruth}(undef, length(domain)) for _ in 1:length(domain)]
            maxmembers = Vector{Vector{HeytingTruth}}(undef, length(domain))
            minmembers = Vector{Vector{HeytingTruth}}(undef, length(domain))
            for α ∈ domain
                for β ∈ domain
                    meet[index(α)][index(β)] = greatestlowerbound(domain, tc, α, β)
                    join[index(α)][index(β)] = leastupperbound(domain, tc, α, β)
                end
            end
            for α ∈ domain
                for β ∈ domain
                    η = HeytingTruth(⊥)
                    for γ ∈ domain
                        if precedeq(domain, tc, meet[index(α)][index(γ)], β)
                            η = join[index(η)][index(γ)]
                        end
                    end
                    implication[index(α)][index(β)] = η
                end
            end
            for α ∈ domain
                maxmembers[index(α)] = maximalmembers(domain, tc, α)
                minmembers[index(α)] = minimalmembers(domain, tc, α)
            end
            return new{D,G}(domain, graph, tc, true, meet, join, implication, maxmembers, minmembers)
        else
            return new{D,G}(domain, graph, transitiveclosure(graph), false)
        end
    end

    function HeytingAlgebra(domain::Vector{HeytingTruth}, relations::Vector{Edge{Int64}}; evaluate::Bool=false)
        return HeytingAlgebra(domain, SimpleDiGraph(relations), evaluate=evaluate)
    end
end

domain(h::HeytingAlgebra) = h.domain
top(h::HeytingAlgebra) = h.domain[1]
bot(h::HeytingAlgebra) = h.domain[2]
graph(h::HeytingAlgebra) = h.graph
Graphs.transitiveclosure(h::HeytingAlgebra) = h.transitiveclosure
isevaluated(h::HeytingAlgebra) = h.isevaluated

meet(h::HeytingAlgebra) = h.meet
meet(h::HeytingAlgebra, α, β) = meet(h)[index(α)][index(β)]

Base.join(h::HeytingAlgebra) = h.join
Base.join(h::HeytingAlgebra, α, β) = join(h)[index(α)][index(β)]

implication(h::HeytingAlgebra) = h.implication
implication(h::HeytingAlgebra, α, β) = implication(h)[index(α)][index(β)]

maxmembers(h::HeytingAlgebra) = h.maxmembers
maxmembers(h::HeytingAlgebra, t::HeytingTruth) = maxmembers(h)[index(t)]

minmembers(h::HeytingAlgebra) = h.minmembers
minmembers(h::HeytingAlgebra, t::HeytingTruth) = minmembers(h)[index(t)]

cardinality(h::HeytingAlgebra) = length(domain(h))
isboolean(h::HeytingAlgebra) = (cardinality(h) == 2)

function Graphs.has_path(
    g::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return has_path(g, index(α), index(β))
end

function isbounded(
    d::D,
    g::G
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    for α ∈ d
        if !has_path(g, HeytingTruth(⊥), α) || !has_path(g, α, HeytingTruth(⊤))
            return false
        end
    end
    return true
end

function Graphs.inneighbors(
    d::D,
    g::G,
    t::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return d[inneighbors(g, index(t))]
end

function Graphs.inneighbors(h::HeytingAlgebra, t::HeytingTruth)
    return domain(h)[inneighbors(graph(h), index(t))]
end

function Graphs.outneighbors(
    d::D,
    g::G,
    t::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return d[outneighbors(g, index(t))]
end

function Graphs.outneighbors(h::HeytingAlgebra, t::HeytingTruth)
    return domain(h)[outneighbors(graph(h), index(t))]
end

# α ≺ β (note: in general, α ⊀ β ≠ α ⪰ β)
function precedes(
    d::D,
    tc::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return β ∈ outneighbors(d, tc, α)
end

function precedes(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) 
    return precedes(domain(h), transitiveclosure(h), α, β)
end

function precedes(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth)
    return precedes(domain(h), transitiveclosure(h), α, convert(HeytingTruth, β))
end

function precedes(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth)
    return precedes(domain(h), transitiveclosure(h), convert(HeytingTruth, α), β)
end

function precedes(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth)
    return precedes(
        domain(h),
        transitiveclosure(h),
        convert(HeytingTruth, α),
        convert(HeytingTruth, β)
    )
end

# α ⪯ β (note: in general, α ⪯̸ β ≠ α ≻ β)
function precedeq(
    d::D,
    tc::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return α == β || precedes(d, tc, α, β)
end

function precedeq(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth)
    return α == β || precedes(domain(h), transitiveclosure(h), α, β)
end

function precedeq(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth)
    return precedeq(h, α, convert(HeytingTruth, β))
end

function precedeq(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth)
    return precedeq(h, convert(HeytingTruth, α), β)
end

function precedeq(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth)
    return α == β || precedes(
        domain(h),
        transitiveclosure(h),
        convert(HeytingTruth, α),
        convert(HeytingTruth, β)
    )
end

# α ≻ β (note: in general, α ⊁ β ≠ α ⪯ β)
function succeedes(
    d::D,
    tc::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return β ∈ inneighbors(d, tc, α)
end

function succeedes(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth)
    return succeedes(domain(h), transitiveclosure(h), α, β)
end

function succeedes(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth)
    return succeedes(domain(h), transitiveclosure(h), α, convert(HeytingTruth, β))
end

function succeedes(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth)
    return succeedes(domain(h), transitiveclosure(h), convert(HeytingTruth, α), β)
end

function succeedes(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth)
    return succeedes(
        domain(h),
        transitiveclosure(h),
        convert(HeytingTruth, α),
        convert(HeytingTruth, β)
    )
end

# α ⪰ β (note: in general, α ⪰̸ β ≠ α ≺ β)
function succeedeq(
    d::D,
    tc::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return α == β || succeedes(d, tc, α, β)
end

function succeedeq(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth)
    return α == β || succeedes(domain(h), transitiveclosure(h), α, β)
end

function succeedeq(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth)
    return succeedeq(h, α, convert(HeytingTruth, β))
end

function succeedeq(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth)
    return succeedeq(h, convert(HeytingTruth, α), β)
end

function succeedeq(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth)
    return α == β || succeedes(
        domain(h),
        transitiveclosure(h),
        convert(HeytingTruth, α),
        convert(HeytingTruth, β)
    )
end

"""
Return all maximal members of h not above t.
"""
function maximalmembers(
    d::D,
    tc::G,
    t::HeytingTruth,
    α::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    ismm = true
    mm = Set{HeytingTruth}()
    for o ∈ outneighbors(d, tc, α)
        if !succeedeq(d, tc, o, t)
            ismm = false
            push!(mm, maximalmembers(d, tc, t, o)...)
        end
    end
    ismm ? HeytingTruth[α] : collect(mm)
end

function maximalmembers(
    d::D,
    tc::G,
    t::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    if isbot(t)
        return HeytingTruth[]
    else
        return maximalmembers(d, tc, t, HeytingTruth(⊥))
    end
end

function maximalmembers(h::HeytingAlgebra, t::HeytingTruth)
    if isbot(t)
        return HeytingTruth[]
    elseif isevaluated(h)
        return maxmembers(h, t)
    else
        return maximalmembers(domain(h), transitiveclosure(h), t, HeytingTruth(⊥))
    end
end

maximalmembers(h::HeytingAlgebra, t::BooleanTruth) = maximalmembers(h, HeytingTruth(t))

"""
Return all minimal members of h not below t
"""
function minimalmembers(
    d::D,
    tc::G,
    t::HeytingTruth,
    α::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    ismm = true
    mm = Set{HeytingTruth}()
    for i ∈ inneighbors(d, tc, α)
        if !precedeq(d, tc, i, t)
            ismm = false
            push!(mm, minimalmembers(d, tc, t, i)...)
        end
    end
    ismm ? HeytingTruth[α] : collect(mm)
end

function minimalmembers(
    d::D,
    tc::G,
    t::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    if istop(t)
        return HeytingTruth[]
    else
        return minimalmembers(d, tc, t, HeytingTruth(⊤))
    end
end

function minimalmembers(h::HeytingAlgebra, t::HeytingTruth)
    if istop(t)
        return HeytingTruth[]
    elseif isevaluated(h)
        return minmembers(h, t)
    else
        return minimalmembers(domain(h), transitiveclosure(h), t, HeytingTruth(⊤))
    end
end

minimalmembers(h::HeytingAlgebra, t::BooleanTruth) = minimalmembers(h, HeytingTruth(t))

function greatervalues(
    d::D,
    tc::G,
    α::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return outneighbors(d, tc, α)
end

function upperbounds(
    d::D,
    tc::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    geqα = push!(greatervalues(d, tc, α), α)
    geqβ = push!(greatervalues(d, tc, β), β)
    return geqα[in.(geqα, Ref(geqβ))]
end

function isleastupperbound(
    d::D,
    tc::G,
    α::HeytingTruth,
    ubs::D
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    for ub ∈ ubs
        if !precedeq(d, tc, α, ub)  # note: in general, α ⪯̸ β ≠ α ≻ β
            return false
        end
    end
    return true
end

function leastupperbound(
    d::D,
    tc::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    ubs = upperbounds(d, tc, α, β)
    for ub ∈ ubs
        if isleastupperbound(d, tc, ub, ubs)
            return ub
        end
    end
    return nothing
end

function lesservalues(
    d::D,
    tc::G,
    α::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    return inneighbors(d, tc, α)
end

function lesservalues(h::HeytingAlgebra, t::HeytingTruth)
    return lesservalues(domain(h), transitiveclosure(h), t)
end

function lowerbounds(
    d::D,
    tc::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    leqα = push!(lesservalues(d, tc, α), α)
    leqβ = push!(lesservalues(d, tc, β), β)
    return leqα[in.(leqα, Ref(leqβ))]
end

function isgreatestlowerbound(
    d::D,
    tc::G,
    α::HeytingTruth,
    lbs::D
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    for lb ∈ lbs
        if !succeedeq(d, tc, α, lb) # note: in general, α ⪰̸ β ≠ α ≺ β
            return false
        end
    end
    return true
end

function greatestlowerbound(
    d::D,
    tc::G,
    α::HeytingTruth,
    β::HeytingTruth
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    lbs = lowerbounds(d, tc, α, β)
    for lb ∈ lbs
        if isgreatestlowerbound(d, tc, lb, lbs)
            return lb
        end
    end
    return nothing
end

function iscomplete(
    d::D,
    g::G
) where {
    D<:AbstractVector{HeytingTruth},
    G<:Graphs.SimpleGraphs.SimpleDiGraph
}
    tc = transitiveclosure(g)
    for α ∈ d
        for β ∈ d
            if α != β && (
                isnothing(leastupperbound(d, tc, α, β)) ||
                isnothing(greatestlowerbound(d, tc, α, β))
            )
                return false
            end
        end
    end
    return true
end

Graphs.Edge(t::Tuple{HeytingTruth, HeytingTruth}) = Edge(index(t[1]), index(t[2]))

function Graphs.Edge(t::Tuple{HeytingTruth, BooleanTruth})
    return Edge((t[1], convert(HeytingTruth, t[2])))
end

function Graphs.Edge(t::Tuple{BooleanTruth, HeytingTruth})
    return Edge((convert(HeytingTruth, t[1]), t[2]))
end

function Graphs.Edge(t::Tuple{BooleanTruth, BooleanTruth})
    return Edge((convert(HeytingTruth, t[1]), convert(HeytingTruth, t[2])))
end

"""
    @heytingtruths(labels...)

Instantiate a collection of [`HeytingTruth`](@ref)s and return them as a vector.
⊤ and ⊥ already exist as `const`s of type `BooleanTruth` and they are
treated as `HeytingTruth`s with index 1 and 2, respectively.

!!! info
    `HeytingTruth`s instantiated with this macro are defined in the global scope as
    constants.

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
with domain containing `values` and graph represented by the tuples in `relations`, with
each tuple (α, β) representing a direct edge in the graph asserting α ≺ β.

# Examples
```julia-repl

julia> myalgebra = SoleLogics.@heytingalgebra (α, β) (⊥, α) (⊥, β) (α, ⊤) (β, ⊤)
HeytingAlgebra(HeytingTruth[⊤, ⊥, α, β], SimpleDiGraph{Int64}(4, [Int64[], [3, 4], [1], [1]]
, [[3, 4], Int64[], [2], [2]]))

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

# Meet (greatest lower bound) between values α and β
function collatetruth(
    ::typeof(∧),
    (α, β)::NTuple{N, T where T<:HeytingTruth},
    h::HeytingAlgebra
) where {
    N
}
    if isevaluated(h)
        return meet(h, α, β)
    else
        return greatestlowerbound(domain(h), transitiveclosure(h), α, β)
    end
end

# Join (least upper bound) between values α and β
function collatetruth(
    ::typeof(∨),
    (α, β)::NTuple{N, T where T<:HeytingTruth},
    h::HeytingAlgebra
) where {
    N
}
    if isevaluated(h)
        return join(h, α, β)
    else
        return leastupperbound(domain(h), transitiveclosure(h), α, β)
    end
end

# Implication/pseudo-complement α → β = join(γ | meet(α, γ) ⪯ β)
function collatetruth(
    ::typeof(→),
    (α, β)::NTuple{N, T where T<:HeytingTruth},
    h::HeytingAlgebra
) where {
    N
}
    if isevaluated(h)
        return implication(h, α, β)
    else
        η = bot(h)
        for γ ∈ domain(h)
            if precedeq(h, collatetruth(∧, (α, γ), h), β)
                η = collatetruth(∨, (η, γ), h)
            end
        end
        return η
    end
end

function collatetruth(
    c::Connective,
    (α, β)::Tuple{HeytingTruth, BooleanTruth},
    h::HeytingAlgebra
)
    return collatetruth(c, (α, convert(HeytingTruth, β)), h)
end

function collatetruth(
    c::Connective,
    (α, β)::Tuple{BooleanTruth, HeytingTruth},
    h::HeytingAlgebra
)
    return collatetruth(c, (convert(HeytingTruth, α), β), h)
end

function collatetruth(
    c::Connective,
    (α, β)::Tuple{BooleanTruth, BooleanTruth},
    h::HeytingAlgebra
)
    return collatetruth(c, (convert(HeytingTruth, α), convert(HeytingTruth, β)), h)
end

function simplify(
    c::Connective,
    (α, β)::Tuple{HeytingTruth,HeytingTruth},
    h::HeytingAlgebra
)
    return collatetruth(c, (α, β), h)
end

    function simplify(
    c::Connective,
    (α, β)::Tuple{HeytingTruth,BooleanTruth},
    h::HeytingAlgebra
)
    return collatetruth(c, (α, convert(HeytingTruth, β)), h)
end

    function simplify(
    c::Connective,
    (α, β)::Tuple{BooleanTruth,HeytingTruth},
    h::HeytingAlgebra
)
    return collatetruth(c, (convert(HeytingTruth, α), β), h)
end

    function simplify(
    c::Connective,
    (α, β)::Tuple{BooleanTruth,BooleanTruth},
    h::HeytingAlgebra
)
    return collatetruth(c, (convert(HeytingTruth, α), convert(HeytingTruth, β)), h)
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

function collatetruth(c::Connective, (α,)::Tuple{BooleanTruth}, h::HeytingAlgebra)
    return collatetruth(c, convert(HeytingTruth, α), h)
end

function simplify(c::Connective, (α,)::Tuple{HeytingTruth}, h::HeytingAlgebra)
    return collatetruth(c, (α,), h)
end

    function simplify(c::Connective, (α,)::Tuple{BooleanTruth}, h::HeytingAlgebra)
    return simplify(c, (convert(HeytingTruth, α),), h)
end
