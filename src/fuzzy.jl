using Graphs

struct HeytingTruth
    label::String
    index::Int  # the index of the node in the domain vector: no order is implied!
end

label(heytingtruth::HeytingTruth)::String = heytingtruth.label
index(heytingtruth::HeytingTruth)::Int = heytingtruth.index

struct HeytingAlgebra
    domain::Vector{HeytingTruth}
    top::HeytingTruth
    bot::HeytingTruth
    graph::Graphs.SimpleGraphs.SimpleDiGraph   # directed graph where each edge (α, β) is consistend with α ≺ β
end

domain(h::HeytingAlgebra) = h.domain
top(h::HeytingAlgebra) = h.top
bot(h::HeytingAlgebra) = h.bot
graph(h::HeytingAlgebra) = h.graph

macro heytingtruths(labels...)
    quote
        $(map(t -> :(const $(t[2]) = $(HeytingTruth(string(t[2]), t[1]))), enumerate(labels))...)
        [$(labels...)]
    end |> esc
end

"""
TODO: heytingalgebra(name, domain) and heytingalgebra(name, domain, relations...) macros
"""
macro heytingalgebra(name, relations...)
    quote
        $(labelsset = Set{Symbol}())
        $(map(t -> map(s -> push!(labelsset, s), t.args), relations))
        $(labels = collect(labelsset))
        @heytingtruths $(labels...)     
        domain = [$(labels...)]
        edges = Vector{Edge{Int64}}()
        map(e -> push!(edges, Edge(eval(e))), $relations)
        graph = SimpleDiGraph(edges)
        const $name = (HeytingAlgebra(domain, ⊤0, ⊥0, graph))
    end |> esc
end

Graphs.Edge(t::Tuple{HeytingTruth, HeytingTruth}) = Edge(index(t[1]), index(t[2]))

function Graphs.inneighbors(heytingalgebra::HeytingAlgebra, heytingtruth::HeytingTruth)::Vector{HeytingTruth}
    return domain(heytingalgebra)[inneighbors(graph(heytingalgebra), index(heytingtruth))]
end

function Graphs.outneighbors(heytingalgebra::HeytingAlgebra, heytingtruth::HeytingTruth)::Vector{HeytingTruth}
    return domain(heytingalgebra)[outneighbors(graph(heytingalgebra), index(heytingtruth))]
end

"""
α ≺ β
"""
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

"""
β ≺ α
"""
succeedes(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = precedes(h, β, α)

precedeq(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = α == β ||  precedes(h, α, β)
succeedeq(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = α == β ||  succeedes(h, α, β)

"""
Meet (greatest lower bound) between values α and β
"""
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

"""
Join (least upper bound) between values α and β
"""
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

"""
Implication/pseudo-complement α → β = join(γ | meet(α, γ) = β)
"""
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
