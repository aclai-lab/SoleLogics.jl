using Graphs

struct HeytingTruth
    label::String
    index::Int  # the index of the node in the domain vector: no order is implied!
end

label(heytingtruth::HeytingTruth)::String = heytingtruth.label
index(heytingtruth::HeytingTruth)::Int = heytingtruth.index

convert(::Type{HeytingTruth}, booleantruth::BooleanTruth) = istop(booleantruth) ? HeytingTruth("⊤", 2) : HeytingTruth("⊥", 1)

struct HeytingAlgebra
    domain::Vector{HeytingTruth}
    top::HeytingTruth
    bot::HeytingTruth
    graph::Graphs.SimpleGraphs.SimpleDiGraph   # directed graph where each edge (α, β) is consistend with α ≺ β

    function HeytingAlgebra(domain::Vector{HeytingTruth}, top::HeytingTruth, bot::HeytingTruth, graph::Graphs.SimpleGraphs.SimpleDiGraph)
        return new(domain, top, bot, graph)
    end

    function HeytingAlgebra(domain::Vector{HeytingTruth}, graph::Graphs.SimpleGraphs.SimpleDiGraph)
        return HeytingAlgebra(domain, convert(HeytingTruth, ⊤), convert(HeytingTruth, ⊥), graph)
    end

    function HeytingAlgebra(domain::Vector{HeytingTruth}, relations::Vector{Edge{Int64}})
        return HeytingAlgebra(domain, convert(HeytingTruth, ⊤), convert(HeytingTruth, ⊥), SimpleDiGraph(relations))
    end
end

domain(h::HeytingAlgebra) = h.domain
top(h::HeytingAlgebra) = h.top
bot(h::HeytingAlgebra) = h.bot
graph(h::HeytingAlgebra) = h.graph

"""
⊥ and ⊤ already exist as const of type BooleanTruth and they are treated as HeytingTruth with index 1 and 2 respectively
"""
macro heytingtruths(labels...)
    quote
        $(map(t -> :(const $(t[2]) = $(HeytingTruth(string(t[2]), t[1]+2))), enumerate(labels))...)
        [$(labels...)]
    end |> esc
end

"""
Note that values of type HeytingTruth must be created beforehand (e.g., with @heytingvalues values...) and not include ⊥ and ⊤
"""
macro heytingalgebra(name, values, relations...)
    quote
        domain = [convert(HeytingTruth, ⊥), convert(HeytingTruth, ⊤), $values...]
        edges = Vector{Edge{Int64}}()
        map(e -> push!(edges, Edge(eval(e))), $relations)
        const $name = (HeytingAlgebra(domain, edges))
    end |> esc
end

Graphs.Edge(t::Tuple{HeytingTruth, HeytingTruth}) = Edge(index(t[1]), index(t[2]))
Graphs.Edge(t::Tuple{HeytingTruth, BooleanTruth}) = Edge((t[1], convert(HeytingTruth, t[2])))
Graphs.Edge(t::Tuple{BooleanTruth, HeytingTruth}) = Edge((convert(HeytingTruth, t[1]), t[2]))
Graphs.Edge(t::Tuple{BooleanTruth, BooleanTruth}) = Edge((convert(HeytingTruth, t[1]), convert(HeytingTruth, t[2])))

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
precedes(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth) = precedes(h, α, convert(HeytingTruth, β))
precedes(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth) = precedes(h, convert(HeytingTruth, α), β)
precedes(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth) = precedes(h, convert(HeytingTruth, α), convert(HeytingTruth, β))

"""
β ≺ α
"""
succeedes(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = precedes(h, β, α)
succeedes(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth) = succeedes(h, α, convert(HeytingTruth, β))
succeedes(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth) = succeedes(h, convert(HeytingTruth, α), β)
succeedes(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth) = succeedes(h, convert(HeytingTruth, α), convert(HeytingTruth, β))

"""
α ⪯ β
"""
precedeq(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = α == β ||  precedes(h, α, β)
precedeq(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth) = precedeq(h, α, convert(HeytingTruth, β))
precedeq(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth) = precedeq(h, convert(HeytingTruth, α), β)
precedeq(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth) = precedeq(h, convert(HeytingTruth, α), convert(HeytingTruth, β))

"""
β ⪯ α
"""
succeedeq(h::HeytingAlgebra, α::HeytingTruth, β::HeytingTruth) = α == β ||  succeedes(h, α, β)
succeedeq(h::HeytingAlgebra, α::HeytingTruth, β::BooleanTruth) = succeedeq(h, α, convert(HeytingTruth, β))
succeedeq(h::HeytingAlgebra, α::BooleanTruth, β::HeytingTruth) = succeedeq(h, convert(HeytingTruth, α), β)
succeedeq(h::HeytingAlgebra, α::BooleanTruth, β::BooleanTruth) = succeedeq(h, convert(HeytingTruth, α), convert(HeytingTruth, β))

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

collatetruth(c::Connective, (α, β)::Tuple{HeytingTruth, BooleanTruth}, h::HeytingAlgebra) = collatetruth(c, (α, convert(HeytingTruth, β)), h)
collatetruth(c::Connective, (α, β)::Tuple{BooleanTruth, HeytingTruth}, h::HeytingAlgebra) = collatetruth(c, (convert(HeytingTruth, α), β), h)
collatetruth(c::Connective, (α, β)::Tuple{BooleanTruth, BooleanTruth}, h::HeytingAlgebra) = collatetruth(c, (convert(HeytingTruth, α), convert(HeytingTruth, β)), h)
