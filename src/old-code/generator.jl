######################
#       Models       #
#     generation     #
######################

# https://hal.archives-ouvertes.fr/hal-00471255v2/document

# Erdos-Rényi method
# Create a graph as an adjacency matrix by randomling
# sampling (probability p) the edges between n nodes.
# Convert the same graph to an adjacency list and return it.
function gnp(n::Integer, p::Float64; rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG)
    M = _gnp(n, p, rng)

    worlds = Worlds([Point(i) for i = 1:n])
    adjs = Adjacents{Point}()

    # Left triangular matrix is used to generate an adjacency list
    for i = 1:n
        neighbors = Worlds{Point}([])
        for j = 1:i
            if M[i, j] == 1
                push!(neighbors.worlds, worlds[j])
            end
        end
        setindex!(adjs, neighbors, worlds[i])
    end

    return adjs
end

function _gnp(n::Integer, p::Real, rng::AbstractRNG)
    M = zeros(Int8, n, n)

    for i = 1:n, j = 1:i
        if rand(rng) < p
            M[i, j] = 1
        end
    end

    return M
end

# Fan-in/Fan-out method
# Create a graph with n nodes as an adjacency list and return it.
# It's possible to set a global maximum to input_degree and output_degree.
# Also it's possible to choose how likely a certain "phase" will happen
# 1) _fanout increases a certain node's output_degree grow by spawning new vertices
# 2) _fanin increases the input_degree of a certain group of nodes
#    by linking a single new vertices to all of them
function fanfan(
    n::Integer,
    id::Integer,
    od::Integer;
    threshold::Float64 = 0.5,
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
)
    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng
    adjs = Adjacents{Point}()
    setindex!(adjs, Worlds{Point}([]), Point(0))  # Ecco qua ad esempio metti un GenericWorld

    od_queue = PriorityQueue{Point,Int64}(Point(0) => 0)

    while length(adjs.adjacents) <= n
        if rand(rng) <= threshold
            _fanout(adjs, od_queue, od, rng)
        else
            _fanin(adjs, od_queue, id, od, rng)
        end
    end

    return adjs
end

function _fanout(
    adjs::Adjacents{Point},
    od_queue::PriorityQueue{Point,Int},
    od::Integer,
    rng::AbstractRNG,
)
    #=
    Find the vertex v with the biggest difference between its out-degree and od.
    Create a random number of vertices between 1 and (od-m)
    and add edges from v to these new vertices.
    =#
    v, m = peek(od_queue)

    for i in rand(rng, 1:(od-m))
        new_node = Point(length(adjs))
        setindex!(adjs, Worlds{Point}([]), new_node)
        push!(adjs, v, new_node)

        od_queue[new_node] = 0
        od_queue[v] = od_queue[v] + 1
    end
end

function _fanin(
    adjs::Adjacents{Point},
    od_queue::PriorityQueue{Point,Int},
    id::Integer,
    od::Integer,
    rng::AbstractRNG,
)
    #=
    Find the set S of all vertices that have out-degree < od.
    Compute a subset T of S of size at most id.
    Add a new vertex v and add new edges (v, t) for all t ∈ T
    =#
    S = filter(x -> x[2] < od, od_queue)
    T = Set(sample(collect(S), rand(rng, 1:min(id, length(S))), replace = false))

    v = Point(length(adjs))
    for t in T
        setindex!(adjs, Worlds{Point}([]), v)
        push!(adjs, t[1], v)

        od_queue[t[1]] = od_queue[t[1]] + 1
        od_queue[v] = 0
    end
end

# Associate each world to a subset of proposistional letters
function dispense_alphabet(
    ws::Worlds{T};
    P::LetterAlphabet = SoleLogics.alphabet(MODAL_LOGIC),
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
) where {T<:AbstractWorld}
    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng
    evals = Dict{T,LetterAlphabet}()
    for w in ws
        evals[w] = sample(P, rand(rng, 0:length(P)), replace = false)
    end
    return evals
end

# NOTE: read the other gen_kmodel dispatch below as it's signature is more flexible.
# Generate and return a kripke model.
# This utility uses `fanfan` and `dispense_alphabet` default methods
# to define `adjacents` and `evaluations` but one could create its model
# piece by piece and then calling KripkeStructure constructor.
function gen_kmodel(
    n::Integer,
    in_degree::Integer,   # needed by fanfan
    out_degree::Integer;  # needed by fanfan
    P::LetterAlphabet = SoleLogics.alphabet(MODAL_LOGIC),
    threshold = 0.5,      # needed by fanfan
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
)
    rng = (typeof(rng) <: Integer) ? Random.MersenneTwister(rng) : rng
    ws = Worlds{Point}(world_gen(n))
    adjs = fanfan(n, in_degree, out_degree, threshold = threshold, rng = rng)
    evs = dispense_alphabet(ws, P = P, rng = rng)
    return KripkeStructure{Point}(ws, adjs, evs)
end

# Generate and return a kripke model.
# Example of valid calls:
# gen_kmodel(15, MODAL_LOGIC, :erdos_renyi, 0.42)
# gen_kmodel(10, MODAL_LOGIC, :fanin_fanout, 3, 4)
#
# NOTE:
# This function is a bit tricky as in kwargs (that is, the arguments of the selected method)
# n has to be excluded (in fact it is already the first argument)
# In other words this dispatch is not compatible with graph-generation functions whose
# signature differs from fx(n, other_arguments...)
function gen_kmodel(n::Integer, P::LetterAlphabet, method::Symbol, kwargs...)
    if method == :fanin_fanout
        fx = fanfan
    elseif method == :erdos_renyi
        fx = gnp
    else
        error("Invalid method provided: $method. Refer to the docs <add link here>")
    end

    ws = Worlds{Point}(world_gen(n))
    adjs = fx(n, kwargs...)
    evs = dispense_alphabet(ws, P = P)
    return KripkeStructure{Point}(ws, adjs, evs)
end
