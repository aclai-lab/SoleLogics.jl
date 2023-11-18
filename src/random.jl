using Random
using StatsBase

import Random: rand
import StatsBase: sample

#= ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Formulas ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ =#

# WIP by Mauro
# TODO: add these new methods for rand formula generation
# rand(connectives, atom leaves array, algebra from which infer truth values)
# rand(connectives, atom leaves array, truth values with common supertype)
# rand(connectives, atom leaves array, true/false (use truth values as leaf or not. If true, default to boolean))
# sample(..., probability distribution)

doc_rand = """
    Base.rand(
        [rng::AbstractRNG = Random.GLOBAL_RNG,]
        alphabet::AbstractAlphabet,
        args...;
        kwargs...
    )::Atom

    Base.rand(
        [rng::AbstractRNG = Random.GLOBAL_RNG,]
        height::Integer,
        g::CompleteFlatGrammar,
        args...
    )

    Base.rand(
        height::Integer,
        connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
        atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
        truthvalues::Union{Nothing,AbstractVector{<:Truth},AbstractAlgebra} = nothing,
        args...;
        rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
        kwargs...
    )

Randomly sample an atom from an `alphabet`, according to a uniform distribution.

# Implementation
If the `alphabet` is finite, the function defaults to `rand(rng, atoms(alphabet))`;
otherwise, it must be implemented, and additional keyword arguments should be provided
in order to limit the (otherwise infinite) sampling domain.

See also
[`AbstractAlphabet`](@ref).
"""

"""$(doc_rand)"""
function Base.rand(alphabet::AbstractAlphabet, args...; kwargs...)
    Base.rand(Random.GLOBAL_RNG, alphabet, args...; kwargs...)
end

function Base.rand(
    rng::AbstractRNG,
    alphabet::AbstractAlphabet,
    args...;
    kwargs...
)
    if isfinite(alphabet)
        Base.rand(rng, atoms(alphabet), args...; kwargs...)
    else
        error("Please, provide method Base.rand(rng::AbstractRNG, " *
            "alphabet::$(typeof(alphabet)), args...; kwargs...).")
    end
end


# For the case of a CompleteFlatGrammar, the alphabet and the operators suffice.
"""$(doc_rand)"""
function Base.rand(
    height::Integer,
    g::CompleteFlatGrammar,
    args...
)
    Base.rand(Random.GLOBAL_RNG, height, g, args...)
end

function Base.rand(
    rng::AbstractRNG,
    height::Integer,
    g::CompleteFlatGrammar,
    args...;
    kwargs...
)
    randformula(rng, height, alphabet(g), operators(g), args...; kwargs...)
end

"""$(doc_rand)"""
function Base.rand(
    height::Integer,    # By Mauro - to generate a random formula, height has to be known
    connectives::Union{AbstractVector{<:Operator},AbstractVector{<:Connective}},
    atoms::Union{AbstractVector{<:Atom},AbstractAlphabet},
    truthvalues::Union{Nothing,AbstractVector{<:Truth},AbstractAlgebra} = nothing,
    args...;
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
    kwargs...
)
    # If Truth's are specified as `operators`, then they cannot be simultaneously
    #  provided as `truthvalues`
    @assert (connectives isa AbstractVector{<:Connective} ||
            !(truthvalues isa AbstractVector{<:Truth})
        ) "Unexpected connectives and truth values: $(connectives) and $(truthvalues)."

    atoms = atoms isa AbstractAlphabet ? SoleLogics.atoms(atoms) : atoms
    ops = connectives
    if !isnothing(truthvalues)
        truthvalues = truthvalues isa AbstractAlgebra ? domain(truthvalues) : truthvalues
        @assert typejoin(typeof.(truthvalues)...) != Truth "Truth values " *
            "$(truthvalues) must belong to the same algebra " *
            "(and have a common supertype that is not Truth)."
        ops = vcat(ops, truthvalues)
    end

    randformula(height, ops, atoms, args...; rng=rng, kwargs...)
end

function StatsBase.sample(
    alphabet::AbstractAlphabet,
    weights::AbstractWeights,
    args...;
    kwargs...
)
    StatsBase.sample(Random.GLOBAL_RNG, alphabet, weights, args...; kwargs...)
end

function StatsBase.sample(
    rng::AbstractRNG,
    alphabet::AbstractAlphabet,
    weights::AbstractWeights,
    args...;
    kwargs...
)
    if isfinite(alphabet)
        StatsBase.sample(rng, atoms(alphabet), weights, args...; kwargs...)
    else
        error("Please, provide method StatsBase.sample(rng::AbstractRNG, " *
            "alphabet::$(typeof(alphabet)), args...; kwargs...).")
    end
end

function StatsBase.sample(l::AbstractLogic, weights::AbstractWeights, args...; kwargs...)
    StatsBase.sample(Random.GLOBAL_RNG, l, weights, args...; kwargs...)
end

function StatsBase.sample(
    rng::AbstractRNG,
    l::AbstractLogic,
    weights::AbstractWeights,
    args...;
    kwargs...
)
    StatsBase.sample(rng, grammar(l), weights, args...; kwargs...)
end

doc_sample = """
    StatsBase.sample(
        [rng::AbstractRNG = Random.GLOBAL_RNG, ]
        g::AbstractGrammar,
        height::Integer,
        args...;
        kwargs...
    )::Formula

    StatsBase.sample(height::Integer, g::AbstractGrammar, args...; kwargs...)

Randomly sample a logic formula of given `height` from a grammar `g`.

# Implementation
This method for must be implemented, and additional keyword arguments should be provided
in order to limit the (otherwise infinite) sampling domain.

See also
[`AbstractAlphabet`](@ref).
"""

"""$(doc_sample)"""
function StatsBase.sample(
    rng::AbstractRNG,
    height::Integer,
    g::AbstractGrammar,
    opweights::Union{Nothing,AbstractWeights} = nothing,
    args...;
    kwargs...
)
    randformula(
        rng, height, alphabet(g), operators(g), args...;
        # atompicker=(rng,dom)->StatsBase.sample(rng, dom, atomweights), kwargs...)
        opweights = opweights, kwargs...)
end

"""$(doc_sample)"""
function StatsBase.sample(
    height::Integer,
    g::AbstractGrammar,
    args...;
    kwargs...
)
    StatsBase.sample(Random.GLOBAL_RNG, height, g, args...; kwargs...)
end

#= ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CompleteFlatGrammar ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ =#

# TODO
# - make rng first (optional) argument of randformula (see above)
# - in randformula, keyword argument alphabet_sample_kwargs that are unpacked upon sampling atoms, as in: Base.rand(rng, a; alphabet_sample_kwargs...). This would allow to sample from infinite alphabets, so when this parameter, !isfinite(alphabet) is allowed!

doc_randformula = """
    randformula(
        height::Integer,
        alphabet,
        operators::AbstractVector;
        rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
    )::SyntaxTree

    # TODO @Mauro implement this method.
    function randformula(
        height::Integer,
        g::AbstractGrammar;
        rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG
    )::SyntaxTree

Return a pseudo-randomic `SyntaxBranch`.

# Arguments
- `height::Integer`: height of the generated structure;
- `alphabet::AbstractAlphabet`: collection from which atoms are chosen randomly;
- `operators::AbstractVector`: vector from which legal operators are chosen;
- `g::AbstractGrammar`: alternative to passing alphabet and operators separately. (TODO explain?)

# Keyword Arguments
- `rng::Union{Intger,AbstractRNG} = Random.GLOBAL_RNG`: random number generator;
- `atompicker::Function` = method used to pick a random element. For example, this could be
    Base.rand or StatsBase.sample.
- `opweights::AbstractWeights` = weight vector over the set of operators (see `StatsBase`).

# Examples

```julia-repl
julia> syntaxstring(randformula(4, ExplicitAlphabet([1,2]), [NEGATION, CONJUNCTION, IMPLICATION]))
"¬((¬(¬(2))) → ((1 → 2) → (1 → 2)))"
```

See also [`AbstractAlphabet`](@ref), [`SyntaxBranch`](@ref).
"""

"""$(doc_randformula)"""
function randformula(
    rng::AbstractRNG,
    height::Integer,
    alphabet,
    operators::AbstractVector;
    modaldepth::Integer = height,
    atompicker::Union{Function,AbstractWeights,AbstractVector{<:Real},Nothing} = sample,
    opweights::Union{AbstractWeights,AbstractVector{<:Real},Nothing} = nothing,
)::SyntaxTree
    alphabet = convert(AbstractAlphabet, alphabet)

    @assert all(x->x isa Operator, operators) "Unexpected object(s) passed as" *
        " operator:" * " $(filter(x->!(x isa Operator), operators))"

    if (isnothing(opweights))
        opweights = StatsBase.uweights(length(operators))
    elseif (opweights isa AbstractVector)
        @assert length(opweights) == length(operators) "Mismatching numbers of operators " *
                "($(length(operators))) and opweights ($(length(opweights)))."
        opweights = StatsBase.weights(opweights)
    end

    if (isnothing(atompicker))
        atompicker = StatsBase.uweights(length(alphabet))
    elseif (atompicker isa AbstractVector)
        @assert length(atompicker) == length(alphabet) "Mismatching numbers of atoms " *
                "($(length(alphabet))) and atompicker ($(length(atompicker)))."
        atompicker = StatsBase.weights(atompicker)
    end

    if !(atompicker isa Function)
        atomweights = atompicker
        atompicker = (rng, dom)->StatsBase.sample(rng, dom, atomweights)
    end

    nonmodal_operators = findall(!ismodal, operators)

    function _randformula(
        rng::AbstractRNG,
        height::Integer,
        modaldepth::Integer
    )::SyntaxTree
        if height == 0
            # Sample atom from alphabet
            return atompicker(rng, atoms(alphabet))
        else
            # Sample operator and generate children (modal connectives only if modaldepth > 0)
            ops, ops_w = begin
                if modaldepth > 0
                    operators, opweights
                else
                    operators[nonmodal_operators], opweights[nonmodal_operators]
                end
            end

            # op = rand(rng, ops)
            op = sample(rng, ops, ops_w)
            ch = Tuple([
                    _randformula(rng, height-1, modaldepth-(ismodal(op) ? 1 : 0))
                    for _ in 1:arity(op)])
            return SyntaxTree(op, ch)
        end
    end

    # If the alphabet is not iterable, this function should not work.
    if !isfinite(alphabet)
        @warn "Attempting to generate random formulas from " *
            "(infinite) alphabet of type $(typeof(alphabet))!"
    end

    return _randformula(rng, height, modaldepth)
end

function randformula(
    rng::AbstractRNG,
    height::Integer,
    g::AbstractGrammar,
    args...;
    kwargs...
)
    initrng(rng)
    randformula(rng, height, alphabet(g), operators(g), args...; kwargs...)
end

# Helper
function randformula(
    height::Integer,
    args...;
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
    kwargs...
)
    initrng(rng)
    randformula(rng, height, args...; kwargs...)
end

#= ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Kripke Structures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ =#

# function fanfan()
# end

# function _fanout()
# end

# function _fanin()
# end

# function dispense_alphabet()
# end

# function sample_worlds()
# end

# function generate_kripke_frame(
#     n::Integer
# )
#     # ws = Vector{AbstractWorld}([SoleLogics.World(i) for i in 1:n])
# end

# #= Deprecated overlay code

# https://hal.archives-ouvertes.fr/hal-00471255v2/document

# # Fan-in/Fan-out method
# # Create a graph with n nodes as an adjacency list and return it.
# # It's possible to set a global maximum to input_degree and output_degree.
# # Also it's possible to choose how likely a certain "phase" will happen
# # 1) _fanout increases a certain node's output_degree grow by spawning new vertices
# # 2) _fanin increases the input_degree of a certain group of nodes
# #    by linking a single new vertices to all of them
# function fanfan(
#     n::Integer,
#     id::Integer,
#     od::Integer;
#     threshold::Float64 = 0.5,
#     rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
# )
#     rng = (rng isa AbstractRNG) ? rng : Random.MersenneTwister(rng)
#     adjs = Adjacents{PointWorld}()
#     setindex!(adjs, Worlds{PointWorld}([]), PointWorld(0))  # Ecco qua ad esempio metti un GenericWorld

#     od_queue = PriorityQueue{PointWorld,Int64}(PointWorld(0) => 0)

#     while length(adjs.adjacents) <= n
#         if rand(rng) <= threshold
#             _fanout(adjs, od_queue, od, rng)
#         else
#             _fanin(adjs, od_queue, id, od, rng)
#         end
#     end

#     return adjs
# end

# function _fanout(
#     adjs::Adjacents{PointWorld},
#     od_queue::PriorityQueue{PointWorld,Int},
#     od::Integer,
#     rng::AbstractRNG,
# )
#
#     # Find the vertex v with the biggest difference between its out-degree and od.
#     # Create a random number of vertices between 1 and (od-m)
#     # and add edges from v to these new vertices.
#
#     v, m = peek(od_queue)

#     for i in rand(rng, 1:(od-m))
#         new_node = PointWorld(length(adjs))
#         setindex!(adjs, Worlds{PointWorld}([]), new_node)
#         push!(adjs, v, new_node)

#         od_queue[new_node] = 0
#         od_queue[v] = od_queue[v] + 1
#     end
# end

# function _fanin(
#     adjs::Adjacents{PointWorld},
#     od_queue::PriorityQueue{PointWorld,Int},
#     id::Integer,
#     od::Integer,
#     rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
# )
#     rng = (rng isa AbstractRNG) ? rng : Random.MersenneTwister(rng)
#     #=
#     Find the set S of all vertices that have out-degree < od.
#     Compute a subset T of S of size at most id.
#     Add a new vertex v and add new edges (v, t) for all t ∈ T
#     =#
#     S = filter(x -> x[2] < od, od_queue)
#     T = Set(sample(collect(S), rand(rng, 1:min(id, length(S))), replace = false))

#     v = PointWorld(length(adjs))
#     for t in T
#         setindex!(adjs, Worlds{PointWorld}([]), v)
#         push!(adjs, t[1], v)

#         od_queue[t[1]] = od_queue[t[1]] + 1
#         od_queue[v] = 0
#     end
# end

# # Associate each world to a subset of proposistional letters
# function dispense_alphabet(
#     ws::Worlds{T};
#     P::LetterAlphabet = SoleLogics.alphabet(MODAL_LOGIC),
#     rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
# ) where {T<:AbstractWorld}
#     rng = (rng isa AbstractRNG) ? rng : Random.MersenneTwister(rng)
#     evals = Dict{T,LetterAlphabet}()
#     for w in ws
#         evals[w] = sample(P, rand(rng, 0:length(P)), replace = false)
#     end
#     return evals
# end

# # NOTE: read the other gen_kmodel dispatch below as it's signature is more flexible.
# # Generate and return a Kripke structure.
# # This utility uses `fanfan` and `dispense_alphabet` default methods
# # to define `adjacents` and `evaluations` but one could create its model
# # piece by piece and then calling KripkeStructure constructor.
# function gen_kmodel(
#     n::Integer,
#     in_degree::Integer,   # needed by fanfan
#     out_degree::Integer;  # needed by fanfan
#     P::LetterAlphabet = SoleLogics.alphabet(MODAL_LOGIC),
#     threshold = 0.5,      # needed by fanfan
#     rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
# )
#     rng = (rng isa AbstractRNG) ? rng : Random.MersenneTwister(rng)
#     ws = Worlds{PointWorld}(world_gen(n))
#     adjs = fanfan(n, in_degree, out_degree, threshold = threshold, rng = rng)
#     evs = dispense_alphabet(ws, P = P, rng = rng)
#     return KripkeStructure(ws, adjs, evs)
# end

# # Generate and return a Kripke structure.
# # Example of valid calls:
# # gen_kmodel(15, MODAL_LOGIC, :erdos_renyi, 0.42)
# # gen_kmodel(10, MODAL_LOGIC, :fanin_fanout, 3, 4)
# #
# # NOTE:
# # This function is a bit tricky as in kwargs (that is, the arguments of the selected method)
# # n has to be excluded (in fact it is already the first argument)
# # In other words this dispatch is not compatible with graph-generation functions whose
# # signature differs from fx(n, other_arguments...)
# function gen_kmodel(n::Integer, P::LetterAlphabet, method::Symbol, kwargs...)
#     if method == :fanin_fanout
#         fx = fanfan
#     elseif method == :erdos_renyi
#         fx = gnp
#     else
#         error("Invalid method provided: $method. Refer to the docs <add link here>")
#     end

#     ws = Worlds{PointWorld}(world_gen(n))
#     adjs = fx(n, kwargs...)
#     evs = dispense_alphabet(ws, P = P)
#     return KripkeStructure(ws, adjs, evs)
# end

# =#
