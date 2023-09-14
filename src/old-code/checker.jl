#################################
#        Kripke Structure       #
#           components          #
#################################

using SoleLogics.Worlds

# Note: overwriting model name
const Worlds = Array{AbstractWorld,1}

# Adjacents is the simplest type of relation-collection
struct Adjacents{T<:AbstractWorld} <: AbstractDict{T,Worlds}
    adjacents::Dict{T,Worlds{T}}

    function Adjacents{T}() where {T<:AbstractWorld}
        return new{T}(Dict{T,Worlds{T}}())
    end

    function Adjacents{T}(adjacents::Dict{T,Worlds{T}}) where {T<:AbstractWorld}
        return new{T}(adjacents)
    end
end
Base.iterate(adj::Adjacents, state = 1) =
    state < length(adj.adjacents) ? Base.iterate(adj, state + 1) : nothing
Base.keys(adj::Adjacents) = keys(adj.adjacents)
Base.values(adj::Adjacents) = values(adj.adjacents)
Base.length(adj::Adjacents) = length(adj.adjacents)

Base.isassigned(adj::Adjacents, w::AbstractWorld) = (w in adj.adjacents)
Base.getindex(adj::Adjacents, key::AbstractWorld) = adj.adjacents[key]
Base.setindex!(adj::Adjacents, value::Worlds, key::AbstractWorld) =
    adj.adjacents[key] = value
Base.setindex!(adj::Adjacents, value::AbstractWorld, key::AbstractWorld) =
    adj.adjacents[key] = Worlds([value])

Base.push!(adj::Adjacents, key::AbstractWorld, value::AbstractWorld) =
    push!(adj.adjacents[key].worlds, value)
Base.push!(adj::Adjacents, key::AbstractWorld, value::Worlds) =
    push!(adj.adjacents[key].worlds, value...)

Base.print(io::IO, adj::Adjacents) = print(adj.adjacents)
Base.show(io::IO, adj::Adjacents) = show(adj.adjacents)

# TODO: may be useful to define a common interface for different/"similar" Memo types
#
# e.g at the moment memoization value-type can be switched between Set and Vector as
# `contains`, `push!` and all the operators custom dispatching does support those types.
# This flexibility could be further extended
const WorldsSet{T<:AbstractWorld} = Set{T}  # Write this in SoleWorlds, near Worlds wrapper
const MemoValueType{T} = WorldsSet{T}
const Memo{T} = Dict{UInt64,MemoValueType{T}}
# const MemoValue{T} = Worlds{T}            <- a possible working alternative
# const Memo{T} = Dict{Integer, Worlds{T}}  <-

struct KripkeStructure{T<:AbstractWorld}
    worlds::Worlds{T}                    # worlds in the model
    adjacents::Adjacents{T}              # neighbors of a given world
    evaluations::Dict{T,LetterAlphabet} # list of prop. letters satisfied by a world
    logic::AbstractLogic                 # logic associated with this model
    L::Memo{T}                           # memoization collection associated with this model

    function KripkeStructure{T}() where {T<:AbstractWorld}
        worlds = Worlds{T}([])
        adjacents = Dict{T,Worlds{T}}([])
        evaluations = Dict{T,Vector{String}}()
        logic = SoleLogics.MODAL_LOGIC
        L = Memo{T}()
        return new{T}(worlds, adjacents, evaluations, L, logic)
    end

    function KripkeStructure{T}(
        worlds::Worlds{T},
        adjacents::Adjacents{T},
        evaluations::Dict{T,Vector{String}},
    ) where {T<:AbstractWorld}
        logic = SoleLogics.MODAL_LOGIC
        L = Memo{T}()
        return new{T}(worlds, adjacents, evaluations, logic, L)
    end
end
worlds(km::KripkeStructure) = km.worlds
worlds!(km::KripkeStructure, ws::Worlds{T}) where {T<:AbstractWorld} = worlds(km) = ws

adjacents(km::KripkeStructure) = km.adjacents
adjacents(km::KripkeStructure, w::AbstractWorld) = km.adjacents[w]
adjacents!(km::KripkeStructure, adjs::Adjacents{T}) where {T<:AbstractWorld} =
    adjacents(km) = adjs

evaluations(km::KripkeStructure) = km.evaluations
evaluations(km::KripkeStructure, w::AbstractWorld) = km.evaluations[w]
evaluations!(km::KripkeStructure, evals::Dict{T,LetterAlphabet}) where {T<:AbstractWorld} =
    evaluations(km) = evals

logic(km::KripkeStructure) = km.logic

Base.eltype(::Type{KripkeStructure{T}}) where {T} = T

########################
#     Memo structure   #
#       utilities      #
########################
memo(km::KripkeStructure) = km.L
memo(km::KripkeStructure, key) = begin
    if haskey(memo(km), key)
        memo(km)[key]
    else
        MemoValueType{eltype(km)}([])
    end
end
memo(km::KripkeStructure, key::AbstractFormula) = memo(km, fhash(key.tree))

# This setter is dangerous as it doesn't check if key exists in the memo structure
# memo!(km::KripkeStructure, key::Integer, val::MemoValueType) = km.L[key] = val # memo(km, key) = val

# Check if memoization structure does contain a certain value, considering a certain key
contains(km::KripkeStructure, key, value::AbstractWorld) = begin
    (!haskey(memo(km), key) || !(value in memo(km, key))) ? false : true
end

# If I try to insert in a non-allocated memory place,
# i first reserve space to this new key.
Base.push!(km::KripkeStructure, key, val::AbstractWorld) = begin
    if !haskey(memo(km), key)
        setindex!(memo(km), MemoValueType{eltype(km)}([]), key)
    end
    push!(memo(km, key), val)
end

#################################
#         Model Checking        #
#################################
function _check_alphabet(km::KripkeStructure, ψ::FNode)
    # As well as _check_unary and _check_binary, this could be done
    # @assert token(ψ) in SoleLogics.alphabet(logic(km)) "$(token(ψ)) is an invalid letter"
    key = fhash(ψ)

    for w in worlds(km)
        # memo
        if w in memo(km, key)
            continue
            # new entry
        elseif token(ψ) in evaluations(km, w)
            push!(km, key, w)
            # no world
        elseif !haskey(memo(km), key)
            setindex!(memo(km), MemoValueType{eltype(km)}([]), key)
        end
    end
end

function _check_unary(km::KripkeStructure, ψ::FNode)
    @assert token(ψ) in SoleLogics.operators(logic(km)) "Error - $(token(ψ)) is an invalid token"
    key = fhash(ψ)

    # Result is already computed
    if haskey(memo(km), key)
        return
    else
        setindex!(memo(km), MemoValueType{eltype(km)}([]), key)
    end

    right_key = fhash(rightchild(ψ))

    # Ad-hoc negation case
    if token(ψ) isa SoleLogics.UnaryOperator{:¬}
        # NOTE: why is casting to MemoValueType needed here?
        setindex!(memo(km), NEGATION(worlds(km), memo(km, right_key)), key)
    elseif is_modal_operator(token(ψ))
        for w in worlds(km)
            # Consider w's neighbors
            if dispatch_modop(token(ψ), km, w, right_key)
                push!(km, key, w)
            end
        end
    else
        error("TODO expand code")
    end
end

function _check_binary(km::KripkeStructure, ψ::FNode)
    @assert token(ψ) in SoleLogics.operators(logic(km)) "Error - $(token(ψ)) is an invalid token"
    key = fhash(ψ)

    # Result is already computed
    if haskey(memo(km), key)
        return
    end

    ch = children(ψ)

    # Implication case is ad-hoc as it needs to know the
    # allworlds were the two operands are placed
    setindex!(memo(km), token(ψ)(worlds(km), memo(km, left_key), memo(km, right_key)), key)
end

function _process_node(km::KripkeStructure, ψ::FNode)
    if is_atom(token(ψ))
        _check_alphabet(km, ψ)
    elseif is_unary_operator(token(ψ))
        _check_unary(km, ψ)
    elseif is_binary_operator(token(ψ))
        _check_binary(km, ψ)
    end
end

function check(
    km::KripkeStructure{T},
    fx::SoleLogics.Formula{L};
    max_fheight_memo = Inf
) where {T<:AbstractWorld,L<:AbstractLogic}
    forget_list = Vector{SoleLogics.FNode}()

    if !haskey(memo(km), fhash(fx.tree))
        for ψ in subformulas(fx.tree)
            if height(ψ) > max_fheight_memo
                push!(forget_list, ψ)
            end

            if !haskey(memo(km), fhash(ψ))
                _process_node(km, ψ)
            end
        end
    end

    # All the worlds where a given formula is valid are returned.
    # Then, internally, memoization-regulation is applied
    # to forget some formula thus freeing space.
    fcollection = deepcopy(memo(km))
    for h in forget_list
        k = fhash(h)
        if haskey(memo(km), k)
            empty!(memo(km, k)) # Collection at memo(km)[k] is erased
            pop!(memo(km), k)    # Key k is deallocated too
        end
    end

    return fcollection
end

function check(
    𝑀::Vector{KripkeStructure{T}},
    Φ::Vector{SoleLogics.Formula{L}};
    max_fheight_memo = Inf,
) where {T<:AbstractWorld,L<:AbstractLogic}
    for km in 𝑀
        for φ in Φ
            check(km, φ, max_fheight_memo = max_fheight_memo)
        end
    end
end

# This overload also returns a matrix 𝑀 x Φ, containing whether
# a certain formula φᵢ is satisfied on 𝑚's initial world
# considering a certain max-memoization threshold.
function check(
    𝑀::Vector{KripkeStructure{T}},
    Φ::Vector{SoleLogics.Formula{L}},
    iw::T;
    max_fheight_memo = Inf,
) where {T<:AbstractWorld,L<:AbstractLogic}
    outcomes = Matrix{Bool}(undef, length(𝑀), length(Φ))

    for 𝑚 in eachindex(𝑀)
        for φ in eachindex(Φ)
            outcomes[𝑚, φ] = (
                iw in check(𝑀[𝑚], Φ[φ], max_fheight_memo = max_fheight_memo)[fhash(
                    Φ[φ].tree,
                )]
            )
        end
    end

    return outcomes
end

"""
    subformulas(root::FNode, sorted=true)
Return each `FNode` in a tree, sorting them by size.
"""
function subformulas(root::FNode{L}; sorted=true) where {L<:AbstractLogic}
    
    function _subformulas(v::FNode{L}, nodes::Vector{FNode{L}}) where {L<:AbstractLogic}
        if isdefined(v, :leftchild)
            _subformulas(v.leftchild, nodes)
        end

        push!(nodes, v)

        if isdefined(v, :rightchild)
            _subformulas(v.rightchild, nodes)
        end
    end

    nodes = FNode{L}[]
    _subformulas(root, nodes)
    if sorted
        sort!(nodes, by = n -> SoleLogics.size(n))
    end
    return nodes
end
