import Base: show
using DataStructures: OrderedDict
using Graphs
using ThreadSafeDicts

"""
    abstract type AbstractWorld end

Abstract type for the nodes of an annotated accessibility graph (Kripke structure).
This is used, for example, in modal logic, where the truth of
formulas is relativized to *worlds*, that is, nodes of a graph.

# Implementing

When implementing a new world type, the logical semantics
should be defined via `accessibles` methods; refer to the help for `accessibles`.

See also [`AbstractKripkeStructure`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractWorld end

# Base.show(io::IO, w::AbstractWorld) = print(io, inlinedisplay(w))

############################################################################################

"""
    abstract type AbstractFrame{W<:AbstractWorld} end

Abstract type for an accessibility graph (Kripke frame), that gives the topology to
[Kripke structures](https://en.wikipedia.org/wiki/Kripke_structure_(model_checking)).
A frame can be queried for its set of vertices (also called *worlds*,
see [`allworlds`](@ref)), and it can be browsed via its accessibility
relation(s) (see [`accessibles`](@ref)). Refer to [`FullDimensionalFrame`](@ref) as an
example.

See also [`truthtype`](@ref), ,
[`allworlds`](@ref), [`nworlds`](@ref),
[`AbstractKripkeStructure`](@ref),
[`AbstractWorld`](@ref).
"""
abstract type AbstractFrame{W<:AbstractWorld} end

"""
    worldtype(fr::AbstractFrame)
    worldtype(i::AbstractKripkeStructure)

Return the world type of the Kripke frame/structure.

See also [`AbstractFrame`](@ref).
"""
worldtype(::Type{<:AbstractFrame{W}}) where {W<:AbstractWorld} = W
worldtype(fr::AbstractFrame) = worldtype(typeof(fr))

"""
    allworlds(fr::AbstractFrame{W})::AbstractVector{<:W} where {W<:AbstractWorld}

Return all worlds within the frame.

See also [`nworlds`](@ref), [`AbstractFrame`](@ref).
"""
function allworlds(fr::AbstractFrame{W})::AbstractVector{<:W} where {W<:AbstractWorld}
    return error("Please, provide method allworlds(frame::$(typeof(fr))).")
end

"""
    nworlds(fr::AbstractFrame)::Integer

Return the number of worlds within the frame.

See also [`nworlds`](@ref), [`AbstractFrame`](@ref).
"""
function nworlds(fr::AbstractFrame)::Integer
    return error("Please, provide method nworlds(frame::$(typeof(fr))).")
end

############################################################################################
##################################### Uni-modal logic ######################################
############################################################################################

"""
    abstract type AbstractUniModalFrame{W<:AbstractWorld} <: AbstractFrame{W} end

A frame of a modal logic based on a single (implicit) accessibility relation.

See also [`AbstractMultiModalFrame`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractUniModalFrame{W<:AbstractWorld} <: AbstractFrame{W} end

"""
    accessibles(fr::AbstractUniModalFrame{W}, w::W)::Worlds{W} where {W<:AbstractWorld}

Return the worlds in frame `fr` that are accessible from world `w`.

See also [`AbstractWorld`](@ref), [`AbstractUniModalFrame`](@ref).
"""
function accessibles(fr::AbstractUniModalFrame{W}, w::W)::Worlds{W} where {W<:AbstractWorld}
    return error("Please, provide method accessibles(fr::$(typeof(f)), w::$(typeof(w)))::Vector{$(W)}.")
end

############################################################################################
#### Multi-modal logic #####################################################################
############################################################################################

"""
    abstract type AbstractRelation end

Abstract type for the relations of a multi-modal
annotated accessibility graph (Kripke structure).
Two noteworthy relations are `identityrel` and `globalrel`, which
access the current world and all worlds, respectively.

# Examples
```julia-repl
julia> fr = SoleLogics.FullDimensionalFrame((10,),);

julia> Interval(8,11) in (accessibles(fr, Interval(2,5), IA_L))
true
```

# Implementation

When implementing a new relation type `R`, please provide the methods:

    arity(::R)::Int = ...
    syntaxstring(::R; kwargs...)::String = ...

If the relation is symmetric, please specify its converse relation `cr` with:

    hasconverse(::R) = true
    converse(::R) = cr

If the relation is many-to-one or one-to-one, please flag it with:

    istoone(::R) = true

If the relation is reflexive or transitive, flag it with:

    isreflexive(::R) = true
    istransitive(::R) = true

Most importantly, the logical semantics for `R` should be defined via `accessibles` methods;
refer to the help for `accessibles`.

See also
[`issymmetric`](@ref),
[`isreflexive`](@ref),
[`istransitive`](@ref),
[`isgrounding`](@ref),
[`arity`](@ref),
[`syntaxstring`](@ref),
[`converse`](@ref),
[`hasconverse`](@ref),
[`istoone`](@ref),
[`identityrel`](@ref),
[`globalrel`](@ref),
[`accessibles`](@ref),
[`AbstractKripkeStructure`](@ref),
[`AbstractFrame`](@ref),
[`AbstractWorld`](@ref).
"""
abstract type AbstractRelation end

"""
    arity(::AbstractRelation)::Integer

Return the `arity` of the relation.

See also [`AbstractRelation`](@ref).
"""
arity(r::AbstractRelation)::Integer = error("Please, provide method arity(::$(typeof(r))).")

function syntaxstring(r::AbstractRelation; kwargs...)::String
    return error("Please, provide method syntaxstring(::$(typeof(r)); kwargs...).")
end

doc_conv_rel = """
    hasconverse(r::AbstractRelation)::Bool
    converse(r::AbstractRelation)::AbstractRelation

If the relation `hasconverse`,
return the converse relation (type) of a given relation (type).

See also [`issymmetric`](@ref), [`isreflexive`](@ref), [`istransitive`](@ref), [`AbstractRelation`](@ref).
"""

"""$(doc_conv_rel)"""
function hasconverse(r::AbstractRelation)::Bool
    return false
end

"""$(doc_conv_rel)"""
function converse(r::AbstractRelation)::AbstractRelation
    return error("Please, provide method converse(::$(typeof(r))).")
end

"""
    istoone(r::AbstractRelation) = false

Return whether it is known that a relation is istoone.

See also [`hasconverse`](@ref), [`converse`](@ref),
[`issymmetric`](@ref), [`istransitive`](@ref), [`isgrounding`](@ref), [`AbstractRelation`](@ref).
"""
istoone(r::AbstractRelation) = false

"""
    issymmetric(r::AbstractRelation) = hasconverse(r) ? converse(r) == r : false

Return whether it is known that a relation is symmetric.

See also [`hasconverse`](@ref), [`converse`](@ref),
[`isreflexive`](@ref), [`istransitive`](@ref), [`isgrounding`](@ref), [`AbstractRelation`](@ref).
"""
issymmetric(r::AbstractRelation) = hasconverse(r) ? converse(r) == r : false

"""
    isreflexive(::AbstractRelation)

Return whether it is known that a relation is reflexive.

See also
[`issymmetric`](@ref), [`istransitive`](@ref), [`isgrounding`](@ref), [`AbstractRelation`](@ref).
"""
isreflexive(::AbstractRelation) = false

"""
    istransitive(::AbstractRelation)

Return whether it is known that a relation is transitive.

See also
[`istoone`](@ref), [`issymmetric`](@ref), [`isgrounding`](@ref), [`AbstractRelation`](@ref).
"""
istransitive(::AbstractRelation) = false

"""
    isgrounding(::AbstractRelation)

Return whether it is known that a relation is grounding.
A relation `R` is grounding if ∀x,z,y R(x,y) ⇔ R(z,y).

See also
[`isreflexive`](@ref), [`issymmetric`](@ref), [`istransitive`](@ref), [`AbstractRelation`](@ref).
"""
isgrounding(::AbstractRelation) = false

############################################################################################
############################################################################################
############################################################################################

"""
    abstract type AbstractMultiModalFrame{W<:AbstractWorld} <: AbstractFrame{W} end

A frame of a multi-modal logic, that is, a modal logic based on a set
of accessibility relations.

# Implementation

When implementing a new multi-modal frame type, the logical semantics for the frame
should be defined via `accessibles` methods; refer to the help for `accessibles`.

See also [`AbstractUniModalFrame`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractMultiModalFrame{W<:AbstractWorld} <: AbstractFrame{W} end

"""
    accessibles(
        fr::AbstractMultiModalFrame{W},
        w::W,
        r::AbstractRelation
    ) where {W<:AbstractWorld}

Return the worlds in frame `fr` that are accessible from world `w` via relation `r`.

# Examples
```julia-repl
julia> fr = SoleLogics.FullDimensionalFrame((10,),);

julia> typeof(accessibles(fr, Interval(2,5), IA_L))
Base.Generator{...}

julia> typeof(accessibles(fr, globalrel))
Base.Generator{...}

julia> @assert SoleLogics.nworlds(fr) == length(collect(accessibles(fr, globalrel)))

julia> typeof(accessibles(fr, Interval(2,5), identityrel))
Vector{Interval{Int64}}

julia> Interval(8,11) in collect(accessibles(fr, Interval(2,5), IA_L))
true
```

# Implementation

Since `accessibles` always returns an iterator of worlds of the same type `W`,
the current implementation of `accessibles` for multi-modal frames delegates the enumeration
to a lower level `_accessibles` function, which returns an iterator of parameter tuples
that are, then, fed to the world constructor the using IterTools generators, as in:

    function accessibles(
        fr::AbstractMultiModalFrame{W},
        w::W,
        r::AbstractRelation,
    ) where {W<:AbstractWorld}
        IterTools.imap(W, _accessibles(fr, w, r))
    end

As such, when defining new frames, worlds, and/or relations, one should provide new methods
for `_accessibles`. For example:

    _accessibles(fr::Full1DFrame, w::Interval{<:Integer}, ::_IA_A) = zip(Iterators.repeated(w.y), w.y+1:X(fr)+1)

This pattern is generally convenient; it can, however, be bypassed,
although this requires defining two additional methods in order to
resolve dispatch ambiguities.
When defining a new frame type `FR{W}`, one can resolve the ambiguities and define
a custom `accessibles` method by providing these three methods:

    # access worlds through relation `r`
    function accessibles(
        fr::FR{W},
        w::W,
        r::AbstractRelation,
    ) where {W<:AbstractWorld}
        ...
    end

    # access current world
    function accessibles(
        fr::FR{W},
        w::W,
        r::IdentityRel,
    ) where {W<:AbstractWorld}
        [w]
    end

    # access all worlds
    function accessibles(
        fr::FR{W},
        w::W,
        r::GlobalRel,
    ) where {W<:AbstractWorld}
        allworlds(fr)
    end

In general, it should be true that
`collect(accessibles(fr, w, r)) isa AbstractWorlds{W}`.

See also [`AbstractWorld`](@ref),
[`AbstractRelation`](@ref), [`AbstractMultiModalFrame`](@ref).
"""
function accessibles(
    fr::AbstractMultiModalFrame,
    w::W,
    r::AbstractRelation
) where {W<:AbstractWorld}
    IterTools.imap(W, _accessibles(fr, w, r))
end

############################################################################################
############################################################################################
############################################################################################

"""
    abstract type AbstractKripkeStructure <: AbstractInterpretation end

Abstract type for representing
[Kripke structures](https://en.wikipedia.org/wiki/Kripke_structure_(model_checking))'s.
It comprehends a directed graph structure (Kripke frame), where nodes are referred to as
*worlds*, and the binary relation between them is referred to as the
*accessibility relation*. Additionally, each world is associated with a mapping from
`Atom`s to `Truth` values.

See also [`frame`](@ref), [`worldtype`](@ref),
[`accessibles`](@ref), [`AbstractInterpretation`](@ref).
"""
abstract type AbstractKripkeStructure <: AbstractInterpretation end

function interpret(
    φ::Truth,
    i::AbstractKripkeStructure,
    w::AbstractWorld,
)::Truth
    return φ
end

function interpret(
    φ::Atom,
    i::AbstractKripkeStructure,
    w::AbstractWorld,
)::SyntaxLeaf
    return error("Please, provide method interpret(::$(typeof(φ)), ::$(typeof(i)), ::$(typeof(w))).")
end

function interpret(
    φ::Formula,
    i::AbstractKripkeStructure,
    w::Union{Nothing,AbstractWorld},
)::Formula
    return error("Please, provide method interpret(::$(typeof(φ)), ::$(typeof(i)), ::$(typeof(w))).")
end

"""
    frame(i::AbstractKripkeStructure)::AbstractFrame

Return the frame of a Kripke structure.

See also [`AbstractFrame`](@ref), [`AbstractKripkeStructure`](@ref).
"""
function frame(i::AbstractKripkeStructure)::AbstractFrame
    return error("Please, provide method frame(i::$(typeof(i))).")
end

worldtype(i::AbstractKripkeStructure) = worldtype(frame(i))
accessibles(i::AbstractKripkeStructure, args...) = accessibles(frame(i), args...)
allworlds(i::AbstractKripkeStructure, args...) = allworlds(frame(i), args...)
nworlds(i::AbstractKripkeStructure) = nworlds(frame(i))

# # General grounding
# function check(
#     φ::SyntaxTree,
#     i::AbstractKripkeStructure;
#     kwargs...
# )
#     if token(φ) isa Union{DiamondRelationalConnective,BoxRelationalConnective}
#         rel = SoleLogics.relation(SoleLogics.token(φ))
#         if rel == tocenterrel
#             checkw(first(children(φ)), i, centralworld(frame(i)); kwargs...)
#         elseif rel == globalrel
#             checkw(first(children(φ)), i, AnyWorld(); kwargs...)
#         elseif isgrounding(rel)
#             checkw(first(children(φ)), i, accessibles(frame(i), rel); kwargs...)
#         else
#             error("Unexpected formula: $φ! Perhaps ")
#         end
#     else
#         # checkw(φ, i, nothing; kwargs...)
#         error("Unexpected formula: $φ! Perhaps ")
#     end
# end

"""
    function check(
        φ::SyntaxTree,
        i::AbstractKripkeStructure,
        w::Union{Nothing,AnyWorld,<:AbstractWorld} = nothing;
        use_memo::Union{Nothing,AbstractDict{<:Formula,<:Vector{<:AbstractWorld}}} = nothing,
        perform_normalization::Bool = true,
        memo_max_height::Union{Nothing,Int} = nothing,
    )::Bool

Check a formula on a specific word in a [`KripkeStructure`](@ref).

# Examples
```julia-repl
julia> using Graphs, Random

julia> @atoms String p q
2-element Vector{Atom{String}}:
 Atom{String}("p")
 Atom{String}("q")

julia> fmodal = randformula(Random.MersenneTwister(14), 3, [p,q], SoleLogics.BASE_MODAL_CONNECTIVES)
¬□(p ∨ q)

# A special graph, called Kripke Frame, is created.
# Nodes are called worlds, and the edges are relations between worlds.
julia> worlds = SoleLogics.World.(1:5) # 5 worlds are created, numerated from 1 to 5

julia> edges = Edge.([(1,2), (1,3), (2,4), (3,4), (3,5)])

julia> kframe = SoleLogics.ExplicitCrispUniModalFrame(worlds, Graphs.SimpleDiGraph(edges))

# A valuation function establishes which fact are true on each world
julia> valuation = Dict([
    worlds[1] => TruthDict([p => true, q => false]),
    worlds[2] => TruthDict([p => true, q => true]),
    worlds[3] => TruthDict([p => true, q => false]),
    worlds[4] => TruthDict([p => false, q => false]),
    worlds[5] => TruthDict([p => false, q => true]),
 ])

# Kripke Frame and valuation function are merged in a Kripke Structure
julia> kstruct = KripkeStructure(kframe, valuation)

julia> [w => check(fmodal, kstruct, w) for w in worlds]
5-element Vector{Pair{SoleLogics.World{Int64}, Bool}}:
 SoleLogics.World{Int64}(1) => 0
 SoleLogics.World{Int64}(2) => 1
 SoleLogics.World{Int64}(3) => 1
 SoleLogics.World{Int64}(4) => 0
 SoleLogics.World{Int64}(5) => 0
```

See also [`SyntaxTree`](@ref), [`AbstractWorld`](@ref), [`KripkeStructure`](@ref).
"""
function check(
    φ::SyntaxTree,
    i::AbstractKripkeStructure,
    w::Union{Nothing,AnyWorld,<:AbstractWorld} = nothing;
    use_memo::Union{Nothing,AbstractDict{<:Formula,<:Vector{<:AbstractWorld}}} = nothing,
    perform_normalization::Bool = true,
    memo_max_height::Union{Nothing,Int} = nothing
)::Bool
    W = worldtype(i)

    if isnothing(w)
        if nworlds(frame(i)) == 1
            w = first(allworlds(frame(i)))
        end
    end
    @assert isgrounded(φ) || !(isnothing(w)) "Please, specify a world in order " *
        "to check non-grounded formula: $(syntaxstring(φ))."

    setformula(memo_structure::AbstractDict{<:Formula}, φ::Formula, val) = memo_structure[tree(φ)] = val
    readformula(memo_structure::AbstractDict{<:Formula}, φ::Formula) = memo_structure[tree(φ)]
    hasformula(memo_structure::AbstractDict{<:Formula}, φ::Formula) = haskey(memo_structure, tree(φ))

    if perform_normalization
        φ = normalize(φ; profile = :modelchecking, allow_atom_flipping = false)
    end

    memo_structure = begin
        if isnothing(use_memo)
            ThreadSafeDict{SyntaxTree,Worlds{W}}()
        else
            use_memo
        end
    end

    if !isnothing(memo_max_height)
        forget_list = Vector{SyntaxTree}()
    end

    fr = frame(i)

    # TODO try lazily
    (_f, _c) = filter, collect
    # (_f, _c) = Iterators.filter, identity

    if !hasformula(memo_structure, φ)
        for ψ in unique(subformulas(φ))
            if !isnothing(memo_max_height) && height(ψ) > memo_max_height
                push!(forget_list, ψ)
            end

            if !hasformula(memo_structure, ψ)
                tok = token(ψ)

                worldset = begin
                    if tok isa Connective
                        _c(collateworlds(fr, tok, map(f->readformula(memo_structure, f), children(ψ))))
                    elseif tok isa SyntaxLeaf
                        _f(_w->begin
                            istop(interpret(tok, i, _w))
                        end, _c(allworlds(fr)))
                    else
                        error("Unexpected token encountered in check: $(typeof(tok))")
                    end
                end
                setformula(memo_structure, ψ, Worlds{W}(worldset))
            end
            # @show syntaxstring(ψ), readformula(memo_structure, ψ)
        end
    end

    if !isnothing(memo_max_height)
        for ψ in forget_list
            delete!(memo_structure, ψ)
        end
    end

    ret = begin
        if isnothing(w) || w isa AnyWorld
            length(readformula(memo_structure, φ)) > 0
        else
            w in readformula(memo_structure, φ)
        end
    end

    return ret
end

############################################################################################
############################################################################################
############################################################################################

"""
    ismodal(::Type{<:Connective})::Bool = false
    ismodal(c::Connective)::Bool = ismodal(typeof(c))

Return whether it is known that an `Connective` is modal.

# Examples
```julia-repl
julia> ismodal(◊)
true

julia> ismodal(∧)
false
```
"""
ismodal(::Type{<:Connective})::Bool = false
ismodal(c::Connective)::Bool = ismodal(typeof(c))
ismodal(::Truth)::Bool = false

"""
    isbox(::Type{<:Connective})::Bool = false
    isbox(c::Connective)::Bool = isbox(typeof(c))

Return whether it is known that an `Connective` is a box (i.e., universal) connective.

# Examples
```julia-repl
julia> SoleLogics.isbox(◊)
false

julia> SoleLogics.isbox(∧)
false

julia> SoleLogics.isbox(□)
true
```
"""
isbox(::Any)::Bool = false
isbox(::Type{<:Connective})::Bool = false
isbox(c::Connective)::Bool = isbox(typeof(c))
isbox(::Truth)::Bool = false

isdiamond(::Any)::Bool = false
isdiamond(C::Type{<:Connective})::Bool = ismodal(C) && !isbox(C)
isdiamond(c::Connective)::Bool = isdiamond(typeof(c))
isdiamond(::Truth)::Bool = false

############################################################################################

"""
    abstract type AbstractRelationalConnective{R<:AbstractRelation} <: Connective end

Abstract type for relational logical connectives. A relational connective
allows for semantic quantification across relational structures (e.g., Kripke structures).
It has arity equal to the arity of its underlying relation minus one.

See, for example [temporal modal logic](https://en.wikipedia.org/wiki/Temporal_logic).

See also [`DiamondRelationalConnective`](@ref), [`BoxRelationalConnective`](@ref),
[`AbstractKripkeStructure`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractRelationalConnective{R<:AbstractRelation} <: Connective end

doc_op_rel = """
    relationtype(::AbstractRelationalConnective{R}) where {R<:AbstractRelation} = R
    relation(op::AbstractRelationalConnective) = relationtype(op)()

Return the underlying relation (and relation type) of the relational connective.

See also [`AbstractFrame`](@ref).
"""

"""$(doc_op_rel)"""
relationtype(::AbstractRelationalConnective{R}) where {R<:AbstractRelation} = R
"""$(doc_op_rel)"""
relation(op::AbstractRelationalConnective) = relationtype(op)()

arity(op::AbstractRelationalConnective) = arity(relation(op))-1

function precedence(op::AbstractRelationalConnective)
    if isunary(op)
        precedence(NEGATION)
    else
        error("Please, provide method SoleLogics.precedence(::$(typeof(op))).")
    end
end

function associativity(op::AbstractRelationalConnective)
    if isunary(op)
        associativity(NEGATION)
    else
        error("Please, provide method SoleLogics.associativity(::$(typeof(op))).")
    end
end

############################################################################################

"""
    collateworlds(
        fr::AbstractFrame{W},
        op::Operator,
        t::NTuple{N,WS},
    )::AbstractVector{<:W} where {N,W<:AbstractWorld,WS<:AbstractWorlds}

For a given crisp frame (`truthtype == Bool`),
return the set of worlds where a composed formula op(φ1, ..., φN) is true, given the `N`
sets of worlds where the each immediate sub-formula is true.

See also [`check`](@ref), [`iscrisp`](@ref),
[`Operator`](@ref), [`AbstractFrame`](@ref).
"""
function collateworlds(
    fr::AbstractFrame{W},
    op::Operator,
    t::NTuple{N,<:AbstractWorlds}
)::AbstractVector{<:W} where {N,W<:AbstractWorld}
    if arity(op) != length(t)
        return error("Cannot collate $(length(t)) truth values for " *
                     "operator $(typeof(op)) with arity $(arity(op))).")
    else
        return error("Please, provide method collateworlds(::$(typeof(fr)), " *
                     "::$(typeof(op)), ::NTuple{$(arity(op)), $(AbstractWorlds{W})}).")
    end
end
