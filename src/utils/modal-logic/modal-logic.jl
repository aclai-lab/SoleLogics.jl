import Base: show
using DataStructures: OrderedDict
using Graphs
using ThreadSafeDicts

"""
    struct World{T} <: AbstractWorld
        name::T
    end

A world that is solely identified by its `name`.
This can be useful when instantiating the underlying graph of a modal frame
in an explicit way.

See also [`OneWorld`](@ref), [`AbstractWorld`](@ref).
"""
struct World{T} <: AbstractWorld
    name::T
end

name(w::World) = w.name

inlinedisplay(w::World) = string(name(w))

include("frames/worlds.jl")

############################################################################################
##################################### Uni-modal logic ######################################
############################################################################################

# """
# Association "(w1,w2) => truth_value". Not recommended in sparse scenarios.
# """
# struct AdjMatUniModalFrame{W<:AbstractWorld,T<:Truth} <: AbstractUniModalFrame{W}
#     adjacents::NamedMatrix{T,Matrix{T},Tuple{OrderedDict{W,Int64},OrderedDict{W,Int64}}}
# end
# Upon construction, check that the type is not "OneWorld"
# end
# Add an example in the above docstring for accessibles
# accessibles(...) = ...

# TODO move truth value out of frame (frame is passive, perhaps it is relations that have a truth value)

"""
A unimodal frame given by an `Graphs.SimpleGraphs.AbstractSimpleGraph` on worlds.
"""
struct ExplicitCrispUniModalFrame{
    W<:AbstractWorld,
    G<:Graphs.SimpleGraphs.AbstractSimpleGraph,
} <: AbstractUniModalFrame{W}
    worlds::Worlds{W}
    graph::G
end
accessibles(fr::ExplicitCrispUniModalFrame, w::AbstractWorld) = fr.worlds[neighbors(fr.graph, findfirst(==(w), fr.worlds))]
allworlds(fr::ExplicitCrispUniModalFrame) = fr.worlds
nworlds(fr::ExplicitCrispUniModalFrame) = length(fr.worlds)

function Base.show(io::IO, fr::ExplicitCrispUniModalFrame)
    println(io, "$(typeof(fr)) with")
    println(io, "- worlds = $(inlinedisplay.(fr.worlds))")
    maxl = maximum(length.(inlinedisplay.(fr.worlds)))
    println(io, "- accessibles = \n$(join(["\t$(rpad(inlinedisplay(w), maxl)) -> [$(join(inlinedisplay.(accessibles(fr, w)), ", "))]" for w in fr.worlds], "\n"))")
end

############################################################################################
#### Multi-modal logic #####################################################################
############################################################################################

############################################################################################
# Singletons representing natural relations
############################################################################################

doc_identityrel = """
    struct IdentityRel <: AbstractRelation end;
    const identityrel   = IdentityRel();

Singleton type for the identity relation. This is a binary relation via which a world
accesses itself. The relation is also symmetric, reflexive and transitive.

# Examples
```julia-repl
julia> syntaxstring(SoleLogics.identityrel)
"="

julia> SoleLogics.converse(identityrel)
IdentityRel()
```

See also
[`globalrel`](@ref),
[`AbstractRelation`](@ref),
[`AbstractWorld`](@ref),
[`AbstractFrame`](@ref).
[`AbstractKripkeStructure`](@ref),
"""

"""$(doc_identityrel)"""
struct IdentityRel <: AbstractRelation end;
"""$(doc_identityrel)"""
const identityrel = IdentityRel();

arity(::IdentityRel) = 2

syntaxstring(::IdentityRel; kwargs...) = "="

hasconverse(::IdentityRel) = true
converse(::IdentityRel) = identityrel
istoone(::IdentityRel) = true
issymmetric(::IdentityRel) = true
isreflexive(::IdentityRel) = true
istransitive(::IdentityRel) = true

############################################################################################

doc_globalrel = """
    struct GlobalRel <: AbstractRelation end;
    const globalrel  = GlobalRel();

Singleton type for the global relation. This is a binary relation via which a world
accesses every other world within the frame.
The relation is also symmetric, reflexive and transitive.

# Examples
```julia-repl
julia> syntaxstring(SoleLogics.globalrel)
"G"

julia> SoleLogics.converse(globalrel)
GlobalRel()
```

See also
[`identityrel`](@ref),
[`AbstractRelation`](@ref),
[`AbstractWorld`](@ref),
[`AbstractFrame`](@ref).
[`AbstractKripkeStructure`](@ref),
"""

"""$(doc_globalrel)"""
struct GlobalRel <: AbstractRelation end;
"""$(doc_globalrel)"""
const globalrel = GlobalRel();

arity(::GlobalRel) = 2

syntaxstring(::GlobalRel; kwargs...) = "G"

hasconverse(::GlobalRel) = true
converse(::GlobalRel) = globalrel
issymmetric(::GlobalRel) = true
isreflexive(::GlobalRel) = true
istransitive(::GlobalRel) = true
isgrounding(::GlobalRel) = true

############################################################################################

"""
A binary relation via which a world *is accessed* by every other world within the frame.
That is, the binary relation that leads to a world.

See also
[`identityrel`](@ref),
[`AbstractRelation`](@ref),
[`AbstractWorld`](@ref),
[`AbstractFrame`](@ref).
[`AbstractKripkeStructure`](@ref),
"""
struct AtWorldRelation{W<:AbstractWorld} <: AbstractRelation
    w::W
end;

arity(::AtWorldRelation) = 2

syntaxstring(r::AtWorldRelation; kwargs...) = "@($(syntaxstring(r.w)))"

hasconverse(::AtWorldRelation) = false
issymmetric(::AtWorldRelation) = false
isreflexive(::AtWorldRelation) = false
istransitive(::AtWorldRelation) = true
isgrounding(::AtWorldRelation) = true

############################################################################################

# Shortcut: when enumerating accessibles through global relation, delegate to `allworlds`
accessibles(fr::AbstractMultiModalFrame, ::GlobalRel) = allworlds(fr)
accessibles(fr::AbstractMultiModalFrame, ::AbstractWorld, r::GlobalRel) = accessibles(fr, r)
accessibles(fr::AbstractMultiModalFrame, w::AbstractWorld,    ::IdentityRel) = [w]
accessibles(fr::AbstractMultiModalFrame, w::AbstractWorld,    r::AtWorldRelation) = [r.w]

############################################################################################

# TODO test
"""
    struct WrapperMultiModalFrame{
        W<:AbstractWorld,
        D<:AbstractDict{<:AbstractRelation,<:AbstractUniModalFrame{W}}
    } <: AbstractMultiModalFrame{W}
        frames::D
    end

A multi-modal frame that is the superposition of many uni-modal frames.
It uses a single `AbstractUniModalFrame` for
each of relations.

See also [`AbstractRelation`](@ref), [`AbstractUniModalFrame`](@ref).
"""
struct WrapperMultiModalFrame{
    W<:AbstractWorld,
    D<:AbstractDict{<:AbstractRelation,<:AbstractUniModalFrame{W}}
} <: AbstractMultiModalFrame{W}
    frames::D
end
function accessibles(
    fr::WrapperMultiModalFrame{W},
    w::W,
    r::AbstractRelation,
) where {W<:AbstractWorld}
    accessibles(frames[r], w, r)
end
function accessibles(
    fr::WrapperMultiModalFrame{W},
    w::W,
    r::IdentityRel,
) where {W<:AbstractWorld}
    [w]
end
function accessibles(
    fr::WrapperMultiModalFrame{W},
    w::W,
    r::GlobalRel,
) where {W<:AbstractWorld}
    accessibles(fr, r)
end

# """
# TODO
# """
# struct AdjMatCrispMultiModalFrame{
#     W<:AbstractWorld
# } <: AbstractMultiModalFrame{W}
#     worlds::Worlds{W}
#     adjacents::Vector{W,Dict{R,Vector{W,3}}}
# end
# accessibles(fr::AdjMatMultiModalFrame) = ...

# allworlds(fr::AdjMatMultiModalFrame) = fr.worlds
# nworlds(fr::AdjMatMultiModalFrame) = length(fr)

include("frames/world-filters.jl");

include("frames/relations.jl")

include("frames/frames.jl")

# TODO explain: a constant representing any world (only to be used for checking grounded formulas).
struct AnyWorld end

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

    ret = begin
        if isnothing(w) || w isa AnyWorld
            length(readformula(memo_structure, φ)) > 0
        else
            w in readformula(memo_structure, φ)
        end
    end

    if !isnothing(memo_max_height)
        for ψ in forget_list
            delete!(memo_structure, ψ)
        end
    end

    return ret
end

"""
    struct KripkeStructure{
        FR<:AbstractFrame,
        MAS<:AbstractDict
    } <: AbstractKripkeStructure
        frame::FR
        assignment::AS
    end

Type for representing
[Kripke structures](https://en.wikipedia.org/wiki/Kripke_structure_(model_checking)).
explicitly; it wraps a `frame`, and an abstract dictionary that assigns an interpretation to
each world.
"""
struct KripkeStructure{
    FR<:AbstractFrame,
    MAS<:AbstractDict
} <: AbstractKripkeStructure
    frame::FR
    assignment::MAS
end

frame(i::KripkeStructure) = i.frame

function interpret(a::Atom, i::KripkeStructure, w::W) where {W<:AbstractWorld}
    interpret(a, i.assignment[w])
end

function Base.show(io::IO, i::KripkeStructure)
    println(io, "$(typeof(i)) with")
    print(io, "- frame = ")
    Base.show(io, frame(i))
    maxl = maximum(length.(inlinedisplay.(allworlds(i))))
    println(io, "- valuations = \n$(join(["\t$(rpad(inlinedisplay(w), maxl)) -> $(inlinedisplay(i.assignment[w]))" for w in allworlds(i)], "\n"))")
end

doc_DIAMOND = """
    const DIAMOND = NamedConnective{:◊}()
    const ◊ = DIAMOND
    ismodal(::typeof(◊)) = true
    arity(::typeof(◊)) = 1

Logical diamond connective, typically interpreted as the modal existential quantifier.
See [here](https://en.wikipedia.org/wiki/Modal_operator).

See also [`BOX`](@ref), [`NamedConnective`](@ref), [`Connective`](@ref).
"""
"""$(doc_DIAMOND)"""
const DIAMOND = NamedConnective{:◊}()
"""$(doc_DIAMOND)"""
const ◊ = DIAMOND
ismodal(::Type{typeof(◊)}) = true
isbox(::Type{typeof(◊)}) = false
arity(::typeof(◊)) = 1
precedence(::typeof(◊)) = precedence(NEGATION)
associativity(::typeof(◊)) = associativity(NEGATION)

doc_BOX = """
    const BOX = NamedConnective{:□}()
    const □ = BOX
    arity(::typeof(□)) = 1

Logical box connective, typically interpreted as the modal universal quantifier.
See [here](https://en.wikipedia.org/wiki/Modal_operator).

See also [`DIAMOND`](@ref), [`NamedConnective`](@ref), [`Connective`](@ref).
"""
"""$(doc_BOX)"""
const BOX = NamedConnective{:□}()
"""$(doc_BOX)"""
const □ = BOX
ismodal(::Type{typeof(□)}) = true
isbox(::Type{typeof(□)}) = true
arity(::typeof(□)) = 1
precedence(::typeof(□)) = precedence(NEGATION)
associativity(::typeof(□)) = associativity(NEGATION)

hasdual(::typeof(DIAMOND)) = true
dual(::typeof(DIAMOND)) = BOX
hasdual(::typeof(BOX)) = true
dual(::typeof(BOX))     = DIAMOND

const BASE_MODAL_CONNECTIVES = [BASE_PROPOSITIONAL_CONNECTIVES..., ◊, □]
const BaseModalConnectives = Union{typeof.(BASE_MODAL_CONNECTIVES)...}

"""
    modallogic(;
        alphabet = AlphabetOfAny{String}(),
        operators = [⊤, ⊥, ¬, ∧, ∨, →, ◊, □],
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), [⊤, ⊥, ¬, ∧, ∨, →, ◊, □]),
        algebra = BooleanAlgebra(),
    )

Instantiate a [modal logic](https://simple.wikipedia.org/wiki/Modal_logic)
given a grammar and an algebra. Alternatively, an alphabet and a set of operators
can be specified instead of the grammar.

# Examples
```julia-repl
julia> (¬) isa operatorstype(modallogic());
true

julia> (□) isa operatorstype(modallogic());
true

julia> (□) isa operatorstype(modallogic(; operators = [¬, ∨]))
┌ Warning: Instantiating modal logic (via `modallogic`) with solely propositional operators (SoleLogics.NamedConnective[¬, ∨]). Consider using propositionallogic instead.
└ @ SoleLogics ~/.julia/dev/SoleLogics/src/modal-logic.jl:642
false

julia> modallogic(; alphabet = ["p", "q"]);

julia> modallogic(; alphabet = ExplicitAlphabet([Atom("p"), Atom("q")]));

```

See also [`propositionallogic`](@ref), [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref).
"""
function modallogic(;
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:Connective}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
    default_operators = BASE_MODAL_CONNECTIVES
)
    if !isnothing(operators) && length(setdiff(operators, BASE_PROPOSITIONAL_CONNECTIVES)) == 0
        @warn "Instantiating modal logic (via `modallogic`) with solely " *
            "propositional operators ($(operators)). Consider using propositionallogic instead."
    end
    _baselogic(
        alphabet = alphabet,
        operators = operators,
        grammar = grammar,
        algebra = algebra;
        default_operators = default_operators,
        logictypename = "modal logic",
    )
end

# A modal logic based on the base modal connectives
const BaseModalLogic = AbstractLogic{G,A} where {ALP,G<:AbstractGrammar{ALP,<:BaseModalConnectives},A<:AbstractAlgebra}

const archetypmodal_relops_docstring = """
    struct DiamondRelationalConnective{R<:AbstractRelation} <: AbstractRelationalConnective{R} end
    struct BoxRelationalConnective{R<:AbstractRelation} <: AbstractRelationalConnective{R} end

Singleton types for relational connectives, typically interpreted as the modal existential
and universal quantifier, respectively.

Both connectives can be easily instantiated with relation instances,
such as `DiamondRelationalConnective(rel)`, which is a shortcut for
`DiamondRelationalConnective{typeof(rel)}()`.

# Examples
```julia-repl
julia> syntaxstring(DiamondRelationalConnective(IA_A))
"⟨A⟩"

julia> syntaxstring(BoxRelationalConnective(IA_A))
"[A]"

julia> @assert DiamondRelationalConnective(IA_A) == SoleLogics.dual(BoxRelationalConnective(IA_A))

```

See also
[`DiamondRelationalConnective`](@ref), [`BoxRelationalConnective`](@ref),
[`syntaxstring`](@ref), [`dual`](@ref),
[`AbstractKripkeStructure`](@ref), [`AbstractFrame`](@ref).
"""

"""$(archetypmodal_relops_docstring)"""
struct DiamondRelationalConnective{R<:AbstractRelation} <: AbstractRelationalConnective{R} end
(DiamondRelationalConnective)(r::AbstractRelation) = DiamondRelationalConnective{typeof(r)}()

"""$(archetypmodal_relops_docstring)"""
struct BoxRelationalConnective{R<:AbstractRelation} <: AbstractRelationalConnective{R} end
(BoxRelationalConnective)(r::AbstractRelation) = BoxRelationalConnective{typeof(r)}()

ismodal(::Type{<:DiamondRelationalConnective}) = true
ismodal(::Type{<:BoxRelationalConnective}) = true

isbox(::Type{<:DiamondRelationalConnective}) = false
isbox(::Type{<:BoxRelationalConnective}) = true

function syntaxstring(op::DiamondRelationalConnective; use_modal_notation = nothing, kwargs...)
    if isnothing(use_modal_notation)
        return "⟨$(syntaxstring(relation(op); kwargs...))⟩"
    elseif use_modal_notation == :superscript
        return "◊$(SoleBase.superscript(syntaxstring(relation(op); kwargs...)))"
    # elseif use_modal_notation == :subscript
    #     return "◊$(SoleBase.subscript(syntaxstring(relation(op); kwargs...)))"
    else
        return error("Unexpected value for parameter `use_modal_notation`. Allowed are: [nothing, :superscript]")
    end
end
function syntaxstring(op::BoxRelationalConnective; use_modal_notation = nothing, kwargs...)
    if isnothing(use_modal_notation)
        return "[$(syntaxstring(relation(op); kwargs...))]"
    elseif use_modal_notation == :superscript
        return "□$(SoleBase.superscript(syntaxstring(relation(op); kwargs...)))"
    # elseif use_modal_notation == :subscript
    #     return "□$(SoleBase.subscript(syntaxstring(relation(op); kwargs...)))"
    else
        return error("Unexpected value for parameter `use_modal_notation`. Allowed are: [nothing, :superscript]")
    end
end

hasdual(::DiamondRelationalConnective) = true
dual(op::DiamondRelationalConnective) = BoxRelationalConnective{relationtype(op)}()
hasdual(::BoxRelationalConnective) = true
dual(op::BoxRelationalConnective)     = DiamondRelationalConnective{relationtype(op)}()

############################################################################################

"""
    diamond() = DIAMOND
    diamond(r::AbstractRelation) = DiamondRelationalConnective(r)

Return either the diamond modal connective from unimodal logic (i.e., ◊), or a
a diamond relational connective from a multi-modal logic, wrapping the relation `r`.

See also [`DiamondRelationalConnective`](@ref), [`diamond`](@ref), [`DIAMOND`](@ref).
"""
function diamond() DIAMOND end
function diamond(r::AbstractRelation) DiamondRelationalConnective(r) end

"""
    box() = BOX
    box(r::AbstractRelation) = BoxRelationalConnective(r)

Return either the box modal connective from unimodal logic (i.e., □), or a
a box relational connective from a multi-modal logic, wrapping the relation `r`.

See also [`BoxRelationalConnective`](@ref), [`box`](@ref), [`BOX`](@ref).
"""
function box() BOX end
function box(r::AbstractRelation) BoxRelationalConnective(r) end

globaldiamond = diamond(globalrel)
globalbox = box(globalrel)

identitydiamond = diamond(identityrel)
identitybox = box(identityrel)

function diamondsandboxes()
    return [diamond(), box()]
end
function diamondsandboxes(r::AbstractRelation)
    return [diamond(r), box(r)]
end
function diamondsandboxes(rs::AbstractVector{<:AbstractRelation})
    return Iterators.flatten([diamondsandboxes(r) for r in rs]) |> collect
end

# Well known connectives
Base.show(io::IO, c::Union{
    typeof(globaldiamond),
    typeof(globalbox),
    typeof(identitydiamond),
    typeof(identitybox)
}) = print(io, "$(syntaxstring(c))")

const BASE_MULTIMODAL_CONNECTIVES = [BASE_PROPOSITIONAL_CONNECTIVES...,
    globaldiamond,
    globalbox,
    diamond(identityrel),
    box(identityrel),
]
const BaseMultiModalConnectives = Union{typeof.(BASE_MULTIMODAL_CONNECTIVES)...}

# I know, these exceed 92 characters. But they look nicer like this!! :D
function collateworlds(fr::AbstractFrame{W}, t::BooleanTruth, ::NTuple{0,<:AbstractWorlds}) where {W<:AbstractWorld}
    istop(t) ? allworlds(fr) : W[]
end

collateworlds(fr::AbstractFrame{W}, ::typeof(¬), (ws,)::NTuple{1,<:AbstractWorlds}) where {W<:AbstractWorld} = setdiff(allworlds(fr), ws)
collateworlds(::AbstractFrame{W}, ::typeof(∧), (ws1, ws2)::NTuple{2,<:AbstractWorlds}) where {W<:AbstractWorld} = intersect(ws1, ws2)
collateworlds(::AbstractFrame{W}, ::typeof(∨), (ws1, ws2)::NTuple{2,<:AbstractWorlds}) where {W<:AbstractWorld} = union(ws1, ws2)
collateworlds(fr::AbstractFrame{W}, ::typeof(→), (ws1, ws2)::NTuple{2,<:AbstractWorlds}) where {W<:AbstractWorld} = union(setdiff(allworlds(fr), ws1), ws2)

function collateworlds(
    fr::AbstractFrame{W},
    op::typeof(◊),
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld}
    filter(w1->intersects(ws, accessibles(fr, w1)), collect(allworlds(fr)))
end

function collateworlds(
    fr::AbstractFrame{W},
    op::typeof(□),
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld}
    filter(w1->issubset(accessibles(fr, w1), ws), collect(allworlds(fr)))
end

# TODO: use AbstractMultiModalFrame
function collateworlds(
    fr::AbstractFrame{W},
    op::DiamondRelationalConnective,
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld}
    r = relation(op)
    if r == globalrel
        if length(ws) > 0
            collect(allworlds(fr))
        else
            W[]
        end
    else
        if hasconverse(r)
            # DIAMOND STRATEGY 1
            union(W[], [accessibles(fr, w, converse(r)) for w in ws]...)
        else
            # DIAMOND STRATEGY 2
            filter(w1->intersects(ws, accessibles(fr, w1, r)), collect(allworlds(fr)))
        end
    end
end

# TODO: use AbstractMultiModalFrame
function collateworlds(
    fr::AbstractFrame{W},
    op::BoxRelationalConnective,
    (ws,)::NTuple{1,<:AbstractWorlds},
) where {W<:AbstractWorld}
    r = relation(op)
    if r == globalrel
        if length(ws) == nworlds(fr) # Assuming no duplicates
            collect(allworlds(fr))
        else
            W[]
        end
    else
        if hasconverse(r)
            # BOX STRATEGY 1
            negws = setdiff(collect(allworlds(fr)), ws)
            negboxws = union(W[], [
                accessibles(fr, w, converse(r)) for w in negws]...)
            setdiff(collect(allworlds(fr)), negboxws)
            # BOX STRATEGY 3
            # filter(w1->all((w2)->w1 in accessibles(fr, w2, converse(r)), ws), collect(allworlds(fr)))
        else
            # BOX STRATEGY 2
            filter(w1->issubset(accessibles(fr, w1, r), ws), collect(allworlds(fr)))
        end
        # Note: this is wrong, as it does not include worlds for which φ is trivially true.
        # union(intersect(W[], [accessibles(fr, w, converse(r)) for w in ws]...))
    end
end
