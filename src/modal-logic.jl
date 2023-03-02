using DataStructures: OrderedDict
using NamedArrays

"""
    abstract type AbstractWorld end

Abstract type for the nodes of an annotated accessibility graph (Kripke structure).
This is used, for example, in modal logic, where the truth of
formulas is relativized to *worlds*, that is, nodes of a graph.

See also [`AbstractKripkeStructure`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractWorld end

include("algebras/worlds.jl")

"""
    abstract type AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

Abstract type for an accessibility graph (Kripke frame), that gives the structure to 
[Kripke structures](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).

See also [`truthtype`](@ref), [`worldtype`](@ref),
[`allworlds`](@ref), [`nworlds`](@ref), [`initialworld`](@ref),
[`AbstractKripkeStructure`](@ref), [`AbstractWorld`](@ref).
"""
abstract type AbstractFrame{W<:AbstractWorld,T<:TruthValue} end

"""
    truthtype(::Type{<:AbstractFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = T
    truthtype(fr::AbstractFrame) = truthtype(typeof(fr))

Returns the truth type for the relation(s) in the frame.

See also [`AbstractFrame`](@ref).
"""
truthtype(::Type{<:AbstractFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = T
truthtype(fr::AbstractFrame) = truthtype(typeof(fr))

# TODO this is actually a trait (see iscrisp(::AbstractAlgebra)).
# Move somewhere else, properly.
iscrisp(FR::Type{<:AbstractFrame}) = (truthtype(FR) == Bool)
iscrisp(fr::AbstractFrame) = iscrisp(typeof(fr))

"""
    worldtype(::Type{<:AbstractFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = W
    worldtype(fr::AbstractFrame) = worldtype(typeof(fr))

Returns the world type of the frame.

See also [`AbstractFrame`](@ref).
"""
worldtype(::Type{<:AbstractFrame{W,T}}) where {W<:AbstractWorld,T<:TruthValue} = W
worldtype(fr::AbstractFrame) = worldtype(typeof(fr))

"""
    allworlds(fr::AbstractFrame{W})::AbstractVector{<:W} where {W<:AbstractWorld}

Returns all worlds within the frame.

See also [`nworlds`](@ref), [`initialworld`](@ref), [`AbstractFrame`](@ref).
"""
function allworlds(fr::AbstractFrame{W})::AbstractVector{<:W} where {W<:AbstractWorld}
    error("Please, provide method allworlds(frame::$(typeof(fr))).")
end

"""
    nworlds(fr::AbstractFrame)::Integer

Returns the number of worlds within the frame.

See also [`nworlds`](@ref), [`AbstractFrame`](@ref).
"""
function nworlds(fr::AbstractFrame)::Integer
    error("Please, provide method nworlds(frame::$(typeof(fr))).")
end

"""
    initialworld(fr::AbstractFrame{W})::W

Returns the initial world of the frame. Note that not all frame types
can provide an initial world.

See also [`allworlds`](@ref), [`nworlds`](@ref), [`AbstractFrame`](@ref).
"""
function initialworld(fr::AbstractFrame{W})::W where {W<:AbstractWorld}
    error("Please, provide method initialworld(frame::$(typeof(fr))).")
end

############################################################################################
##################################### Uni-modal logic ######################################
############################################################################################

"""
    abstract type AbstractUniModalFrame{
        W<:AbstractWorld,
        T<:TruthValue
    } <: AbstractFrame{W,T} end

Frame of a modal logic based on a single (implicit) accessibility relation.

See also [`AbstractMultiModalFrame`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractUniModalFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractFrame{W,T} end

"""
    accessibles(fr::AbstractUniModalFrame{W}, w::W)::Vector{W} where {W<:AbstractWorld}

Returns the worlds in frame `fr` that are accessible from world `w`.

See also [`AbstractWorld`](@ref), [`AbstractUniModalFrame`](@ref).
"""
function accessibles(fr::AbstractUniModalFrame{W}, w::W)::Vector{W} where {W<:AbstractWorld}
    error("Please, provide method accessibles(fr::$(typeof(f)), w::$(typeof(w)))::Vector{$(W)}.")
end

############################################################################################

# """
# TODO Mauro
# Association "(w1,w2) => truth_value". Not recommended in sparse scenarios.
# """
# struct AdjMatModalFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractUniModalFrame{W,T}
#     adjacents::NamedMatrix{T,Matrix{T},Tuple{OrderedDict{W,Int64},OrderedDict{W,Int64}}}
# Upon construction, check that the type is not "OneWorld"
# end
# Add an example in the above docstring for accessibles
# accessibles(...) = ...

############################################################################################
#################################### Multi-modal logic #####################################
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

See also
[`issymmetric`](@ref),
[`isreflexive`](@ref),
[`istransitive`](@ref),
[`arity`](@ref),
[`syntaxstring`](@ref),
[`converse`](@ref),
[`hasconverse`](@ref),
[`IdentityRel`](@ref),
[`GlobalRel`](@ref),
[`AbstractKripkeStructure`](@ref),
[`AbstractFrame`](@ref),
[`AbstractWorld`](@ref).
"""
abstract type AbstractRelation end

"""
    arity(::Type{<:AbstractRelation})::Integer
    arity(t::AbstractRelation)::Integer = arity(typeof(t))

Returns the `arity` of the relation.

# Extended help

When defining a new relation type `R` with arity `2`, please provide the method:

    arity(::Type{R}) = 2

See also [`AbstractRelation`](@ref).
"""
arity(R::Type{<:AbstractRelation})::Integer = error("Please, provide method arity(::$(typeof(R))).")
arity(r::AbstractRelation)::Integer = arity(typeof(r))

syntaxstring(R::Type{<:AbstractRelation}; kwargs...)::String = error("Please, provide method syntaxstring(::$(typeof(R)); kwargs...).")
syntaxstring(r::AbstractRelation; kwargs...)::String = syntaxstring(typeof(r); kwargs...)

# TODO3: This *should be* inverse (not converse??)
doc_conv_rel = """
    converse(R::Type{<:AbstractRelation})::Type{<:AbstractRelation}
    converse(r::AbstractRelation)::AbstractRelation = converse(typeof(r))()
    
If it exists, returns the converse relation (type) of a given relation (type)

# Extended help

This trait is implemented as:

    hasconverse(R::Type{<:AbstractRelation})::Bool = false
    hasconverse(r::AbstractRelation)::Bool = hasconverse(typeof(r))

    converse(R::Type{<:AbstractRelation})::Type{<:AbstractRelation} = error("Please, provide method converse(::\$(typeof(R))).")
    converse(r::AbstractRelation)::AbstractRelation = converse(typeof(r))()
    
When defining a new symmetric relation `R` with converse `CR`, please define the two methods:

    hasconverse(R::Type{R}) = true
    converse(R::Type{R}) = CR

See also [`issymmetric`](@ref), [`isreflexive`](@ref), [`istransitive`](@ref), [`AbstractRelation`](@ref).
"""

"""$(doc_conv_rel)"""
hasconverse(R::Type{<:AbstractRelation})::Bool = false
hasconverse(r::AbstractRelation)::Bool = hasconverse(typeof(r))

"""$(doc_conv_rel)"""
converse(R::Type{<:AbstractRelation})::Type{<:AbstractRelation} = error("Please, provide method converse(::$(typeof(R))).")
converse(r::AbstractRelation)::AbstractRelation = converse(typeof(r))()


"""
    issymmetric(::AbstractRelation) = hasconverse(r) ? converse(r) == r : false
    
Returns whether it is known that a relation is symmetric.

See also [`hasconverse`](@ref), [`converse`](@ref),
[`isreflexive`](@ref), [`istransitive`](@ref), [`AbstractRelation`](@ref).
"""
# TODO3: r is not defined
issymmetric(::AbstractRelation) = hasconverse(r) ? converse(r) == r : false

"""
    isreflexive(::AbstractRelation)
    
Returns whether it is known that a relation is reflexive.

See also 
[`issymmetric`](@ref), [`istransitive`](@ref), [`AbstractRelation`](@ref).
"""
isreflexive(::AbstractRelation) = false

"""
    istransitive(::AbstractRelation)
    
Returns whether it is known that a relation is transitive.

See also 
[`isreflexive`](@ref), [`issymmetric`](@ref), [`AbstractRelation`](@ref).
"""
istransitive(::AbstractRelation) = false

include("algebras/relations.jl")

############################################################################################

"""
    abstract type AbstractMultiModalFrame{
        W<:AbstractWorld,
        T<:TruthValue,
    } <: AbstractFrame{W,T} end

Frame of a multi-modal logic, that is, a modal logic based on a set
of accessibility relations.

See also [`AbstractUniModalFrame`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractMultiModalFrame{
    W<:AbstractWorld,
    T<:TruthValue,
} <: AbstractFrame{W,T} end


"""
    accessibles(
        fr::AbstractMultiModalFrame{W},
        w::W,
        r::AbstractRelation
    ) where {W<:AbstractWorld}

Returns the worlds in frame `fr` that are accessible from world `w`
via relation `r`.

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
# Extended help

Since `accessibles` always returns an iterator of worlds of the same type `W`,
the current implementation of `accessibles` delegates the enumeration
to a lower level `_accessibles` function, which returns an iterator of parameter tuples that are,
then, fed to the world constructor the using IterTools generators, as in:

    function accessibles(
        fr::AbstractMultiModalFrame{W},
        w::W,
        r::AbstractRelation,
    ) where {W<:AbstractWorld}
        IterTools.imap(W, _accessibles(fr, w, r))
    end

As such, when defining new frames, worlds, and/or relations, one should provide new methods
for `_accessibles`. For example:

    _accessibles(fr::Full1DFrame, w::Interval{Int}, ::_IA_A) = zip(Iterators.repeated(w.y), w.y+1:X(fr)+1)

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
`collect(accessibles(fr, w, r)) isa AbstractVector{W}`.

See also [`AbstractWorld`](@ref),
[`AbstractRelation`](@ref), [`AbstractMultiModalFrame`](@ref).
"""
function accessibles(
    fr::AbstractMultiModalFrame{W},
    w::W,
    r::AbstractRelation,
) where {W<:AbstractWorld}
    IterTools.imap(W, _accessibles(fr, w, r))
end

# TODO needed?
# """
# A frame must indicate their compatible relations via the `goeswith` trait, which
# is defaulted by:

#     goeswith(::Type{<:AbstractMultiModalFrame}, ::AbstractRelation) = false

# For example, if frame of type `FR` is compatible with relation `R`, specify:
    
#     goeswith(::Type{FR}, ::R) = true
# """
# goeswith(::Type{<:AbstractMultiModalFrame}, ::AbstractRelation) = false
# # TODO Perhaps a check whether the `accessibles` method exists is enough?
# # as in @show methods(accessibles, (FR, worldtype(FR), typeof(r)))
# # goeswith(::Type{FR}, r::AbstractRelation) where {FR<:AbstractMultiModalFrame} = begin
# #     @show methods(accessibles, (FR, worldtype(FR), typeof(r)))
# # end

############################################################################################

"""
    struct WrapperMultiModalFrame{
        W<:AbstractWorld,
        T<:TruthValue,
        D<:AbstractDict{<:AbstractRelation,<:AbstractUniModalFrame{W,T}}
    } <: AbstractMultiModalFrame{W,T}
        frames::D
    end

Multi-modal frame that is the superposition of many uni-modal frames.
It uses a single `AbstractUniModalFrame` for
each of relations.

See also [`AbstractRelation`](@ref), [`AbstractUniModalFrame`](@ref).
"""
struct WrapperMultiModalFrame{
    W<:AbstractWorld,
    T<:TruthValue,
    D<:AbstractDict{<:AbstractRelation,<:AbstractUniModalFrame{W,T}}
} <: AbstractMultiModalFrame{W,T}
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
# struct AdjMatMultiModalFrame{W<:AbstractWorld,T<:TruthValue} <: AbstractMultiModalFrame{W,T}
#     adjacents::NamedArray{W,3}
# end
# # accessibles(...) = ...

include("algebras/frames.jl")

############################################################################################
############################################################################################
############################################################################################

"""
    abstract type AbstractModalAssignment{W<:AbstractWorld,A,T<:TruthValue} end

A modal assignment is a mapping from `World`s to propositional assignments;
or equivalently, a mapping from `World`s, `Proposition`s of atom type `A`
to truth values of type `T`.

See also [`AbstractAssignment`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractModalAssignment{W<:AbstractWorld,A,T<:TruthValue} end

# """
# TODO
# """
# function check(::Proposition{A}, ::AbstractModalAssignment{W,A,T}, ::W)::T where {W<:AbstractWorld,A,T<:TruthValue}
#     error("Please, provide ...")
# end

# struct GenericExplicitAssignment{W<:AbstractWorld,A,T<:TruthValue} <: AbstractModalAssignment{W,A,T}
#     dict::Dict{W,AbstractAssignment{A,T}}
# end

############################################################################################


"""
    abstract type AbstractKripkeStructure{
        W<:AbstractWorld,
        A,
        T<:TruthValue,
        FR<:AbstractFrame{W,T},
    } <: AbstractInterpretation{A,T} end

Abstract type for representing
[Kripke structures](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).
It comprehends a directed graph structure (Kripke frame), where nodes are referred to as
*worlds*, and the binary relation between them is referred to as the
*accessibility relation*. Additionally, each world is associated with a mapping from
`Proposition`s of atom type `A` to truth values of type `T`.

See also [`AbstractInterpretation`](@ref).
"""
abstract type AbstractKripkeStructure{
    W<:AbstractWorld,
    A,
    T<:TruthValue,
    FR<:AbstractFrame{W,T},
} <: AbstractInterpretation{A,T} end

function check(
    ::Proposition{A},
    ::AbstractKripkeStructure{W,A,T},
    ::W,
)::T where {W<:AbstractWorld,A,T<:TruthValue}
    error("Please, provide ...")
end

function check(
    ::Formula,
    ::AbstractKripkeStructure{W,A,T},
    ::W,
)::T where {W<:AbstractWorld,A,T<:TruthValue}
    error("Please, provide ...")
end

function frame(i::AbstractKripkeStructure{W,A,T,FR})::FR where {W<:AbstractWorld,A,T<:TruthValue,FR<:AbstractFrame{W,T}}
    return error("Please, provide method frame(i::$(typeof(i))).")
end

"""
    truthtype(::Type{<:AbstractKripkeStructure{W,A,T}}) where {W<:AbstractWorld,A,T<:TruthValue} = T
    truthtype(a::AbstractKripkeStructure) = truthtype(typeof(a))

The truth type of the model.

See also [`AbstractKripkeStructure`](@ref).
"""
truthtype(::Type{<:AbstractKripkeStructure{W,A,T}}) where {W<:AbstractWorld,A,T<:TruthValue} = T
truthtype(a::AbstractKripkeStructure) = truthtype(typeof(a))

"""
    worldtype(::Type{<:AbstractKripkeStructure{W,A,T}}) where {W<:AbstractWorld,A,T<:TruthValue} = W
    worldtype(a::AbstractKripkeStructure) = worldtype(typeof(a))

The world type of the model.

See also [`AbstractKripkeStructure`](@ref).
"""
worldtype(::Type{<:AbstractKripkeStructure{W,A,T}}) where {W<:AbstractWorld,A,T<:TruthValue} = W
worldtype(a::AbstractKripkeStructure) = worldtype(typeof(a))

accessibles(i::AbstractKripkeStructure, args...) = accessibles(frame(i), args...)
allworlds(i::AbstractKripkeStructure, args...) = allworlds(frame(i), args...)
nworlds(i::AbstractKripkeStructure) = nworlds(frame(i))
initialworld(i::AbstractKripkeStructure) = initialworld(frame(i))

############################################################################################

"""
    struct KripkeStructure{
        W<:AbstractWorld,
        A,
        T<:TruthValue,
        FR<:AbstractFrame{W,T},
        AS<:AbstractModalAssignment{W,A,T}
    } <: AbstractKripkeStructure{W,A,T,FR}
        frame::FR
        assignment::AS
    end


Structure for representing
[Kripke structures](https://en.m.wikipedia.org/wiki/Kripke_structure_(model_checking))'s).
explicitly; it wraps a `frame`, and an abstract dictionary that assigns an interpretation to
each world.
"""
struct KripkeStructure{
    W<:AbstractWorld,
    A,
    T<:TruthValue,
    FR<:AbstractFrame{W,T},
    AS<:AbstractModalAssignment{W,A,T}
} <: AbstractKripkeStructure{W,A,T,FR}
    frame::FR
    assignment::AS
end

function check(f::Formula, i::KripkeStructure{W,A,T}, w::W)::T where {W<:AbstractWorld,A,T<:TruthValue} end

function check(f::Formula, i::KripkeStructure{W,A,T})::T where {W<:AbstractWorld,A,T<:TruthValue}
    check(f, i, initial(i))
end

function check(p::Proposition{A}, i::KripkeStructure{W,A}, w::W) where {W<:AbstractWorld,A}
    check(p, i.assignment, w)
end

# TODO maybe this yields the worlds where a certain formula is true...?
# function check(i::KripkeStructure{W,A,T}, f::Formula)::AbstractVector{W} where {W<:AbstractWorld,A,T<:TruthValue}

############################################################################################
############################################################################################
############################################################################################

"""
    ismodal(::Type{<:AbstractOperator})::Bool = false
    ismodal(o::AbstractOperator)::Bool = ismodal(typeof(o))

Returns whether it is known that an `AbstractOperator` is modal.

# Examples
```julia-repl
julia> ismodal(◊)
true
julia> ismodal(∧)
false
```
"""
ismodal(::Type{<:AbstractOperator})::Bool = false
ismodal(o::AbstractOperator)::Bool = ismodal(typeof(o))

"""
    isbox(::Type{<:AbstractOperator})::Bool = false
    isbox(o::AbstractOperator)::Bool = isbox(typeof(o))

Returns whether it is known that an `AbstractOperator` is a box (i.e., universal) operator.

# Examples
```julia-repl
julia> SoleLogics.isbox(◊)
true

julia> SoleLogics.isbox(∧)
false

julia> SoleLogics.isbox(□)
true
```
"""
isbox(::Type{<:AbstractOperator})::Bool = false
isbox(o::AbstractOperator)::Bool = isbox(typeof(o))

doc_DIAMOND = """
    const DIAMOND = NamedOperator{:◊}()
    const ◊ = DIAMOND
    ismodal(::NamedOperator{:◊}) = true
    arity(::Type{typeof(◊)}) = 1

Logical diamond operator, typically interpreted as the modal existential quantifier.
See (here)[https://en.m.wikipedia.org/wiki/Modal_operator].

See also [`BOX`](@ref), [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""$(doc_DIAMOND)"""
const DIAMOND = NamedOperator{:◊}()
"""$(doc_DIAMOND)"""
const ◊ = DIAMOND
ismodal(::Type{typeof(◊)}) = true
isbox(::Type{typeof(◊)}) = false
arity(::Type{typeof(◊)}) = 1


doc_BOX = """
    const BOX = NamedOperator{:□}()
    const □ = BOX
    arity(::Type{typeof(□)}) = 1

Logical box operator, typically interpreted as the modal universal quantifier.
See (here)[https://en.m.wikipedia.org/wiki/Modal_operator].

See also [`DIAMOND`](@ref), [`NamedOperator`](@ref), [`AbstractOperator`](@ref).
"""
"""$(doc_BOX)"""
const BOX = NamedOperator{:□}()
"""$(doc_BOX)"""
const □ = BOX
ismodal(::Type{typeof(□)}) = true
isbox(::Type{typeof(□)}) = true
arity(::Type{typeof(□)}) = 1

"""
    dual(op::DiamondRelationalOperator) = BoxRelationalOperator{relationtype(op)}()
    dual(op::BoxRelationalOperator)     = DiamondRelationalOperator{relationtype(op)}()

Returns the modal operator with dual semantics (existential<->universal).

See also
[`DiamondRelationalOperator`](@ref), [`BoxRelationalOperator`](@ref).
"""
dual(::typeof(DIAMOND)) = BOX
dual(::typeof(BOX))     = DIAMOND


############################################################################################

const BASE_MODAL_OPERATORS = [BASE_PROPOSITIONAL_OPERATORS..., ◊, □]
const BaseModalOperators = Union{typeof.(BASE_MODAL_OPERATORS)...}

"""
    modallogic(;
        alphabet = AlphabetOfAny{String}(),
        operators = [⊤, ⊥, ¬, ∧, ∨, →, ◊, □],
        grammar = CompleteFlatGrammar(AlphabetOfAny{String}(), [⊤, ⊥, ¬, ∧, ∨, →, ◊, □]),
        algebra = BooleanAlgebra(),
    )

Instantiates a [modal logic](https://simple.m.wikipedia.org/wiki/Modal_logic)
given a grammar and an algebra. Alternatively, an alphabet and a set of operators
can be specified instead of the grammar.

# Examples
```julia-repl
julia> (¬) isa operatorstype(modallogic());
true

julia> (□) isa operatorstype(modallogic());
true

julia> (□) isa operatorstype(modallogic(; operators = [¬, ∨]))
┌ Warning: Instantiating modal logic (via `modallogic`) with solely propositional operators (SoleLogics.NamedOperator[¬, ∨]). Consider using propositionallogic instead.
└ @ SoleLogics ~/.julia/dev/SoleLogics/src/modal-logic.jl:642
false

julia> modallogic(; alphabet = ["p", "q"]);

julia> modallogic(; alphabet = ExplicitAlphabet([Proposition("p"), Proposition("q")]));

```

See also [`propositionallogic`](@ref), [`AbstractAlphabet`](@ref), [`AbstractAlgebra`](@ref).
"""
function modallogic(;
    alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    operators::Union{Nothing,Vector{<:AbstractOperator}} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
)
    if !isnothing(operators) && length(setdiff(operators, BASE_PROPOSITIONAL_OPERATORS)) == 0
        @warn "Instantiating modal logic (via `modallogic`) with solely" *
            " propositional operators ($(operators)). Consider using propositionallogic instead."
    end
    _baselogic(
        alphabet = alphabet,
        operators = operators,
        grammar = grammar,
        algebra = algebra;
        default_operators = BASE_MODAL_OPERATORS,
        logictypename = "modal logic",
    )
end

# A modal logic based on the base modal operators
const BaseModalLogic = AbstractLogic{G,A} where {ALP,G<:AbstractGrammar{ALP,<:BaseModalOperators},A<:AbstractAlgebra}

############################################################################################

"""
    abstract type AbstractRelationalOperator{R<:AbstractRelation} <: AbstractOperator end

Abstract type for relational logical operators. A relational operator 
allows for semantic quantification across relational structures (e.g., Krikpe structures).
It has arity equal to the arity of its underlying relation minus one.

See, for example (temporal modal logic)[https://en.m.wikipedia.org/wiki/Temporal_logic].

See also [`DiamondRelationalOperator`](@ref), [`BoxRelationalOperator`](@ref),
[`AbstractKripkeStructure`](@ref), [`AbstractFrame`](@ref).
"""
abstract type AbstractRelationalOperator{R<:AbstractRelation} <: AbstractOperator end

arity(::Type{<:AbstractRelationalOperator{R}}) where {R<:AbstractRelation} = arity(R)-1

doc_op_rel = """
    relationtype(::AbstractRelationalOperator{R}) where {R<:AbstractRelation} = R
    relation(op::AbstractRelationalOperator) = relationtype(op)()

Returns the underlying relation (and relation type) of the relational operator.

See also [`AbstractFrame`](@ref).
"""

"""$(doc_op_rel)"""
relationtype(::AbstractRelationalOperator{R}) where {R<:AbstractRelation} = R
"""$(doc_op_rel)"""
relation(op::AbstractRelationalOperator) = relationtype(op)()

"""
    struct DiamondRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
    struct BoxRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end

Singleton types for relational operators, typically interpreted as the modal existential
and universal quantifier, respectively.

Both operators can be easily instantiated with relation instances,
such as `DiamondRelationalOperator(rel)`, which is a shortcut for
`DiamondRelationalOperator{typeof(rel)}()`.

# Examples
```julia-repl
julia> syntaxstring(DiamondRelationalOperator(IA_A))
"⟨A⟩"

julia> syntaxstring(BoxRelationalOperator(IA_A))
"[A]"

julia> @assert DiamondRelationalOperator(IA_A) == SoleLogics.dual(BoxRelationalOperator(IA_A))

```

See also
[`DiamondRelationalOperator`](@ref), [`BoxRelationalOperator`](@ref),
[`syntaxstring`](@ref), [`dual`](@ref),
[`AbstractKripkeStructure`](@ref), [`AbstractFrame`](@ref).
"""
struct DiamondRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(DiamondRelationalOperator)(r::AbstractRelation) = DiamondRelationalOperator{typeof(r)}()

struct BoxRelationalOperator{R<:AbstractRelation} <: AbstractRelationalOperator{R} end
(BoxRelationalOperator)(r::AbstractRelation) = BoxRelationalOperator{typeof(r)}()

ismodal(::Type{<:DiamondRelationalOperator}) = true
ismodal(::Type{<:BoxRelationalOperator}) = true

isbox(::Type{<:DiamondRelationalOperator}) = false
isbox(::Type{<:BoxRelationalOperator}) = true

syntaxstring(op::DiamondRelationalOperator; kwargs...) = "⟨$(syntaxstring(relationtype(op); kwargs...))⟩"
syntaxstring(op::BoxRelationalOperator; kwargs...)     = "[$(syntaxstring(relationtype(op); kwargs...))]"

"""
    dual(op::DiamondRelationalOperator) = BoxRelationalOperator{relationtype(op)}()
    dual(op::BoxRelationalOperator)     = DiamondRelationalOperator{relationtype(op)}()

Returns the modal operator with dual semantics (existential<->universal).

See also
[`DiamondRelationalOperator`](@ref), [`BoxRelationalOperator`](@ref).
"""
dual(op::DiamondRelationalOperator) = BoxRelationalOperator{relationtype(op)}()
dual(op::BoxRelationalOperator)     = DiamondRelationalOperator{relationtype(op)}()

