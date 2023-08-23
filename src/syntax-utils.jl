import Base: show, promote_rule, length, getindex
using SoleBase

doc_lmlf = """
    struct LeftmostLinearForm{O<:AbstractOperator,SS<:AbstractSyntaxStructure} <: AbstractSyntaxStructure
        children::Vector{<:SS}
    end

A syntax structure representing the `foldl` of a set of other syntax structure of type `SS`
by means of an operator `O`. This structure enables a structured instantiation of
formulas in conjuctive/disjunctive forms, and
conjuctive normal form (CNF) or disjunctive normal form (DNF), defined as:

    const LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}
    const LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

    const CNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),LeftmostLinearForm{typeof(∨),SS}}
    const DNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),LeftmostLinearForm{typeof(∧),SS}}

# Examples
```julia-repl
julia> LeftmostLinearForm(→, parseformula.(["p", "q", "r"]))
LeftmostLinearForm{SoleLogics.NamedOperator{:→},SyntaxTree{Proposition{String}}}
    (p) → (q) → (r)

julia> LeftmostConjunctiveForm(parseformula.(["¬p", "q", "¬r"]))
LeftmostLinearForm{SoleLogics.NamedOperator{:∧},SyntaxTree}
    (¬(p)) ∧ (q) ∧ (¬(r))

julia> LeftmostDisjunctiveForm{Literal}([Literal(false, Proposition("p")), Literal(true, Proposition("q")), Literal(false, Proposition("r"))])
LeftmostLinearForm{SoleLogics.NamedOperator{:∨},Literal}
    (¬(p)) ∨ (q) ∨ (¬(r))

julia> LeftmostDisjunctiveForm([LeftmostConjunctiveForm(parseformula.(["¬p", "q", "¬r"]))]) isa SoleLogics.DNF
true

```
"""

"""$(doc_lmlf)

See also [`AbstractSyntaxStructure`](@ref), [`SyntaxTree`](@ref),
[`LeftmostConjunctiveForm`](@ref), [`LeftmostDisjunctiveForm`](@ref),
[`Literal`](@ref).
"""
struct LeftmostLinearForm{O<:AbstractOperator,SS<:AbstractSyntaxStructure} <: AbstractSyntaxStructure
    children::Vector{SS}

    function LeftmostLinearForm{O,SS}(
        children::Vector,
    ) where {O<:AbstractOperator,SS<:AbstractSyntaxStructure}
        a = arity(O)
        n_children = length(children)

        if a == 0
            n_children == 0 ||
                error("Mismatching number of children ($n_children) and operator's arity ($a).")
        elseif a == 1
            n_children == 1 ||
                error("Mismatching number of children ($n_children) and operator's arity ($a).")
        else
            h = (n_children-1)/(a-1)
            (isinteger(h) && h >= 0) ||
            # TODO figure out whether the base case n_children = 0 makes sense
                error("Mismatching number of children ($n_children) and operator's arity ($a).")
        end

        new{O,SS}(children)
    end

    function LeftmostLinearForm{O}(
        children::Vector,
    ) where {O<:AbstractOperator}
        length(children) > 0 || error("Cannot instantiate LeftmostLinearForm{$(O)} with no children.")
        SS = SoleBase._typejoin(typeof.(children)...)
        LeftmostLinearForm{O,SS}(children)
    end

    function LeftmostLinearForm(
        O::Type{<:SoleLogics.AbstractOperator},
        children::Vector,
    )
        LeftmostLinearForm{O}(children)
    end

    function LeftmostLinearForm(
        op::AbstractOperator,
        children::Vector,
    )
        LeftmostLinearForm(typeof(op), children)
    end

    function LeftmostLinearForm(
        tree::SyntaxTree,
        operator::Union{<:SoleLogics.AbstractOperator,Nothing} = nothing
    )
        # Check operator correctness; it should not be nothing (thus, auto inferred) if
        # tree root contains something that is not an AbstractOperator
        if (!(token(tree) isa AbstractOperator) && !isnothing(operator))
            error("Syntax tree cannot be converted to a LeftmostLinearForm:" *
                "tree root is $(token(tree))")
        end

        if isnothing(operator)
            operator = token(tree)
        end

        # Get a vector of `SyntaxTree`s, having `operator` as common ancestor, then,
        # call LeftmostLinearForm constructor.
        _children = AbstractSyntaxStructure[]

        function _dig_and_retrieve(tree::SyntaxTree, operator::SoleLogics.AbstractOperator)
            token(tree) != operator ?
            push!(_children, tree) :    # Lexical scope
            for c in children(tree)
                _dig_and_retrieve(c, operator)
            end
        end
        _dig_and_retrieve(tree, operator)

        LeftmostLinearForm(operator, _children)
    end
end

children(lf::LeftmostLinearForm) = lf.children
op(::LeftmostLinearForm{O}) where {O} = O()

operatortype(::LeftmostLinearForm{O}) where {O} = O
childrentype(::LeftmostLinearForm{O,SS}) where {O,SS} = SS

Base.length(lf::LeftmostLinearForm) = Base.length(children(lf))
function Base.getindex(
    lf::LeftmostLinearForm{O,SS},
    idxs::AbstractVector
) where {O,SS}
    return LeftmostLinearForm{O,SS}(children(lf)[idxs])
end
Base.getindex(lf::LeftmostLinearForm, idx::Integer) = Base.getindex(lf,[idx])

nchildren(lf::LeftmostLinearForm) = length(children(lf))

# TODO: add parameter remove_redundant_parentheses
# TODO: add parameter parentheses_at_propositions
function syntaxstring(
    lf::LeftmostLinearForm;
    function_notation = false,
    kwargs...,
)
    if function_notation
        syntaxstring(tree(lf); function_notation = function_notation, kwargs...)
    else
        ch = children(lf)
        if length(ch) == 0
            syntaxstring(op(lf); kwargs...)
        else
            children_ss = map(
                c->syntaxstring(c; kwargs...),
                ch
            )
            "(" * join(children_ss, ") $(syntaxstring(op(lf); kwargs...)) (") * ")"
        end
    end
end

function tree(lf::LeftmostLinearForm)
    operator = op(lf)
    a = arity(operator)
    cs = children(lf)

    st = begin
        if length(cs) == 0
            # No children
            SyntaxTree(operator)
        elseif length(cs) == 1
            # Only child
            tree(cs[1])
        else
            function _tree(childs::Vector{<:SyntaxTree})
                @assert (length(childs) != 0) "$(childs); $(lf); $(operator); $(a); $(cs)"
                return length(childs) == a ?
                    SyntaxTree(operator, childs...) :
                    SyntaxTree(operator, childs[1:(a-1)]..., _tree(childs[a:end]))
            end
            _tree(tree.(children(lf)))
        end
    end

    return st
end

function Base.show(io::IO, lf::LeftmostLinearForm{O,SS}) where {O,SS}
    println(io, "LeftmostLinearForm{$(O),$(SS)} with $(nchildren(lf)) children")
    println(io, "\t$(syntaxstring(lf))")
end

Base.promote_rule(::Type{<:LeftmostLinearForm}, ::Type{<:LeftmostLinearForm}) = SyntaxTree
Base.promote_rule(::Type{SS}, ::Type{LF}) where {SS<:AbstractSyntaxStructure,LF<:LeftmostLinearForm} = SyntaxTree
Base.promote_rule(::Type{LF}, ::Type{SS}) where {LF<:LeftmostLinearForm,SS<:AbstractSyntaxStructure} = SyntaxTree

############################################################################################

"""
    struct Literal{T<:AbstractSyntaxToken} <: AbstractSyntaxStructure
        ispos::Bool
        prop::T
    end

A proposition or its negation.

See also [`CNF`](@ref), [`DNF`](@ref), [`AbstractSyntaxStructure`](@ref).
"""
struct Literal{T<:AbstractSyntaxToken} <: AbstractSyntaxStructure
    ispos::Bool
    prop::T

    function Literal{T}(
        ispos::Bool,
        prop::T,
    ) where {T<:AbstractSyntaxToken}
        new{T}(ispos, prop)
    end

    function Literal(
        ispos::Bool,
        prop::T,
    ) where {T<:AbstractSyntaxToken}
        Literal{T}(ispos, prop)
    end
end

ispos(l::Literal) = l.ispos
prop(l::Literal) = l.prop

propositionstype(::Literal{T}) where {T} = T

tree(l::Literal) = ispos(l) ? SyntaxTree(l.prop) : ¬(SyntaxTree(l.prop))

hasdual(l::Literal) = true
dual(l::Literal) = Literal(!ispos(l), prop(l))

function Base.show(io::IO, l::Literal)
    println(io,
        "Literal{$(propositionstype(l))}: " * (ispos(l) ? "" : "¬") * syntaxstring(prop(l))
    )
end

############################################################################################

"""$(doc_lmlf)"""
const LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}
"""$(doc_lmlf)"""
const LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

"""$(doc_lmlf)"""
const CNF{SS<:AbstractSyntaxStructure} = LeftmostConjunctiveForm{LeftmostDisjunctiveForm{SS}}
"""$(doc_lmlf)"""
const DNF{SS<:AbstractSyntaxStructure} = LeftmostDisjunctiveForm{LeftmostConjunctiveForm{SS}}

conjuncts(m::Union{LeftmostConjunctiveForm,CNF}) = children(m)
nconjuncts(m::Union{LeftmostConjunctiveForm,CNF}) = nchildren(m)
disjuncts(m::Union{LeftmostDisjunctiveForm,DNF}) = children(m)
ndisjuncts(m::Union{LeftmostDisjunctiveForm,DNF}) = nchildren(m)

# conjuncts(m::DNF) = map(d->conjuncts(d), disjuncts(m))
# nconjuncts(m::DNF) = map(d->nconjuncts(d), disjuncts(m))
# disjuncts(m::CNF) = map(d->disjuncts(d), conjuncts(m))
# ndisjuncts(m::CNF) = map(d->ndisjuncts(d), conjuncts(m))

function joinformulas(
    op::AbstractOperator,
    tojoin::Vararg{LeftmostConjunctiveForm,N}
) where {N}
    @assert iscommutative(op) "Cannot join $(length(tojoin)) LeftmostConjunctiveForm's" *
        " by means of $(op) because the operator is not commutative"

    return LeftmostConjunctiveForm(op,reduce(vcat,children.(tojoin)))
end

############################################################################################

subtrees(tree::SyntaxTree) = [Iterators.flatten(_subtrees.(children(tree)))...]
_subtrees(tree::SyntaxTree) = [tree, Iterators.flatten(_subtrees.(children(tree)))...]

"""
    function treewalk(
        st::SyntaxTree,
        args...;
        rng::AbstractRNG = Random.GLOBAL_RNG,
        criterion::Function = ntokens,
        toleaf::Bool = true,
        returnnode::Bool = false,
        transformnode::Function = nothing,
    )::SyntaxTree

Return a subtree from passed SyntaxTree by following options:
 - `criterion`: function used to calculate the probability of stopping at a random node;
 - `returnnode`: true if only the subtree is to be returned;
 - `transformnode`: function that will be applied to the chosen subtree.
"""
function treewalk(
    st::SyntaxTree,
    args...;
    rng::AbstractRNG = Random.GLOBAL_RNG,
    criterion::Function = c->true,
    returnnode::Bool = false,
    transformnode::Union{Function,Nothing} = nothing,
)
    chs = children(st)

    return length(chs) == 0 ? begin
        isnothing(transformnode) ? st : transformnode(st, args...)
    end : begin
        c_chsub = map(c->length(filter(criterion, tokens(c))), chs)
        c_father = criterion(token(st)) ? 1 : 0

        @assert [c_chsub..., c_father] isa AbstractVector{<:Integer} "Not all values " *
        "calculated as criterion are integers, double check the passed function used for " *
        "calculating these; values: $([c_chsub..., c_father])"

        w_nodes = [c_chsub..., c_father]/sum([c_chsub..., c_father])
        idx_randnode = sample(rng, 1:length(w_nodes), Weights(w_nodes))

        if idx_randnode == length(w_nodes)
            isnothing(transformnode) ? st : transformnode(st, args...)
        else
            returnnode ?
                treewalk(
                    chs[idx_randnode],
                    args...;
                    rng=rng,
                    criterion=criterion,
                    returnnode=returnnode,
                    transformnode=transformnode,
                ) :
                SyntaxTree(
                    token(st),
                    (
                        chs[1:(idx_randnode-1)]...,
                        treewalk(
                            chs[idx_randnode],
                            args...;
                            rng=rng,
                            criterion=criterion,
                            returnnode=returnnode,
                            transformnode=transformnode,
                        ),
                        chs[(idx_randnode+1):end]...
                    )
                )
        end
    end
end
