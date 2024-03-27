import Base: show, promote_rule, length, getindex
using SoleBase

doc_lmlf = """
    struct LeftmostLinearForm{C<:Connective,SS<:AbstractSyntaxStructure} <: AbstractSyntaxStructure
        children::Vector{<:SS}
    end

A syntax structure representing the [`foldl`](https://en.wikipedia.org/wiki/Fold_(higher-order_function))
of a set of other syntax structure of type `SS` by means of a connective `C`.
This structure enables a structured instantiation of formulas in conjuctive/disjunctive forms, and
conjuctive normal form (CNF) or disjunctive normal form (DNF), defined as:

    const LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}
    const LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

    const CNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),LeftmostLinearForm{typeof(∨),SS}}
    const DNF{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),LeftmostLinearForm{typeof(∧),SS}}

# Examples
```julia-repl
julia> LeftmostLinearForm(→, parseformula.(["p", "q", "r"]))
LeftmostLinearForm{SoleLogics.NamedConnective{:→},Atom{String}}
    "(p) → (q) → (r)"

julia> LeftmostConjunctiveForm(parseformula.(["¬p", "q", "¬r"]))
LeftmostLinearForm{SoleLogics.NamedConnective{:∧},SyntaxTree}
    "(¬p) ∧ (q) ∧ (¬r)"

julia> LeftmostDisjunctiveForm{Literal}([Literal(false, Atom("p")), Literal(true, Atom("q")), Literal(false, Atom("r"))])
LeftmostLinearForm{SoleLogics.NamedConnective{:∨},Literal}
    "(¬p) ∨ (q) ∨ (¬r)"

julia> LeftmostDisjunctiveForm([LeftmostConjunctiveForm(parseformula.(["¬p", "q", "¬r"]))]) isa DNF
true

julia> conj = LeftmostConjunctiveForm(@atoms p q)
LeftmostConjunctiveForm with 2 Atom{String} children:
        p
        q

julia> tree(conj)
SyntaxBranch{NamedConnective{:∧}}: p ∧ q

julia> nconj = NEGATION(conj)
LeftmostLinearForm with connective ¬ and 1 LeftmostConjunctiveForm{Atom{String}} children:
        (p) ∧ (q)

julia> tree(nconj)
SyntaxBranch{NamedConnective{:¬}}: ¬(p ∧ q)

julia> tree(nconj ∧ nconj)
SyntaxBranch{NamedConnective{:∧}}: ¬(p ∧ q) ∧ ¬(p ∧ q)
```
"""

"""$(doc_lmlf)

See also [`AbstractSyntaxStructure`](@ref), [`SyntaxTree`](@ref),
[`LeftmostConjunctiveForm`](@ref), [`LeftmostDisjunctiveForm`](@ref),
[`Literal`](@ref).
"""
struct LeftmostLinearForm{C<:Connective,SS<:AbstractSyntaxStructure} <: AbstractSyntaxStructure
    children::Vector{SS}

    function LeftmostLinearForm{C,SS}(
        children::Vector,
    ) where {C<:Connective,SS<:AbstractSyntaxStructure}
        a = arity(C()) # TODO maybe add member connective::C and use that instead of C()
        n_children = length(children)

        length(children) > 0 || error("Cannot instantiate LeftmostLinearForm{$(C)} with no children.")

        if a == 1
            n_children == 1 ||
                error("Mismatching number of children ($n_children) and connective's arity ($a).")
        else
            h = (n_children-1)/(a-1)
            (isinteger(h) && h >= 0) ||
            # TODO figure out whether the base case n_children = 0 makes sense
                error("Mismatching number of children ($n_children) and connective's arity ($a).")
        end

        new{C,SS}(children)
    end

    function LeftmostLinearForm{C}(children::AbstractVector{SS}) where {C<:Connective,SS<:AbstractSyntaxStructure}
        # SS = SoleBase._typejoin(typeof.(children)...)
        LeftmostLinearForm{C,SS}(children)
    end

    # Ugly!!
    function LeftmostLinearForm{C}(children::AbstractVector) where {C<:Connective}
        SS = SoleBase._typejoin(typeof.(children)...)
        LeftmostLinearForm{C,SS}(children)
    end

    function LeftmostLinearForm(
        C::Type{<:SoleLogics.Connective},
        children::Vector,
    )
        LeftmostLinearForm{C}(children)
    end

    function LeftmostLinearForm(
        op::Connective,
        children::Vector,
    )
        LeftmostLinearForm(typeof(op), children)
    end

    function LeftmostLinearForm(
        tree::SyntaxTree,
        c::Union{<:SoleLogics.Connective,Nothing} = nothing
    )
        # Check c correctness; it should not be nothing (thus, auto inferred) if
        # tree root contains something that is not a connective
        if (!(token(tree) isa Connective) && !isnothing(c))
            error("Syntax tree cannot be converted to a LeftmostLinearForm. " *
                "tree root is $(token(tree)). " *
                "Try specifying a connective as a second argument."
            )
        end

        if isnothing(c)
            c = token(tree)
        end

        # Get a vector of `SyntaxTree`s, having `c` as common ancestor, then,
        # call LeftmostLinearForm constructor.
        _children = AbstractSyntaxStructure[]

        function _dig_and_retrieve(tree::SyntaxTree, c::SoleLogics.Connective)
            token(tree) != c ?
            push!(_children, tree) :    # Lexical scope
            for chs in children(tree)
                _dig_and_retrieve(chs, c)
            end
        end
        _dig_and_retrieve(tree, c)

        LeftmostLinearForm(c, _children)
    end

    function LeftmostLinearForm{C}(tree::SyntaxTree) where {C<:Connective}
        LeftmostLinearForm(tree, C()) # TODO avoid
    end
end

children(lf::LeftmostLinearForm) = lf.children
connective(::LeftmostLinearForm{C}) where {C} = C() # TODO avoid

operatortype(::LeftmostLinearForm{C}) where {C} = C
childrentype(::LeftmostLinearForm{C,SS}) where {C,SS} = SS

nchildren(lf::LeftmostLinearForm) = length(children(lf))

Base.length(lf::LeftmostLinearForm) = Base.length(children(lf))
function Base.getindex(
    lf::LeftmostLinearForm{C,SS},
    idxs::AbstractVector
) where {C,SS}
    return LeftmostLinearForm{C,SS}(children(lf)[idxs])
end
Base.getindex(lf::LeftmostLinearForm, idx::Integer) = Base.getindex(lf,[idx])
Base.push!(lf::LeftmostLinearForm, el) = Base.push!(children(lf), el)

function composeformulas(c::Connective, φs::NTuple{N,LeftmostLinearForm}) where {N}
    if all(_c->_c == c, connective.(φs)) # If operator is the same, collapse children
        return LeftmostLinearForm(c, collect(Iterators.flatten(children.(φs))))
        # return LeftmostLinearForm(c, reduce(vcat,children.(φs)))
    else
        return LeftmostLinearForm(c, collect(φs))
    end
end

# TODO: add parameter remove_redundant_parentheses
# TODO: add parameter parenthesize_atoms
function syntaxstring(
    lf::LeftmostLinearForm;
    function_notation = false,
    kwargs...,
)
    if function_notation
        syntaxstring(tree(lf); function_notation = function_notation, kwargs...)
    else
        chs = children(lf)
        children_ss = map(
            c->syntaxstring(c; kwargs...),
            chs
        )
        "(" * join(children_ss, ") $(syntaxstring(connective(lf); kwargs...)) (") * ")"
    end
end

function tree(lf::LeftmostLinearForm)
    c = connective(lf)
    a = arity(c)
    chs = children(lf)

    st = begin
        if length(chs) == 1 # Only child
            if a == 1
                c(tree(first(chs)))
            else
                tree(first(chs))
            end
        else
            function _tree(φs::Vector{<:SyntaxTree})
                @assert (length(φs) != 0) "$(φs); $(lf); $(c); $(a)."
                return length(φs) == a ?
                    SyntaxTree(c, φs...) :
                    SyntaxTree(c, φs[1:(a-1)]..., _tree(φs[a:end])) # Left-most unwinding
            end
            _tree(tree.(chs))
        end
    end

    return st
end

function Base.show(io::IO, lf::LeftmostLinearForm{C,SS}) where {C,SS}
    if lf isa CNF
        print(io, "CNF with")
        println(io, " $(nconjuncts(lf)) conjuncts:")
        L = literaltype(lf)
        L <: Literal || println(io, " $(nconjuncts(lf)) and literals of type $(L):")
    elseif lf isa DNF
        print(io, "DNF with")
        println(io, " $(ndisjuncts(lf)) disjuncts:")
        L = literaltype(lf)
        L <: Literal || println(io, " $(ndisjuncts(lf)) and literals of type $(L):")
    else
        if lf isa LeftmostConjunctiveForm
            print(io, "LeftmostConjunctiveForm with")
        elseif lf isa LeftmostDisjunctiveForm
            print(io, "LeftmostDisjunctiveForm with")
        else
            print(io, "LeftmostLinearForm with connective $(syntaxstring(connective(lf))) and")
        end
        println(io, " $(nchildren(lf)) $((SS == AbstractSyntaxStructure ? "" : "$(SS) "))children:")
    end
    # println(io, "\t$(join(syntaxstring.(children(lf)), " $(syntaxstring(connective(lf))) \n\t"))")
    println(io, "\t$(join(syntaxstring.(children(lf)), "\n\t"))")
end

# TODO fix
Base.promote_rule(::Type{<:LeftmostLinearForm}, ::Type{<:LeftmostLinearForm}) = SyntaxTree
Base.promote_rule(::Type{SS}, ::Type{LF}) where {SS<:AbstractSyntaxStructure,LF<:LeftmostLinearForm} = SyntaxTree
Base.promote_rule(::Type{LF}, ::Type{SS}) where {LF<:LeftmostLinearForm,SS<:AbstractSyntaxStructure} = SyntaxTree

Base.promote_rule(::Type{LF}, ::Type{SS}) where {LF<:LeftmostLinearForm,SS<:SyntaxTree} = SyntaxTree
Base.promote_rule(::Type{SS}, ::Type{LF}) where {LF<:LeftmostLinearForm,SS<:SyntaxTree} = SyntaxTree

############################################################################################

# TODO actually:
# const CNF{SS<:AbstractSyntaxStructure} = Union{LeftmostLinearForm{typeof(∧),LeftmostLinearForm{typeof(∨),SS}},LeftmostLinearForm{typeof(∨),SS}}
# const DNF{SS<:AbstractSyntaxStructure} = Union{LeftmostLinearForm{typeof(∨),LeftmostLinearForm{typeof(∧),SS}},LeftmostLinearForm{typeof(∧),SS}}

"""
    LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}

Specific instantiation of a [`LeftmostLinearForm`](@ref), where [`Connective`](@ref)s are
all [`CONJUNCTION`](@ref)s.

See also [`AbstractSyntaxStructure`](@ref), [`Connective`](@ref), [`LeftmostLinearForm`](@ref),
[`CONJUNCTION`](@ref).
"""
const LeftmostConjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}

function check(
    φ::LeftmostConjunctiveForm,
    args...;
    kwargs...
)
    return all(ch -> check(ch, args; kwargs...), children(φ))
end

"""
    LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

Specific instantiation of a [`LeftmostLinearForm`](@ref), where [`Connective`](@ref)s are
all [`DISJUNCTION`](@ref)s.

See also [`AbstractSyntaxStructure`](@ref), [`Connective`](@ref),
[`LeftmostLinearForm`](@ref), [`DISJUNCTION`](@ref).
"""
const LeftmostDisjunctiveForm{SS<:AbstractSyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

function check(
    φ::LeftmostDisjunctiveForm,
    args...;
    kwargs...
)
    return any(ch -> check(ch, args; kwargs...), children(φ))
end

"""
    CNF{SS<:AbstractSyntaxStructure} = LeftmostConjunctiveForm{LeftmostDisjunctiveForm{SS}}

Conjunctive Normal Form of an [`AbstractSyntaxStructure`](@ref).

See also [`AbstractSyntaxStructure`](@ref), [`LeftmostConjunctiveForm`](@ref),
[`LeftmostDisjunctiveForm`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref).
"""
const CNF{SS<:AbstractSyntaxStructure} = LeftmostConjunctiveForm{LeftmostDisjunctiveForm{SS}}

function check(
    φ::CNF,
    args...;
    kwargs...
)
    return all(ch -> any(grandch -> check(grandch, args; kwargs...), children(ch)), children(φ))
end

"""
    DNF{SS<:AbstractSyntaxStructure} = LeftmostConjunctiveForm{LeftmostConjunctiveForm{SS}}

Disjunctive Normal Form of an [`AbstractSyntaxStructure`](@ref).

See also [`AbstractSyntaxStructure`](@ref), [`LeftmostConjunctiveForm`](@ref),
[`LeftmostDisjunctiveForm`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref).
"""
const DNF{SS<:AbstractSyntaxStructure} = LeftmostDisjunctiveForm{LeftmostConjunctiveForm{SS}}

function check(
    φ::DNF,
    args...;
    kwargs...
)
    return any(ch -> all(grandch -> check(grandch, args; kwargs...), children(ch)), children(φ))
end

# Helpers
function CNF(conjuncts::AbstractVector{<:LeftmostDisjunctiveForm})
    SS = Union{childrentype.(conjuncts)...}
    return CNF{SS}(conjuncts)
end
function DNF(disjuncts::AbstractVector{<:LeftmostConjunctiveForm})
    SS = Union{childrentype.(disjuncts)...}
    return DNF{SS}(disjuncts)
end
CNF(conjuncts::NTuple{N,<:LeftmostDisjunctiveForm}) where {N} = CNF(collect(conjuncts))
DNF(disjuncts::NTuple{N,<:LeftmostConjunctiveForm}) where {N} = DNF(collect(disjuncts))
CNF(conjuncts::Vararg{LeftmostDisjunctiveForm}) = CNF(collect(conjuncts))
DNF(disjuncts::Vararg{LeftmostConjunctiveForm}) = DNF(collect(disjuncts))
CNF(conjunct::LeftmostDisjunctiveForm) = CNF([conjunct])
DNF(disjunct::LeftmostConjunctiveForm) = DNF([disjunct])

function CNF(φ::SyntaxLeaf)
    return LeftmostConjunctiveForm([LeftmostDisjunctiveForm([φ])])
end

function DNF(φ::SyntaxLeaf)
    return LeftmostDisjunctiveForm([LeftmostConjunctiveForm([φ])])
end

function CNF(φ::SyntaxBranch)
    return erorr("TODO")
end

function DNF(φ::SyntaxBranch)
    return erorr("TODO")
end


literaltype(::CNF{SS}) where {SS<:AbstractSyntaxStructure} = SS
literaltype(::DNF{SS}) where {SS<:AbstractSyntaxStructure} = SS

# # TODO maybe not needed?
# Base.promote_rule(::Type{<:LeftmostConjunctiveForm}, ::Type{<:LeftmostConjunctiveForm}) = LeftmostConjunctiveForm
# Base.promote_rule(::Type{<:LeftmostDisjunctiveForm}, ::Type{<:LeftmostDisjunctiveForm}) = LeftmostDisjunctiveForm
# Base.promote_rule(::Type{<:LeftmostConjunctiveForm}, ::Type{<:LeftmostDisjunctiveForm}) = SyntaxTree

conjuncts(φ::LeftmostConjunctiveForm) = children(φ)
nconjuncts(φ::LeftmostConjunctiveForm) = nchildren(φ)
pushconjunct!(φ::LeftmostLinearForm, el) = Base.push!(children(φ), el)

disjuncts(φ::LeftmostDisjunctiveForm) = children(φ)
ndisjuncts(φ::LeftmostDisjunctiveForm) = nchildren(φ)
pushdisjunct(φ::LeftmostDisjunctiveForm, el) = Base.push!(children(φ), el)

# conjuncts(φ::DNF) = map(d->conjuncts(d), disjuncts(φ))
# nconjuncts(φ::DNF) = map(d->nconjuncts(d), disjuncts(φ))
# disjuncts(φ::CNF) = map(d->disjuncts(d), conjuncts(φ))
# ndisjuncts(φ::CNF) = map(d->ndisjuncts(d), conjuncts(φ))


############################################################################################

"""
    struct Literal{T<:SyntaxLeaf} <: AbstractSyntaxStructure
        ispos::Bool
        prop::T
    end

An atom, or its negation.

See also [`CNF`](@ref), [`DNF`](@ref), [`AbstractSyntaxStructure`](@ref).
"""
struct Literal{T<:SyntaxLeaf} <: AbstractSyntaxStructure
    ispos::Bool
    prop::T

    function Literal{T}(
        ispos::Bool,
        prop::T,
    ) where {T<:SyntaxLeaf}
        new{T}(ispos, prop)
    end

    function Literal(
        ispos::Bool,
        prop::T,
    ) where {T<:SyntaxLeaf}
        Literal{T}(ispos, prop)
    end

    function Literal(φ::SyntaxLeaf, flag = true)
        return Literal(flag, φ)
    end
    function Literal(φ::SyntaxBranch{typeof(¬)}, flag = true)
        ch = first(children(φ))
        @assert ch isa Union{SyntaxLeaf,SyntaxBranch{typeof(¬)}} "Cannot " *
        # @assert ch isa SyntaxLeaf "Cannot " *
            "construct Literal with formula of type $(typeof(ch))): $(syntaxstring(ch))."
        return Literal(ch, !flag)
    end
end

ispos(l::Literal) = l.ispos
prop(l::Literal) = l.prop

atomstype(::Literal{T}) where {T} = T

tree(l::Literal) = ispos(l) ? l.prop : ¬(l.prop)

hasdual(l::Literal) = true
dual(l::Literal) = Literal(!ispos(l), prop(l))

function Base.show(io::IO, l::Literal)
    println(io,
        "Literal{$(atomstype(l))}: " * (ispos(l) ? "" : "¬") * syntaxstring(prop(l))
    )
end

############################################################################################
# CNF conversion
############################################################################################

"""
    cnf(φ::Formula, literaltype = Literal; kwargs...)

    TODO docstring. Converts to cnf form ([`CNF`](@ref)).
    `CNF{literaltype}`
    Additional `kwargs` are passed to [`normalize`](@ref)
"""
function cnf(φ::Formula, literaltype = Literal; kwargs...)
    return _cnf(normalize(φ; profile = :nnf, kwargs...), literaltype)
end

function cnf(φ::CNF{T}, literaltype = Literal; kwargs...) where {T<:AbstractSyntaxStructure}
    if T == literaltype
        return φ
    else
        return cnf(tree(φ), literaltype; kwargs...)
    end
end

function cnf(φ::DNF, args...; kwargs...)
    return cnf(tree(φ), args...; kwargs...)
end


function _cnf(φ::Formula, literaltype = Literal)
    return error("Cannot convert to CNF formula of type $(typeof(φ)): $(syntaxstring(φ))")
end

function _cnf(φ::Union{SyntaxLeaf,SyntaxBranch{typeof(¬)}}, literaltype = Literal)
    φ = φ isa literaltype ? φ : literaltype(φ)
    return LeftmostConjunctiveForm([LeftmostDisjunctiveForm{literaltype}([φ])])
end

function _cnf(φ::SyntaxBranch{typeof(∧)}, literaltype = Literal)
    return _cnf(first(children(φ)), literaltype) ∧ _cnf(last(children(φ)), literaltype)
end

function _cnf(φ::SyntaxBranch{typeof(∨)}, literaltype = Literal)
    conjs = vec([begin
        # @show typeof(c1), typeof(c2)
        # @show typeof(c1 ∨ c2)
        # LeftmostDisjunctiveForm{literaltype}(c1 ∨ c2)
        c1 ∨ c2
    end for (c1,c2) in Iterators.product(conjuncts(_cnf(first(children(φ)), literaltype)),conjuncts(_cnf(last(children(φ)), literaltype)))])
    # @show typeof.(conjs)
    # conjs = Vector{LeftmostDisjunctiveForm{literaltype}}(conjs)
    LeftmostConjunctiveForm(conjs)
end



############################################################################################

subtrees(tree::SyntaxTree) = [Iterators.flatten(_subtrees.(children(tree)))...]
_subtrees(tree::SyntaxTree) = [tree, Iterators.flatten(_subtrees.(children(tree)))...]

# TODO: explain better
# TODO: is this available in AbstractTrees?
"""
    treewalk(
        st::SyntaxTree,
        args...;
        rng::AbstractRNG = Random.GLOBAL_RNG,
        criterion::Function = ntokens,
        toleaf::Bool = true,
        returnnode::Bool = false,
        transformnode::Function = nothing
    )::SyntaxTree

Return a subtree of syntax tree, by following these options:
 - `criterion`: function used to compute the probability of stopping at a random node;
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
        "computed as criterion are integers, double check the passed function used for " *
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



"""
    subformulas(f::Formula; sorted=true)

Return all sub-formulas (sorted by size when `sorted=true`)
of a given formula.

# Examples
```julia-repl
julia> syntaxstring.(SoleLogics.subformulas(parseformula("◊((p∧q)→r)")))
6-element Vector{String}:
 "p"
 "q"
 "r"
 "p ∧ q"
 "◊(p ∧ q)"
 "(◊(p ∧ q)) → r"
```

See also
[`SyntaxTree`](@ref)), [`Formula`](@ref).
"""
subformulas(f::Formula, args...; kwargs...) = subformulas(tree(f), args...; kwargs...)
function subformulas(t::SyntaxTree; sorted=true)
    # function _subformulas(_t::SyntaxTree)
    #     SyntaxTree[
    #         (map(SyntaxTree, Iterators.flatten(subformulas.(children(_t)))))...,
    #         _t
    #     ]
    # end
    function _subformulas(_t::SyntaxTree)
        SyntaxTree[
            (Iterators.flatten(subformulas.(children(_t))))...,
            _t
        ]
    end
    ts = _subformulas(t)
    if sorted
        sort!(ts, by = t -> SoleLogics.height(t))
    end
    ts
end

# TODO move to utils and rename "normalize" -> "transform"/"reshape"/"simplify"
# TODO \to diventano \lor
# TODO explain profile's and other parameters
"""
    normalize(
        f::Formula;
        profile = :readability,
        remove_boxes = nothing,
        reduce_negations = true,
        simplify_constants = true,
        allow_atom_flipping = false,
        prefer_implications = false,
        remove_implications = false,
        forced_negation_removal = nothing,
        remove_identities = true,
        unify_toones = true,
        rotate_commutatives = true,
    )

Return a modified version of a given formula, that has the same semantics
but different syntax. This is useful for simplifying formulas for readability,
or when checking the truth of many
(possibly semantically similar) formulas; for example, when performing
[model checking](https://en.wikipedia.org/wiki/Model_checking).
The current implementation assumes the underlying algebra is Boolean!

# Arguments
- `f::Formula`: when set to `true`,
    the formula;
- `profile::Symbol`: possible values are :readability, which optimizes for qualitative
    simplicity for a human to understand, and :modelchecking, which optimizes
    model checking speed;
- `remove_boxes::Bool`: remove all (non-relational and relational) box operators by using the
    equivalence ◊φ ≡ ¬□¬φ. Note: this assumes an underlying Boolean algebra.
- `reduce_negations::Bool`: when set to `true`,
    attempts at reducing the number of negations by appling
    some transformation rules
    (e.g., [De Morgan's laws](https://en.wikipedia.org/wiki/De_Morgan%27s_laws)).
    Note: this assumes an underlying Boolean algebra.
- `allow_atom_flipping::Bool`: when set to `true`,
    together with `reduce_negations=true`, this may cause the negation of an atom
    to be replaced with the its [`dual`](@ref) atom.

# Examples
```julia-repl
julia> f = parseformula("□¬((p∧¬q)→r)∧⊤");

julia> syntaxstring(f)
"□¬((p ∧ ¬q) → r) ∧ ⊤"

julia> syntaxstring(SoleLogics.normalize(f; profile = :modelchecking, allow_atom_flipping = false))
"¬◊(q ∨ ¬p ∨ r)"

julia> syntaxstring(SoleLogics.normalize(f; profile = :readability, allow_atom_flipping = false))
"□(¬r ∧ p ∧ ¬q)"
```

See also
[`SyntaxTree`](@ref)), [`Formula`](@ref).
"""
normalize(f::Formula, args...; kwargs...) = normalize(tree(f), args...; kwargs...)
function normalize(
    t::SyntaxTree;
    profile = :readability,
    remove_boxes = nothing,
    reduce_negations = nothing,
    simplify_constants = nothing,
    allow_atom_flipping = nothing,
    prefer_implications = nothing,
    remove_implications = nothing,
    forced_negation_removal = nothing,
    remove_identities = nothing,
    unify_toones = nothing,
    rotate_commutatives = nothing,
)
    if profile == :readability
        if isnothing(remove_boxes)               remove_boxes = false end
        if isnothing(reduce_negations)           reduce_negations = true end
        if isnothing(simplify_constants)         simplify_constants = true end
        if isnothing(allow_atom_flipping)        allow_atom_flipping = false end
        if isnothing(prefer_implications)        prefer_implications = false end
        if isnothing(remove_implications)        remove_implications = false end
        if isnothing(remove_identities)          remove_identities = true end
        if isnothing(unify_toones)               unify_toones = true end
        if isnothing(rotate_commutatives)        rotate_commutatives = true end
        # TODO leave \to's instead of replacing them with \lor's...
    elseif profile == :nnf
        if isnothing(remove_boxes)               remove_boxes = false end
        if isnothing(reduce_negations)           reduce_negations = true end
        if isnothing(simplify_constants)         simplify_constants = false end
        if isnothing(allow_atom_flipping)        allow_atom_flipping = false end
        if isnothing(prefer_implications)        prefer_implications = false end
        if isnothing(remove_implications)        remove_implications = true end
        if isnothing(remove_identities)          remove_identities = false end
        if isnothing(unify_toones)               unify_toones = false end
        if isnothing(rotate_commutatives)        rotate_commutatives = true end
        # TODO leave \to's instead of replacing them with \lor's...
    elseif profile == :modelchecking
        if isnothing(remove_boxes)               remove_boxes = true end
        if isnothing(reduce_negations)           reduce_negations = true end
        if isnothing(simplify_constants)         simplify_constants = true end
        if isnothing(allow_atom_flipping)        allow_atom_flipping = false end
        if isnothing(prefer_implications)        prefer_implications = false end
        if isnothing(remove_implications)        remove_implications = false end
        if isnothing(remove_identities)          remove_identities = true end
        if isnothing(unify_toones)               unify_toones = true end
        if isnothing(rotate_commutatives)        rotate_commutatives = true end
    else
        error("Unknown normalization profile: $(repr(profile))")
    end

    if isnothing(forced_negation_removal)
        if isnothing(allow_atom_flipping)
            forced_negation_removal = true
        else
            forced_negation_removal = false
        end
    end

    # TODO we're currently assuming Boolean algebra!!! Very wrong assumption...

    _normalize = t->normalize(t;
        profile = profile,
        remove_boxes = remove_boxes,
        reduce_negations = reduce_negations,
        simplify_constants = simplify_constants,
        allow_atom_flipping = allow_atom_flipping,
        prefer_implications = prefer_implications,
        remove_implications = remove_implications,
        forced_negation_removal = forced_negation_removal,
        remove_identities = remove_identities,
        unify_toones = unify_toones,
        rotate_commutatives = rotate_commutatives
    )

    newt = t

    # Remove modal connectives based on the identity relation
    newt = begin
        tok, chs = token(newt), children(newt)
        if remove_identities && tok isa AbstractRelationalConnective &&
            relation(tok) == identityrel && arity(tok) == 1
            first(chs)
        elseif unify_toones && tok isa AbstractRelationalConnective &&
            istoone(relation(tok)) && arity(tok) == 1
            diamond(relation(tok))(first(chs))
        else
            newt
        end
    end

    # Simplify
    newt = begin
        tok, chs = token(newt), children(newt)
        if (tok == ¬) && arity(tok) == 1
            child = chs[1]
            chtok, grandchildren = token(child), children(child)
            if reduce_negations && (chtok == ¬) && arity(chtok) == 1
                _normalize(grandchildren[1])
            elseif reduce_negations && (chtok == ∨) && arity(chtok) == 2
                ∧(_normalize(¬(grandchildren[1])), _normalize(¬(grandchildren[2])))
                # TODO use implication, maybe it's more interpretable?
            elseif reduce_negations && (chtok == ∧) && arity(chtok) == 2
                # if prefer_implications
                #     →(_normalize(grandchildren[1]), _normalize(¬(grandchildren[2])))
                # else
                ∨(_normalize(¬(grandchildren[1])), _normalize(¬(grandchildren[2])))
                # end
            elseif reduce_negations && (chtok == →) && arity(chtok) == 2
                # _normalize(∨(¬(grandchildren[1]), grandchildren[2]))
                ∧(_normalize(grandchildren[1]), _normalize(¬(grandchildren[2])))
            elseif reduce_negations && chtok isa Atom
                if allow_atom_flipping && hasdual(chtok)
                    dual(chtok)
                else
                    ¬(_normalize(child))
                end
            # elseif reduce_negations && chtok isa SoleLogics.AbstractRelationalConnective && arity(chtok) == 1
            #     dual_op = dual(chtok)
            #     if remove_boxes && dual_op isa SoleLogics.BoxRelationalConnective
            #         ¬(_normalize(child))
            #     else
            #         dual_op(_normalize(¬(grandchildren[1])))
            #     end
            elseif reduce_negations && ismodal(chtok) && arity(chtok) == 1
                dual_op = dual(chtok)
                # if remove_boxes && SoleLogics.isbox(dual_op)
                #     ¬(_normalize(child))
                # else
                dual_op(_normalize(¬(grandchildren[1])))
                # end
            elseif (reduce_negations || simplify_constants) && chtok == ⊤ && arity(chtok) == 1
                ⊥
            elseif (reduce_negations || simplify_constants) && chtok == ⊥ && arity(chtok) == 1
                ⊤
            elseif !forced_negation_removal
                SyntaxTree(tok, _normalize.(chs))
            else
                error("Unknown chtok when removing negations: $(chtok) (type = $(typeof(chtok)))")
            end
        else
            SyntaxTree(tok, _normalize.(chs))
        end
    end

    # DEBUG: old_newt = newt
    # Simplify constants
    newt = begin
        tok, chs = token(newt), children(newt)
        if simplify_constants && tok isa Connective
            if (tok == ∨) && arity(tok) == 2 # TODO maybe use istop, isbot?
                if     token(chs[1]) == ⊥  chs[2]          # ⊥ ∨ φ ≡ φ
                elseif token(chs[2]) == ⊥  chs[1]          # φ ∨ ⊥ ≡ φ
                elseif token(chs[1]) == ⊤  ⊤              # ⊤ ∨ φ ≡ ⊤
                elseif token(chs[2]) == ⊤  ⊤              # φ ∨ ⊤ ≡ ⊤
                else                       newt
                end
            elseif (tok == ∧) && arity(tok) == 2
                if     token(chs[1]) == ⊥  ⊥              # ⊥ ∧ φ ≡ ⊥
                elseif token(chs[2]) == ⊥  ⊥              # φ ∧ ⊥ ≡ ⊥
                elseif token(chs[1]) == ⊤  chs[2]          # ⊤ ∧ φ ≡ φ
                elseif token(chs[2]) == ⊤  chs[1]          # φ ∧ ⊤ ≡ φ
                else                       newt
                end
            elseif (tok == →) && arity(tok) == 2
                if     token(chs[1]) == ⊥  ⊤                   # ⊥ → φ ≡ ⊤
                elseif token(chs[2]) == ⊥  _normalize(¬chs[1])  # φ → ⊥ ≡ ¬φ
                elseif token(chs[1]) == ⊤  chs[2]               # ⊤ → φ ≡ φ
                elseif token(chs[2]) == ⊤  ⊤                   # φ → ⊤ ≡ ⊤
                else                       newt
                end
            elseif (tok == ¬) && arity(tok) == 1
                if     token(chs[1]) == ⊤  ⊥
                elseif token(chs[1]) == ⊥  ⊤
                else                       newt
                end
            elseif SoleLogics.isbox(tok) && arity(tok) == 1
                if     token(chs[1]) == ⊤  ⊤
                else                       newt
                end
            elseif SoleLogics.isdiamond(tok) && arity(tok) == 1
                if     token(chs[1]) == ⊥  ⊥
                else                       newt
                end
            else
                newt
            end
        else
            newt
        end
    end

    # Implication <-> disjunction
    newt = begin
        tok, chs = token(newt), children(newt)
        if prefer_implications && (tok == ∨)
            if token(chs[1]) == ¬
                →(_normalize(first(children(chs[1]))), _normalize((chs[2])))
            else
                →(_normalize(¬chs[1]), _normalize((chs[2])))
            end
        elseif remove_implications && (tok == →)
            if token(chs[1]) == ¬
                ∨(_normalize(first(children(chs[1]))), _normalize((chs[2])))
            else
                ∨(_normalize(¬chs[1]), _normalize((chs[2])))
            end
        else
            newt
        end
    end
    # DEBUG:
    # "$(syntaxstring(old_newt)) => $(syntaxstring(newt))" |> println

    newt = begin
        tok, chs = token(newt), children(newt)
        if remove_boxes && tok isa Connective && SoleLogics.isbox(tok) && arity(tok) == 1
            # remove_boxes -> substitute every [X]φ with ¬⟨X⟩¬φ
            child = chs[1]
            dual_op = dual(tok)
            ¬(dual_op(_normalize(¬child)))
            # TODO remove
            # if relation(tok) == globalrel
            #     # Special case: [G]φ -> ⟨G⟩φ
            #     dual_op(_normalize(child))
            # else
            #     ¬(dual_op(_normalize(¬child)))
            # end
        else
            newt
        end
    end

    function _isless(st1::SyntaxTree, st2::SyntaxTree)
        isless(Base.hash(st1), Base.hash(st2))
    end

    # Rotate commutatives
    if rotate_commutatives
        newt = begin
            tok, chs = token(newt), children(newt)
            if tok isa Connective && iscommutative(tok) && arity(tok) > 1
                chs = children(LeftmostLinearForm(newt, tok))
                chs = Vector(sort(collect(_normalize.(chs)), lt=_isless))
                if tok in [∧,∨] # TODO create trait for this behavior: p ∧ p ∧ p ∧ q   -> p ∧ q
                    chs = unique(chs)
                end
                tree(LeftmostLinearForm(tok, chs))
            else
                SyntaxTree(tok, chs)
            end
        end
    end

    return newt
end

"""
    isgrounded(f::Formula)::Bool

Return `true` if the formula is grounded, that is, if it can be inferred from its syntactic
structure that, given any frame-based model, the truth value of the formula is the same
on every world.

# Examples
```julia-repl
julia> f = parseformula("⟨G⟩p → [G]q");

julia> syntaxstring(f)
"(⟨G⟩p) → ([G]q)"

julia> SoleLogics.isgrounded(f)
true
```

See also
[`isgrounding`](@ref)), [`SyntaxTree`](@ref)), [`Formula`](@ref).
"""
isgrounded(f::Formula) = isgrounded(tree(f))
function isgrounded(t::SyntaxTree)::Bool
    # (println(token(t)); println(children(t)); true) &&
    return (token(t) isa SoleLogics.AbstractRelationalConnective && isgrounding(relation(token(t)))) ||
    # (token(t) in [◊,□]) ||
    (token(t) isa Connective && all(c->isgrounded(c), children(t)))
end
