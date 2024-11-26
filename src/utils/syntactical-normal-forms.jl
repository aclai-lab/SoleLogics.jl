
############################################################################################
# import Base: show, promote_rule, length, getindex
# using SoleBase

doc_lmlf = """
    struct LeftmostLinearForm{C<:Connective,SS<:SyntaxStructure} <: SyntaxStructure
        grandchildren::Vector{<:SS}
    end

A syntax structure representing the [`foldl`](https://en.wikipedia.org/wiki/Fold_(higher-order_function))
of a set of other syntax structure of type `SS` by means of a connective `C`.
This structure enables a structured instantiation of formulas in conjuctive/disjunctive forms, and
conjuctive normal form (CNF) or disjunctive normal form (DNF), defined as:

    const LeftmostConjunctiveForm{SS<:SyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}
    const LeftmostDisjunctiveForm{SS<:SyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

    const CNF{SS<:SyntaxStructure} = LeftmostLinearForm{typeof(∧),LeftmostLinearForm{typeof(∨),SS}}
    const DNF{SS<:SyntaxStructure} = LeftmostLinearForm{typeof(∨),LeftmostLinearForm{typeof(∧),SS}}

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
LeftmostConjunctiveForm with 2 Atom{String} grandchildren:
        p
        q

julia> tree(conj)
SyntaxBranch: p ∧ q

julia> nconj = NEGATION(conj)
LeftmostLinearForm with connective ¬ and 1 LeftmostConjunctiveForm{Atom{String}} grandchildren:
        (p) ∧ (q)

julia> tree(nconj)
SyntaxBranch: ¬(p ∧ q)

julia> tree(nconj ∧ nconj)
SyntaxBranch: ¬(p ∧ q) ∧ ¬(p ∧ q)
```
"""

"""$(doc_lmlf)

See also [`SyntaxStructure`](@ref), [`SyntaxTree`](@ref),
[`LeftmostConjunctiveForm`](@ref), [`LeftmostDisjunctiveForm`](@ref),
[`Literal`](@ref).
"""
struct LeftmostLinearForm{C<:Connective,SS<:SyntaxStructure} <: SyntaxStructure
    grandchildren::Vector{SS}

    function LeftmostLinearForm{C,SS}(
        grandchildren::Vector,
        allow_empty::Bool = false,
    ) where {C<:Connective,SS<:SyntaxStructure}
        a = arity(C()) # TODO maybe add member connective::C and use that instead of C()
        n_children = length(grandchildren)

        if !allow_empty
            length(grandchildren) > 0 || error("Cannot instantiate LeftmostLinearForm{$(C)} with no grandchildren.")

            if a == 1
                n_children == 1 ||
                    error("Mismatching number of grandchildren ($n_children) and connective's arity ($a).")
            else
                h = (n_children-1)/(a-1)
                (isinteger(h) && h >= 0) ||
                # TODO figure out whether the base case n_children = 0 makes sense
                    error("Mismatching number of grandchildren ($n_children) and connective's arity ($a).")
            end
        end

        new{C,SS}(grandchildren)
    end

    function LeftmostLinearForm{C}(grandchildren::AbstractVector{SS}, args...) where {C<:Connective,SS<:SyntaxStructure}
        # SS = SoleBase._typejoin(typeof.(grandchildren)...)
        LeftmostLinearForm{C,SS}(grandchildren, args...)
    end

    # Ugly!!
    function LeftmostLinearForm{C}(
        grandchildren::AbstractVector,
        allow_empty::Bool = false,
        args...
    ) where {C<:Connective}
        allow_empty || length(grandchildren) > 0 || error("Cannot instantiate LeftmostLinearForm{$(C)} with no grandchildren.")
        SS = SoleBase._typejoin(typeof.(grandchildren)...)
        LeftmostLinearForm{C,SS}(grandchildren, allow_empty, args...)
    end

    function LeftmostLinearForm(
        op::C,
        grandchildren::Vector,
        args...
    ) where {C<:SoleLogics.Connective}
        LeftmostLinearForm{C}(grandchildren, args...)
    end

    function LeftmostLinearForm(
        tree::SyntaxTree,
        c::Union{Nothing,<:SoleLogics.Connective} = nothing,
        args...
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
        _children = SyntaxStructure[]

        function _dig_and_retrieve(tree::SyntaxTree, c::SoleLogics.Connective)
            token(tree) != c ?
            push!(_children, tree) :    # Lexical scope
            for chs in children(tree)
                _dig_and_retrieve(chs, c)
            end
        end
        _dig_and_retrieve(tree, c)

        LeftmostLinearForm(c, _children, args...)
    end

    function LeftmostLinearForm{C}(tree::SyntaxTree, args...) where {C<:Connective}
        LeftmostLinearForm(tree, C(), args...) # TODO avoid
    end
end

grandchildren(lf::LeftmostLinearForm) = lf.grandchildren
ngrandchildren(lf::LeftmostLinearForm) = length(grandchildren(lf))
connective(::LeftmostLinearForm{C}) where {C} = C() # TODO avoid using C alone, since it may not be a singleton.

operatortype(::LeftmostLinearForm{C}) where {C} = C
grandchildrentype(::LeftmostLinearForm{C,SS}) where {C,SS} = SS

# AbstractTrees.children (from Formula interface)
# children(lf::LeftmostLinearForm) = error("TODO implement.")
# TODO not quite true.
children(lf::LeftmostLinearForm) = grandchildren(lf)
token(lf::LeftmostLinearForm) = connective(lf)


@forward LeftmostLinearForm.grandchildren (
    Base.length,
    Base.setindex!,
    Base.push!,
    Base.pushfirst!,
    Base.append!,
    Base.iterate, Base.IteratorSize, Base.IteratorEltype,
    Base.firstindex, Base.lastindex,
    Base.keys, Base.values,
)

# function Base.getindex(lf::LeftmostLinearForm{C,SS}, idxs::AbstractVector) where {C,SS}
    # return LeftmostLinearForm{C,SS}(grandchildren(lf)[idxs])
# end
# Base.getindex(lf::LeftmostLinearForm, idx::Integer) = Base.getindex(lf,[idx])
function Base.getindex(lf::LeftmostLinearForm, idxs::AbstractVector)
    return LeftmostLinearForm(grandchildren(lf)[idxs])
end
Base.getindex(lf::LeftmostLinearForm, idx::Integer) = Base.getindex(grandchildren(lf),idx)
Base.push!(lf::LeftmostLinearForm, el) = Base.push!(grandchildren(lf), el)

function composeformulas(c::Connective, φs::NTuple{N,LeftmostLinearForm}) where {N}
    # @show φs
    if all(_c->_c == c, connective.(φs)) # If operator is the same, collapse grandchildren TODO and operator is ... associative?
        return LeftmostLinearForm(c, collect(Iterators.flatten(grandchildren.(φs))))
        # return LeftmostLinearForm(c, reduce(vcat,grandchildren.(φs)))
    else
        return LeftmostLinearForm(c, collect(φs))
    end
end

# TODO: add parameter remove_redundant_parentheses
# TODO: add parameter parenthesize_atoms
function syntaxstring(
    lf::LeftmostLinearForm;
    function_notation = false,
    parenthesize_grandchildren = true,
    kwargs...,
)
    if function_notation
        syntaxstring(tree(lf); function_notation = function_notation, kwargs...)
    elseif arity(connective(lf)) == 2
        chs = grandchildren(lf)
        children_ss = map(
            c->syntaxstring(c; parenthesize_grandchildren, kwargs...),
            chs
        )
        if parenthesize_grandchildren
            children_ss = map(ch->"($(ch))", children_ss)
        end
        join(children_ss, " $(syntaxstring(connective(lf); kwargs...)) ")
    end
end

function tree(lf::LeftmostLinearForm)
    c = connective(lf)
    a = arity(c)
    chs = grandchildren(lf)

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
        print(io, " $(nconjuncts(lf)) conjuncts")
        L = literaltype(lf)
        if L <: Literal
            println(io, ":")
        else
            println(io, " and literals of type $(L):")
        end
    elseif lf isa DNF
        print(io, "DNF with")
        print(io, " $(ndisjuncts(lf)) disjuncts")
        L = literaltype(lf)
        if L <: Literal
            println(io, ":")
        else
            println(io, " and literals of type $(L):")
        end
    else
        if lf isa LeftmostConjunctiveForm
            print(io, "LeftmostConjunctiveForm with")
        elseif lf isa LeftmostDisjunctiveForm
            print(io, "LeftmostDisjunctiveForm with")
        else
            print(io, "LeftmostLinearForm with connective $(syntaxstring(connective(lf))) and")
        end
        println(io, " $(ngrandchildren(lf)) $((SS == SyntaxStructure ? "" : "$(SS) "))grandchildren:")
    end
    # println(io, "\t$(join(syntaxstring.(grandchildren(lf)), " $(syntaxstring(connective(lf))) \n\t"))")
    println(io, "\t$(join(syntaxstring.(grandchildren(lf)), "\n\t"))")
end

# TODO fix
Base.promote_rule(::Type{<:LeftmostLinearForm}, ::Type{<:LeftmostLinearForm}) = SyntaxTree
Base.promote_rule(::Type{SS}, ::Type{LF}) where {SS<:SyntaxStructure,LF<:LeftmostLinearForm} = SyntaxTree
Base.promote_rule(::Type{LF}, ::Type{SS}) where {LF<:LeftmostLinearForm,SS<:SyntaxStructure} = SyntaxTree

function Base.in(tok::SyntaxToken, φ::LeftmostLinearForm)::Bool
    return (tok isa Connective && connective(φ) == tok) ||
        any(c->Base.in(tok, c), grandchildren(φ))
end

function Base.in(tok::SyntaxLeaf, φ::LeftmostLinearForm{C,<:SyntaxLeaf})::Bool where {C<:Connective}
    return Base.in(tok, grandchildren(φ))
end


atoms(φ::LeftmostLinearForm) = Iterators.flatten(Iterators.map(atoms, grandchildren(φ)))
leaves(φ::LeftmostLinearForm) = Iterators.flatten(Iterators.map(leaves, grandchildren(φ)))

natoms(φ::LeftmostLinearForm) = sum(natoms, grandchildren(φ))
nleaves(φ::LeftmostLinearForm) = sum(nleaves, grandchildren(φ))

# function tokens(φ::LeftmostLinearForm)
#     # return TODO
# end

function atoms(φ::LeftmostLinearForm{C,<:Atom})::Vector{Atom} where {C<:Connective}
    return grandchildren(φ)
end

# function connectives(φ::LeftmostLinearForm{C,<:Atom})::Bool where {C<:Connective}
#     # return TODO
# end

# function operators(φ::LeftmostLinearForm{C,<:Atom})::Bool where {C<:Connective}
#     # return TODO
# end

function leaves(φ::LeftmostLinearForm{C,<:SyntaxLeaf})::SyntaxLeaf where {C<:Connective}
    return grandchildren(φ)
end

# function ntokens(φ::LeftmostLinearForm{C,<:Atom})::Bool where {C<:Connective}
#     # return TODO
# end

function natoms(φ::LeftmostLinearForm{C,<:Atom})::Integer where {C<:Connective}
    return ngrandchildren(φ)
end

# function nconnectives(φ::LeftmostLinearForm{C,<:Atom})::Bool where {C<:Connective}
#     # return TODO
# end

# function noperators(φ::LeftmostLinearForm{C,<:Atom})::Bool where {C<:Connective}
#     # return TODO
# end

function nleaves(φ::LeftmostLinearForm{C,<:SyntaxLeaf})::Integer where {C<:Connective}
    return ngrandchildren(φ)
end

Base.promote_rule(::Type{LF}, ::Type{SS}) where {LF<:LeftmostLinearForm,SS<:SyntaxTree} = SyntaxTree
Base.promote_rule(::Type{SS}, ::Type{LF}) where {LF<:LeftmostLinearForm,SS<:SyntaxTree} = SyntaxTree

############################################################################################

# TODO actually:
# const CNF{SS<:SyntaxStructure} = Union{LeftmostLinearForm{typeof(∧),LeftmostLinearForm{typeof(∨),SS}},LeftmostLinearForm{typeof(∨),SS}}
# const DNF{SS<:SyntaxStructure} = Union{LeftmostLinearForm{typeof(∨),LeftmostLinearForm{typeof(∧),SS}},LeftmostLinearForm{typeof(∧),SS}}

"""
    LeftmostConjunctiveForm{SS<:SyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}

Specific instantiation of a [`LeftmostLinearForm`](@ref), where [`Connective`](@ref)s are
all [`CONJUNCTION`](@ref)s.

See also [`SyntaxStructure`](@ref), [`Connective`](@ref), [`LeftmostLinearForm`](@ref),
[`CONJUNCTION`](@ref).
"""
const LeftmostConjunctiveForm{SS<:SyntaxStructure} = LeftmostLinearForm{typeof(∧),SS}

function check(
    φ::LeftmostConjunctiveForm,
    args...;
    kwargs...
)
    return all(ch -> check(ch, args...; kwargs...), grandchildren(φ))
end

function check(
    φ::LeftmostConjunctiveForm,
    i::AbstractInterpretation,
    args...;
    kwargs...
)
    return all(ch -> check(ch, i, args...; kwargs...), grandchildren(φ))
end

"""
    LeftmostDisjunctiveForm{SS<:SyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

Specific instantiation of a [`LeftmostLinearForm`](@ref), where [`Connective`](@ref)s are
all [`DISJUNCTION`](@ref)s.

See also [`SyntaxStructure`](@ref), [`Connective`](@ref),
[`LeftmostLinearForm`](@ref), [`DISJUNCTION`](@ref).
"""
const LeftmostDisjunctiveForm{SS<:SyntaxStructure} = LeftmostLinearForm{typeof(∨),SS}

function check(
    φ::LeftmostDisjunctiveForm,
    args...;
    kwargs...
)
    return any(ch -> check(ch, args...; kwargs...), grandchildren(φ))
end

function check(
    φ::LeftmostDisjunctiveForm,
    i::AbstractInterpretation,
    args...;
    kwargs...
)
    return any(ch -> check(ch, i, args...; kwargs...), grandchildren(φ))
end

"""
    CNF{SS<:SyntaxStructure} = LeftmostConjunctiveForm{LeftmostDisjunctiveForm{SS}}

Conjunctive Normal Form of an [`SyntaxStructure`](@ref).

See also [`SyntaxStructure`](@ref), [`LeftmostConjunctiveForm`](@ref),
[`LeftmostDisjunctiveForm`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref).
"""
const CNF{SS<:SyntaxStructure} = LeftmostConjunctiveForm{LeftmostDisjunctiveForm{SS}}

function check(
    φ::CNF,
    args...;
    kwargs...
)
    return all(ch -> any(grandch -> check(grandch, args...; kwargs...), grandchildren(ch)), grandchildren(φ))
end

function check(
    φ::CNF,
    i::AbstractInterpretation,
    args...;
    kwargs...
)
    return all(ch -> any(grandch -> check(grandch, i, args...; kwargs...), grandchildren(ch)), grandchildren(φ))
end

"""
    DNF{SS<:SyntaxStructure} = LeftmostConjunctiveForm{LeftmostConjunctiveForm{SS}}

Disjunctive Normal Form of an [`SyntaxStructure`](@ref).

See also [`SyntaxStructure`](@ref), [`LeftmostConjunctiveForm`](@ref),
[`LeftmostDisjunctiveForm`](@ref), [`CONJUNCTION`](@ref), [`DISJUNCTION`](@ref).
"""
const DNF{SS<:SyntaxStructure} = LeftmostDisjunctiveForm{LeftmostConjunctiveForm{SS}}

function check(
    φ::DNF,
    args...;
    kwargs...
)
    return any(ch -> all(grandch -> check(grandch, args...; kwargs...), grandchildren(ch)), grandchildren(φ))
end

function check(
    φ::DNF,
    i::AbstractInterpretation,
    args...;
    kwargs...
)
    return any(ch -> all(grandch -> check(grandch, i, args...; kwargs...), grandchildren(ch)), grandchildren(φ))
end

# Helpers
function CNF(conjuncts::AbstractVector{<:LeftmostDisjunctiveForm})
    SS = Union{grandchildrentype.(conjuncts)...}
    return CNF{SS}(conjuncts)
end
function DNF(disjuncts::AbstractVector{<:LeftmostConjunctiveForm})
    SS = Union{grandchildrentype.(disjuncts)...}
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


literaltype(::CNF{SS}) where {SS<:SyntaxStructure} = SS
literaltype(::DNF{SS}) where {SS<:SyntaxStructure} = SS

# # TODO maybe not needed?
# Base.promote_rule(::Type{<:LeftmostConjunctiveForm}, ::Type{<:LeftmostConjunctiveForm}) = LeftmostConjunctiveForm
# Base.promote_rule(::Type{<:LeftmostDisjunctiveForm}, ::Type{<:LeftmostDisjunctiveForm}) = LeftmostDisjunctiveForm
# Base.promote_rule(::Type{<:LeftmostConjunctiveForm}, ::Type{<:LeftmostDisjunctiveForm}) = SyntaxTree

conjuncts(φ::LeftmostConjunctiveForm) = grandchildren(φ)
nconjuncts(φ::LeftmostConjunctiveForm) = ngrandchildren(φ)
pushconjunct!(φ::LeftmostLinearForm, el) = Base.push!(grandchildren(φ), el)

disjuncts(φ::LeftmostDisjunctiveForm) = grandchildren(φ)
ndisjuncts(φ::LeftmostDisjunctiveForm) = ngrandchildren(φ)
pushdisjunct(φ::LeftmostDisjunctiveForm, el) = Base.push!(grandchildren(φ), el)

# conjuncts(φ::DNF) = map(d->conjuncts(d), disjuncts(φ))
# nconjuncts(φ::DNF) = map(d->nconjuncts(d), disjuncts(φ))
# disjuncts(φ::CNF) = map(d->disjuncts(d), conjuncts(φ))
# ndisjuncts(φ::CNF) = map(d->ndisjuncts(d), conjuncts(φ))


############################################################################################

"""
    struct Literal{T<:SyntaxLeaf} <: SyntaxStructure
        ispos::Bool
        atom::T
    end

An atom, or its negation.

See also [`CNF`](@ref), [`DNF`](@ref), [`SyntaxStructure`](@ref).
"""
struct Literal{T<:SyntaxLeaf} <: SyntaxStructure
    ispos::Bool
    atom::T

    function Literal{T}(
        ispos::Bool,
        atom::T,
    ) where {T<:SyntaxLeaf}
        new{T}(ispos, atom)
    end

    function Literal(
        ispos::Bool,
        atom::T,
    ) where {T<:SyntaxLeaf}
        Literal{T}(ispos, atom)
    end

    function Literal(φ::SyntaxLeaf, flag = true)
        return Literal(flag, φ)
    end
    function Literal(φ::SyntaxBranch, flag = true)
        ch = first(children(φ))
        @assert (token(φ) == ¬) "Cannot " *
            "construct Literal with formula of type $(typeof(ch))): $(syntaxstring(ch))."
        return Literal(ch, !flag)
    end
end

ispos(l::Literal) = l.ispos
atom(l::Literal) = l.atom

atomstype(::Literal{T}) where {T} = T

tree(l::Literal) = ispos(l) ? l.atom : ¬(l.atom)

hasdual(l::Literal) = true
dual(l::Literal) = Literal(!ispos(l), atom(l))

function Base.show(io::IO, l::Literal)
    println(io,
        "Literal{$(atomstype(l))}: " * (ispos(l) ? "" : "¬") * syntaxstring(atom(l))
    )
end
