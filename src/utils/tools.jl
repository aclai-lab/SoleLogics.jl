
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
    transformnode::Union{Nothing,Function} = nothing,
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
        flip_atom = nothing,
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
- `flip_atom::Union{Nothing,Callable}`: when a callable is provided, it is used
    for deciding when to flip atoms.

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
    t::LLF;
    kwargs...
) where {LLF<:Union{LeftmostConjunctiveForm,LeftmostDisjunctiveForm}}
    ch = deepcopy(children(t))
    unique!(ch)
    ch = normalize.(ch; kwargs...)
    unique!(ch)
    if connective(t) == (∧)
        filter!(c->c != ⊤, ch)
    elseif connective(t) == (∨)
        filter!(c->c != ⊥, ch)
    end
    return LeftmostLinearForm(connective(t), ch)
end

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
    flip_atom = nothing,
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
        # if isnothing(simplify_constants)         simplify_constants = false end
        if isnothing(simplify_constants)         simplify_constants = true end
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
        rotate_commutatives = rotate_commutatives,
        flip_atom = flip_atom,
    )
    # @show allow_atom_flipping, forced_negation_removal
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
        if tok isa AbstractAtom && !isnothing(flip_atom)
            # @show (1, newt)
            flip_atom(tok) ? ¬dual(tok) : tok
        elseif (tok == ¬) && arity(tok) == 1
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
            elseif reduce_negations && chtok isa AbstractAtom
                # @show chtok
                # @show hasdual(chtok)
                if allow_atom_flipping && hasdual(chtok)
                    should_flip = isnothing(flip_atom) || (!isnothing(flip_atom) && flip_atom(chtok))
                    # @show should_flip
                    should_flip ? dual(chtok) : ¬chtok
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
    # @show (2, newt)

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

    # Remove boxes
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
    # @show (3, newt)

    function _isless(st1::SyntaxTree, st2::SyntaxTree)
        isless(Base.hash(st1), Base.hash(st2))
    end

    # Rotate commutatives
    if rotate_commutatives
        newt = begin
            tok, chs = token(newt), children(newt)
            if tok isa Connective && iscommutative(tok) && arity(tok) > 1
                chs = children(LeftmostLinearForm(newt, tok))
                # @show chs
                chs = Vector(sort(collect(_normalize.(chs)), lt=_isless))
                # @show chs
                if tok in [∧,∨] # TODO create trait for this behavior: p ∧ p ∧ p ∧ q   -> p ∧ q
                    chs = unique(chs)
                end
                tree(LeftmostLinearForm(tok, chs))
            else
                SyntaxTree(tok, chs)
            end
        end
    end
    # @show (4, newt)
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

################################################################################
################################################################################
################################################################################

using SoleLogics
using SoleLogics: AbstractSyntaxBranch, token

"""
    normalize_formula(φ::Formula, mode::Symbol, literaltype = Literal; kwargs...)

Returns a formula in a normal form (CNF or DNF) with literals of type `literaltype`.
`mode` can be `:cnf` or `:dnf`. Uses [`normalize`](@ref) to ensure the formula
is in Negation Normal Form (NNF) first.
"""
function normalize_formula(φ::Formula, mode::Symbol, literaltype = Literal; kwargs...)
    if mode == :cnf
        return _convert_to_normal_form(normalize(φ; profile = :nnf, kwargs...), ∧, ∨, literaltype)
    elseif mode == :dnf
        return _convert_to_normal_form(normalize(φ; profile = :nnf, kwargs...), ∨, ∧, literaltype)
    else
        throw(ArgumentError("Unsupported mode: $mode. Use :cnf or :dnf"))
    end
end

"""
    cnf(φ::Formula, literaltype = Literal; kwargs...)

Return a formula into Conjunctive Normal Form with literals of type
`literaltype` (`CNF{literaltype}`).
Any additional `kwarg` is internally passed to [`normalize`](@ref).
"""
function cnf(φ::Formula, literaltype = Literal; kwargs...)
    return normalize_formula(φ, :cnf, literaltype; kwargs...)
end

"""
    dnf(φ::Formula, literaltype = Literal; kwargs...)

Return a formula into Disjunctive Normal Form with literals of type
`literaltype` (`CNF{literaltype}`).
Any additional `kwarg` is internally passed to [`normalize`](@ref).
"""
function dnf(φ::Formula, literaltype = Literal; kwargs...)
    return normalize_formula(φ, :dnf, literaltype; kwargs...)
end

"""
    _convert_to_normal_form(φ::Formula, primary::Connective, secondary::Connective, literaltype::Type)

General helper for converting formulas into CNF or DNF.
- `primary`: The main connective of the normal form (`∧` for CNF, `∨` for DNF).
- `secondary`: The secondary connective used for distributing terms.
"""
function _convert_to_normal_form(φ::Formula, primary::Connective, secondary::Connective, literaltype::Type)
    ret = if φ isa SyntaxLeaf # TODO first find literals, then use `if φ isa literaltype`
        _literal_to_normal_form(φ, primary, secondary, literaltype)
    elseif φ isa Union{LeftmostConjunctiveForm,LeftmostDisjunctiveForm}
        _convert_to_normal_form(tree(φ), primary, secondary, literaltype) # TODO: this is slow.
    elseif φ isa AbstractSyntaxBranch
        tok = token(φ)
        if tok == primary
            @assert isbinary(tok)
            # Combine the primary operation directly
            _combine_normal_form(
                _convert_to_normal_form(first(children(φ)), primary, secondary, literaltype),
                _convert_to_normal_form(last(children(φ)), primary, secondary, literaltype),
                primary
            )
        elseif tok == secondary
            @assert isbinary(secondary)
            # Distribute the secondary operation over the primary
            ONE = _extract_normal_form_clauses(
                    _convert_to_normal_form(first(children(φ)), primary, secondary, literaltype), primary
                )
            TWO = _extract_normal_form_clauses(
                    _convert_to_normal_form(last(children(φ)), primary, secondary, literaltype), primary
                )
            # @show first(children(φ))
            # @show ONE
            # @show last(children(φ))
            # @show TWO
            distributed_terms = [
                _combine_normal_form(c1, c2, secondary)
                for c1 in ONE
                for c2 in TWO
            ]
            # @show distributed_terms
            _combine_clauses(distributed_terms, primary)
        elseif tok == (¬)
            # @show φ
            # Negate literals and convert to desired form
            _literal_to_normal_form(φ, primary, secondary, literaltype)
        else
            throw(ArgumentError("Unexpected token in formula: $tok"))
        end
    else
        throw(ArgumentError("Unsupported formula type: $(typeof(φ))"))
    end
    # @show φ
    # @show ret
    return ret
end

"""
    _literal_to_normal_form(φ::Formula, primary::Connective, secondary::Connective, literaltype::Type)

Converts a literal into the specified normal form's atomic unit.
"""
function _literal_to_normal_form(φ::Formula, primary::typeof(∧), secondary::typeof(∨), literaltype::Type)
    # @show φ, literaltype
    literal = φ isa literaltype ? φ : literaltype(φ)
    return LeftmostConjunctiveForm([LeftmostDisjunctiveForm{literaltype}([literal])])
end

function _literal_to_normal_form(φ::Formula, primary::typeof(∨), secondary::typeof(∧), literaltype::Type)
    # @show φ, literaltype
    literal = φ isa literaltype ? φ : literaltype(φ)
    return LeftmostDisjunctiveForm([LeftmostConjunctiveForm{literaltype}([literal])])
end

# """
#     _combine_normal_form!(left, right, c)

# Combines two sub-formulas using the given connective.
# """
function _combine_normal_form!(left::LeftmostLinearForm{C}, right::LeftmostLinearForm{C}, c::C) where {C<:Connective}
    Base.append!(SoleLogics.grandchildren(left), SoleLogics.grandchildren(right))
    return left
end

"""
    _combine_normal_form(left, right, c)

Combines two sub-formulas using the given connective.
"""
function _combine_normal_form(left::LeftmostLinearForm{C}, right::LeftmostLinearForm{C}, c::C) where {C<:Connective}
    if c == ∧
        return left ∧ right
    elseif c == ∨
        return left ∨ right
    else
        throw(ArgumentError("Unsupported connective: $op"))
    end
end

"""
    _extract_normal_form_clauses(φ::Formula, primary::Connective)

Extracts the conjuncts or disjuncts from the given formula.
"""
function _extract_normal_form_clauses(φ::Formula, primary::Connective)
    if token(φ) == primary
        return SoleLogics.grandchildren(φ)
    else
        return [φ]
    end
end

"""
    _combine_clauses(clauses::Vector, c::Connective)

Combines a set of clauses using the specified connective.
"""
function _combine_clauses(clauses::Vector, c::Connective)
    if c == (∧)
        return LeftmostConjunctiveForm(clauses)
    elseif c == (∨)
        return LeftmostDisjunctiveForm(clauses)
    else
        throw(ArgumentError("Unsupported connective: $c"))
    end
end