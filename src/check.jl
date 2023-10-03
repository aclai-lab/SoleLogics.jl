
"""
    subformulas(f::AbstractFormula; sorted=true)

Return all sub-formulas (sorted by size when `sorted=true`)
of a given formula.

# Examples
```julia-repl
julia> syntaxstring.(SoleLogics.subformulas(parsebaseformula("◊((p∧q)→r)")))
6-element Vector{String}:
 "p"
 "q"
 "r"
 "p ∧ q"
 "◊(p ∧ q)"
 "(◊(p ∧ q)) → r"
```

See also
[`SyntaxTree`](@ref)), [`AbstractFormula`](@ref).
"""
subformulas(f::AbstractSyntaxStructure; kwargs...) = subformulas(tree(f); kwargs...)
subformulas(f::Formula; kwargs...) = f.(subformulas(tree(f); kwargs...))
function subformulas(t::SyntaxTree; sorted=true)
    # function _subformulas(_t::SyntaxTree)
    #     SyntaxTree[
    #         (map(SyntaxTree, Iterators.flatten(subformulas.(children(_t)))))...,
    #         SyntaxTree(_t)
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
        f::AbstractFormula;
        remove_boxes = true,
        reduce_negations = true,
        allow_atom_flipping = true,
    )

Return a modified version of a given formula, that has the same semantics
but different syntax. This is useful when dealing with the truth of many
(possibly similar) formulas; for example, when performing
[model checking](https://en.wikipedia.org/wiki/Model_checking).
BEWARE: it currently assumes the underlying algebra is Boolean!

# Arguments
- `f::AbstractFormula`: when set to `true`,
    the formula;
- `remove_boxes::Bool`: converts all uni-modal and multi-modal box operators by using the
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
julia> f = parsebaseformula("□¬((p∧¬q)→r)∧⊤");

julia> syntaxstring(f)
"□¬((p ∧ ¬q) → r) ∧ ⊤"

julia> syntaxstring(SoleLogics.normalize(f; profile = :modelchecking, allow_atom_flipping = false))
"¬◊(q ∨ ¬p ∨ r)"

julia> syntaxstring(SoleLogics.normalize(f; profile = :readability, allow_atom_flipping = false))
"□(¬r ∧ p ∧ ¬q)"
```

See also
[`SyntaxTree`](@ref)), [`AbstractFormula`](@ref).
"""
normalize(f::AbstractSyntaxStructure; kwargs...) = normalize(tree(f); kwargs...)
normalize(f::Formula; kwargs...) = f(normalize(tree(f); kwargs...))
function normalize(
    t::SyntaxTree;
    profile = :readability,
    remove_boxes = nothing,
    reduce_negations = nothing,
    simplify_constants = nothing,
    allow_atom_flipping = nothing,
    forced_negation_removal = nothing,
    remove_identities = nothing,
    rotate_commutatives = nothing
)
    if profile == :readability
        if isnothing(remove_boxes)               remove_boxes = false end
        if isnothing(reduce_negations)           reduce_negations = true end
        if isnothing(simplify_constants)         simplify_constants = true end
        if isnothing(allow_atom_flipping) allow_atom_flipping = false end
        if isnothing(remove_identities)          remove_identities = false end
        if isnothing(rotate_commutatives)        rotate_commutatives = true end
        # TODO leave \to's instead of replacing them with \lor's...
    elseif profile == :modelchecking
        if isnothing(remove_boxes)               remove_boxes = true end
        if isnothing(reduce_negations)           reduce_negations = true end
        if isnothing(simplify_constants)         simplify_constants = true end
        if isnothing(allow_atom_flipping) allow_atom_flipping = false end
        if isnothing(remove_identities)          remove_identities = true end
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

    # TODO we're currently assuming Boolean algebra!!! Very wrong.

    _normalize = t->normalize(t;
        profile = profile,
        remove_boxes = remove_boxes,
        reduce_negations = reduce_negations,
        simplify_constants = simplify_constants,
        allow_atom_flipping = allow_atom_flipping,
        forced_negation_removal = forced_negation_removal,
        rotate_commutatives = rotate_commutatives
    )

    newt = t

    # Remove modal operators based on the identity relation
    newt = begin
        tok, ch = token(newt), children(newt)
        if remove_identities && tok isa AbstractRelationalOperator &&
            relation(tok) == identityrel && arity(tok) == 1
            first(ch)
        else
            newt
        end
    end

    # Simplify
    newt = begin
        tok, ch = token(newt), children(newt)
        if (tok == ¬) && arity(tok) == 1
            child = ch[1]
            chtok, grandchildren = token(child), children(child)
            if reduce_negations && (chtok == ¬) && arity(chtok) == 1
                _normalize(grandchildren[1])
            elseif reduce_negations && (chtok == ∨) && arity(chtok) == 2
                ∧(_normalize(¬(grandchildren[1])), _normalize(¬(grandchildren[2])))
                # TODO use implication, maybe it's more interpretable?
            elseif reduce_negations && (chtok == ∧) && arity(chtok) == 2
                ∨(_normalize(¬(grandchildren[1])), _normalize(¬(grandchildren[2])))
            elseif reduce_negations && (chtok == →) && arity(chtok) == 2
                # _normalize(∨(¬(grandchildren[1]), grandchildren[2]))
                ∧(_normalize(grandchildren[1]), _normalize(¬(grandchildren[2])))
            elseif reduce_negations && chtok isa Atom
                if allow_atom_flipping && hasdual(chtok)
                    SyntaxTree(dual(chtok))
                else
                    ¬(_normalize(child))
                end
            # elseif reduce_negations && chtok isa SoleLogics.AbstractRelationalOperator && arity(chtok) == 1
            #     dual_op = dual(chtok)
            #     if remove_boxes && dual_op isa SoleLogics.BoxRelationalOperator
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
                SyntaxTree(⊥)
            elseif (reduce_negations || simplify_constants) && chtok == ⊥ && arity(chtok) == 1
                SyntaxTree(⊤)
            elseif !forced_negation_removal
                SyntaxTree(tok, _normalize.(ch))
            else
                error("Unknown chtok when removing negations: $(chtok) (type = $(typeof(chtok)))")
            end
        else
            SyntaxTree(tok, _normalize.(ch))
        end
    end

    # Simplify constants
    newt = begin
        tok, ch = token(newt), children(newt)
        if simplify_constants && tok isa Operator
            if (tok == ∨) && arity(tok) == 2
                if     token(ch[1]) == ⊥  ch[2]          # ⊥ ∨ φ ≡ φ
                elseif token(ch[2]) == ⊥  ch[1]          # φ ∨ ⊥ ≡ φ
                elseif token(ch[1]) == ⊤  SyntaxTree(⊤)  # ⊤ ∨ φ ≡ ⊤
                elseif token(ch[2]) == ⊤  SyntaxTree(⊤)  # φ ∨ ⊤ ≡ ⊤
                else                      newt
                end
            elseif (tok == ∧) && arity(tok) == 2
                if     token(ch[1]) == ⊥  SyntaxTree(⊥)  # ⊥ ∧ φ ≡ ⊥
                elseif token(ch[2]) == ⊥  SyntaxTree(⊥)  # φ ∧ ⊥ ≡ ⊥
                elseif token(ch[1]) == ⊤  ch[2]          # ⊤ ∧ φ ≡ φ
                elseif token(ch[2]) == ⊤  ch[1]          # φ ∧ ⊤ ≡ φ
                else                      newt
                end
            elseif (tok == →) && arity(tok) == 2
                if     token(ch[1]) == ⊥  SyntaxTree(⊤)       # ⊥ → φ ≡ ⊤
                elseif token(ch[2]) == ⊥  _normalize(¬ch[1])  # φ → ⊥ ≡ ¬φ
                elseif token(ch[1]) == ⊤  ch[2]               # ⊤ → φ ≡ φ
                elseif token(ch[2]) == ⊤  SyntaxTree(⊤)       # φ → ⊤ ≡ ⊤
                else                      SyntaxTree(∨, _normalize(¬ch[1]), ch[2])
                end
            elseif (tok == ¬) && arity(tok) == 1
                if     token(ch[1]) == ⊤  SyntaxTree(⊥)
                elseif token(ch[1]) == ⊥  SyntaxTree(⊤)
                else                      newt
                end
            elseif SoleLogics.isbox(tok) && arity(tok) == 1
                if     token(ch[1]) == ⊤  SyntaxTree(⊤)
                else                      newt
                end
            elseif SoleLogics.isdiamond(tok) && arity(tok) == 1
                if     token(ch[1]) == ⊥  SyntaxTree(⊥)
                else                      newt
                end
            else
                newt
            end
        else
            newt
        end
    end

    newt = begin
        tok, ch = token(newt), children(newt)
        if remove_boxes && tok isa Operator && SoleLogics.isbox(tok) && arity(tok) == 1
            # remove_boxes -> substitute every [X]φ with ¬⟨X⟩¬φ
            child = ch[1]
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
            tok, ch = token(newt), children(newt)
            if tok isa Connective && iscommutative(tok) && arity(tok) > 1
                ch = children(LeftmostLinearForm(newt, tok))
                ch = Vector(sort(collect(_normalize.(ch)), lt=_isless))
                if tok in [∧,∨] # TODO create trait for this behavior: p ∧ p ∧ p ∧ q   -> p ∧ q
                    ch = unique(ch)
                end
                tree(LeftmostLinearForm(tok, ch))
            else
                SyntaxTree(tok, ch)
            end
        end
    end

    return newt
end

"""
    isgrounded(f::AbstractFormula)::Bool

Return `true` if the formula is grounded, that is, if it can be inferred from its syntactic
structure that, given any frame-based model, the truth value of the formula is the same
on every world.

# Examples
```julia-repl
julia> f = parsebaseformula("⟨G⟩p → [G]q");

julia> syntaxstring(f)
"(⟨G⟩p) → ([G]q)"

julia> SoleLogics.isgrounded(f)
true
```

See also
[`isgrounding`](@ref)), [`SyntaxTree`](@ref)), [`AbstractFormula`](@ref).
"""
isgrounded(f::AbstractFormula)::Bool = isgrounded(tree(f))
isgrounded(t::SyntaxTree)::Bool =
    # (println(token(t)); println(children(t)); true) &&
    (token(t) isa SoleLogics.AbstractRelationalOperator && isgrounding(relation(token(t)))) ||
    # (token(t) in [◊,□]) ||
    (token(t) isa Operator && all(c->isgrounded(c), children(t)))

"""
    function collateworlds(
        fr::AbstractFrame{W},
        op::Operator,
        t::NTuple{N,WorldSetType},
    )::AbstractWorldSet{<:W} where {N,W<:AbstractWorld,WorldSetType<:AbstractWorldSet}

For a given crisp frame (`truthtype == Bool`),
return the set of worlds where a composed formula op(φ1, ..., φN) is true, given the `N`
sets of worlds where the each immediate sub-formula is true.

See also [`check`](@ref), [`iscrisp`](@ref),
[`Operator`](@ref), [`AbstractFrame`](@ref).
"""
function collateworlds(
    fr::AbstractFrame{W},
    op::Operator,
    t::NTuple{N,<:AbstractWorldSet},
)::AbstractWorldSet{<:W} where {N,W<:AbstractWorld}
    if arity(op) != length(t)
        return error("Cannot collate $(length(t)) truth values for " *
                     "operator $(typeof(op)) with arity $(arity(op))).")
    else
        return error("Please, provide method collateworlds(::$(typeof(fr)), " *
                     "::$(typeof(op)), ::NTuple{$(arity(op)), $(AbstractWorldSet{W})}).")
    end
end

# I know, these exceed 92 characters. But they look nicer like this!! :D
collateworlds(fr::AbstractFrame{W}, ::typeof(⊤), ::NTuple{0,<:AbstractWorldSet}) where {W<:AbstractWorld} = allworlds(fr)
collateworlds(::AbstractFrame{W}, ::typeof(⊥), ::NTuple{0,<:AbstractWorldSet}) where {W<:AbstractWorld} = W[]

collateworlds(fr::AbstractFrame{W}, ::typeof(¬), (ws,)::NTuple{1,<:AbstractWorldSet}) where {W<:AbstractWorld} = setdiff(allworlds(fr), ws)
collateworlds(::AbstractFrame{W}, ::typeof(∧), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = intersect(ws1, ws2)
collateworlds(::AbstractFrame{W}, ::typeof(∨), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = union(ws1, ws2)
collateworlds(fr::AbstractFrame{W}, ::typeof(→), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = union(setdiff(allworlds(fr), ws1), ws2)

function collateworlds(
    fr::AbstractFrame{W},
    op::typeof(◊),
    (ws,)::NTuple{1,<:AbstractWorldSet},
) where {W<:AbstractWorld}
    filter(w1->intersects(ws, accessibles(fr, w1)), collect(allworlds(fr)))
end

function collateworlds(
    fr::AbstractFrame{W},
    op::typeof(□),
    (ws,)::NTuple{1,<:AbstractWorldSet},
) where {W<:AbstractWorld}
    filter(w1->issubset(accessibles(fr, w1), ws), collect(allworlds(fr)))
end

# TODO: use AbstractMultiModalFrame
function collateworlds(
    fr::AbstractFrame{W},
    op::DiamondRelationalOperator,
    (ws,)::NTuple{1,<:AbstractWorldSet},
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
    op::BoxRelationalOperator,
    (ws,)::NTuple{1,<:AbstractWorldSet},
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
            negboxws = union(W[], [accessibles(fr, w, converse(r)) for w in negws]...)
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
