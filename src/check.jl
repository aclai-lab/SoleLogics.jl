
"""
    subformulas(f::AbstractFormula; sorted=true)

Returns all sub-formulas (sorted by size when `sorted=true`)
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
[`SyntaxTree`](@ref), [`Formula`](@ref), [`AbstractFormula`](@ref).
"""
subformulas(f::Formula; kwargs...) = f.(subformulas(tree(f); kwargs...))
function subformulas(t::SyntaxTree; sorted=true)
    # function _subformulas(_t::SyntaxTree)
    #     SyntaxTree{tokenstype(t)}[
    #         (map(SyntaxTree{tokenstype(t)}, Iterators.flatten(subformulas.(children(_t)))))...,
    #         SyntaxTree{tokenstype(t)}(_t)
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
"""
    normalize(
        f::AbstractFormula;
        remove_boxes = true,
        reduce_negations = true,
        allow_inverse_propositions = true,
    )

Returns a modified version of a given formula, that has the same semantics
but different syntax. This is useful when dealing with the truth of many
(possibly similar) formulas; for example, when performing
[model checking](https://en.m.wikipedia.org/wiki/Model_checking).
BEWARE: it currently assumes the underlying algebra is Boolean!

# Arguments
- `f::AbstractFormula`: when set to `true`,
    the formula;
- `remove_boxes::Bool`: converts all uni-modal and multi-modal box operators by using the 
    equivalence ◊φ ≡ ¬□¬φ. Note: this assumes an underlying Boolean algebra.
- `reduce_negations::Bool`: when set to `true`,
    attempts at reducing the number of negations by appling
    some transformation rules
    (e.g., [De Morgan's laws](https://en.m.wikipedia.org/wiki/De_Morgan%27s_laws)).
    Note: this assumes an underlying Boolean algebra.
- `allow_inverse_propositions::Bool`: when set to `true`,
    together with `reduce_negations=true`, this may cause the negation of a proposition
    to be replaced with the proposition with its [`negation`](@ref).

# Examples
```julia-repl
julia> f = parseformula("¬□¬((p∧¬q)→r)"; operators = SoleLogics.BASE_MODAL_OPERATORS);

julia> syntaxstring(f)
"(¬(□(¬(p ∧ (¬(q)))))) → r"

julia> syntaxstring(SoleLogics.normalize(f; allow_inverse_propositions = false))
"(◊(p ∧ (¬(q)))) → r"
```

See also
[`SyntaxTree`](@ref), [`Formula`](@ref), [`AbstractFormula`](@ref).
"""
normalize(f::Formula; kwargs...) = f(normalize(tree(f); kwargs...))
function normalize(
    t::SyntaxTree;
    remove_boxes = true,
    reduce_negations = true,
    allow_inverse_propositions = true,
)
    # TODO we're currently assuming Boolean algebra!!! Very wrong.

    _normalize = x->normalize(x;
        remove_boxes = remove_boxes,
        reduce_negations = reduce_negations,
        allow_inverse_propositions = allow_inverse_propositions,
    )

    # TODO introduce rotate_commutatives parameter and use rotate_commutatives && iscommutative for normalizing the structure of commutative operators.
    tok, ch = token(t), children(t)
    if remove_boxes && tok isa BoxRelationalOperator && arity(tok) == 1
        # remove_boxes -> substitute every [X]φ with ¬⟨X⟩¬φ
        childtok = ch[1]
        dual_op = dual(tok)
        ¬(dual_op(_normalize(¬childtok)))
        # TODO remove
        # if relation(tok) == globalrel
        #     # Special case: [G]φ -> ⟨G⟩φ
        #     dual_op(_normalize(childtok))
        # else
        #     ¬(dual_op(_normalize(¬childtok)))
        # end
    elseif reduce_negations && (tok == ¬) && arity(tok) == 1
        # reduce_negations
        childtok = ch[1]
        grandchildren = children(childtok)
        if (token(childtok) == ∨) && arity(token(childtok)) == 2
            ∧(_normalize(¬(grandchildren[1])), _normalize(¬(grandchildren[2])))
            # TODO use implication, maybe it's more interpretable?
        elseif (token(childtok) == ∧) && arity(token(childtok)) == 2
            ∨(_normalize(¬(grandchildren[1])), _normalize(¬(grandchildren[2])))
        elseif (token(childtok) == →) && arity(token(childtok)) == 2
            # _normalize(∨(¬(grandchildren[1]), grandchildren[2]))
            ∨(_normalize(grandchildren[1]), _normalize(¬(grandchildren[2])))
        elseif (token(childtok) == ¬) && arity(token(childtok)) == 1
            _normalize(grandchildren[1])
        elseif token(childtok) isa Proposition
            if allow_inverse_propositions
                SyntaxTree(negation(token(childtok)))
            else
                ¬(_normalize(childtok))
            end
        elseif token(childtok) isa SoleLogics.AbstractRelationalOperator && arity(token(childtok)) == 1
            dual_op = dual(token(childtok))
            if remove_boxes && dual_op isa SoleLogics.BoxRelationalOperator
                ¬(_normalize(childtok))
            else
                dual_op(_normalize(¬(grandchildren[1])))
            end
        elseif ismodal(token(childtok)) && arity(token(childtok)) == 1
            dual_op = dual(token(childtok))
            if remove_boxes && SoleLogics.isbox(dual_op)
                ¬(_normalize(childtok))
            else
                dual_op(_normalize(¬(grandchildren[1])))
            end
        else
            error("Unknown token(childtok) when removing negations: $(token(childtok)) (type = $(typeof(token(childtok))))")
        end
    else
        SyntaxTree(tok, _normalize.(ch))
    end
end

"""
    isglobal(f::AbstractFormula)::Bool

Returns `true` if the formula is global, that is, if it can be inferred from its syntactic
structure that, given any frame-based model, the truth value of the formula is the same
on every world.

# Examples
```julia-repl
julia> f = parseformula("⟨G⟩p → [G]q"); # TODO Mauro fix this, and also allow propositions that are Regex strings \\w?
julia> f = →(BoxRelationalOperator(globalrel)(Proposition("p")), DiamondRelationalOperator(globalrel)(Proposition("q"))); # Temporary fix

julia> syntaxstring(f)
"([G](p)) → (⟨G⟩(q))"

julia> SoleLogics.isglobal(f)
true
```

See also
[`SyntaxTree`](@ref), [`Formula`](@ref), [`AbstractFormula`](@ref).
"""
isglobal(f::AbstractFormula)::Bool = isglobal(tree(f))
isglobal(t::SyntaxTree)::Bool =
    # (println(token(t)); println(children(t)); true) && 
    (token(t) isa SoleLogics.AbstractRelationalOperator && relation(token(t)) == globalrel) ||
    # (token(t) in [◊,□]) ||
    (token(t) isa AbstractOperator && all(c->isglobal(c), children(t)))

"""
    function collateworlds(
        fr::AbstractFrame{W,Bool},
        op::AbstractOperator,
        t::NTuple{N,WorldSetType},
    )::AbstractWorldSet{<:W} where {N,W<:AbstractWorld,WorldSetType<:AbstractWorldSet}

On a crisp frame (`truthtype == Bool`),
returns the set of worlds where a composed formula op(φ1, ..., φN) is true, given the `N`
sets of worlds where the each immediate sub-formula is true.

See also [`check`](@ref), [`iscrisp`](@ref),
[`AbstractOperator`](@ref), [`AbstractFrame`](@ref).
"""
function collateworlds(
    fr::AbstractFrame{W,Bool},
    op::AbstractOperator,
    t::NTuple{N,<:AbstractWorldSet},
)::AbstractWorldSet{<:W} where {N,W<:AbstractWorld}
    if arity(op) != length(t)
        return error("Cannot collate $(length(t)) truth values for" *
                     " operator $(typeof(op)) with arity $(arity(op))).")
    else
        return error("Please, provide method collateworlds(::$(typeof(fr))," *
                     " ::$(typeof(op)), ::NTuple{$(arity(op)), $(AbstractWorldSet{W})}.")
    end
end

# I know, these exceed 92 characters. But they look nicer like this!! :D
collateworlds(fr::AbstractFrame{W,Bool}, ::typeof(⊤), ::NTuple{0,<:AbstractWorldSet}) where {W<:AbstractWorld} = allworlds(fr)
collateworlds(::AbstractFrame{W,Bool}, ::typeof(⊥), ::NTuple{0,<:AbstractWorldSet}) where {W<:AbstractWorld} = W[]

collateworlds(fr::AbstractFrame{W,Bool}, ::typeof(¬), (ws,)::NTuple{1,<:AbstractWorldSet}) where {W<:AbstractWorld} = setdiff(allworlds(fr), ws)
collateworlds(::AbstractFrame{W,Bool}, ::typeof(∧), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = intersect(ws1, ws2)
collateworlds(::AbstractFrame{W,Bool}, ::typeof(∨), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = union(ws1, ws2)
collateworlds(fr::AbstractFrame{W,Bool}, ::typeof(→), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = union(setdiff(allworlds(fr), ws1), ws2)

function collateworlds(
    fr::AbstractMultiModalFrame{W,Bool},
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

function collateworlds(
    fr::AbstractMultiModalFrame{W,Bool},
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

